use super::super::{
    gc::Rt,
    object::{Gc, GcObj, List, Object},
};
use super::Cons;
use anyhow::{anyhow, Result};
use streaming_iterator::StreamingIterator;

#[derive(Clone)]
pub(crate) struct ElemIter<'ob> {
    cons: Option<&'ob Cons>,
}

#[allow(clippy::multiple_inherent_impl)]
impl Cons {
    pub(crate) fn elements(&self) -> ElemIter {
        ElemIter { cons: Some(self) }
    }

    pub(crate) fn conses(&self) -> ConsIter {
        ConsIter { list: List::Cons(self) }
    }
}

impl<'ob> Gc<List<'ob>> {
    pub(crate) fn elements(self) -> ElemIter<'ob> {
        match self.untag() {
            List::Nil => ElemIter { cons: None },
            List::Cons(cons) => ElemIter { cons: Some(cons) },
        }
    }

    pub(crate) fn conses(self) -> ConsIter<'ob> {
        ConsIter { list: self.untag() }
    }
}

impl<'ob> Iterator for ElemIter<'ob> {
    type Item = Result<GcObj<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cons {
            Some(cons) => {
                self.cons = match cons.cdr().untag() {
                    Object::Cons(next) => Some(next),
                    Object::NIL => None,
                    _ => return Some(Err(anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons.car()))
            }
            None => None,
        }
    }
}

impl<'ob> GcObj<'ob> {
    pub(crate) fn as_list(self) -> Result<ElemIter<'ob>> {
        let list: Gc<List> = self.try_into()?;
        match list.untag() {
            List::Cons(cons) => Ok(ElemIter { cons: Some(cons) }),
            List::Nil => Ok(ElemIter { cons: None }),
        }
    }
}

impl<'ob> ElemIter<'ob> {
    pub(crate) fn len(&self) -> usize {
        self.clone().count()
    }
}

#[derive(Clone)]
pub(crate) struct ConsIter<'ob> {
    list: List<'ob>,
}

impl<'ob> Iterator for ConsIter<'ob> {
    type Item = Result<&'ob Cons>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.list {
            List::Nil => None,
            List::Cons(cons) => {
                self.list = match cons.cdr().untag() {
                    Object::Cons(next) => List::Cons(next),
                    Object::NIL => List::Nil,
                    _ => return Some(Err(anyhow::anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons))
            }
        }
    }
}

pub(crate) struct ElemStreamIter<'rt> {
    elem: Option<&'rt mut Rt<GcObj<'static>>>,
    cons: Option<&'rt mut Rt<&'static Cons>>,
}

impl<'rt> ElemStreamIter<'rt> {
    pub(crate) fn new(
        elem: Option<&'rt mut Rt<GcObj<'static>>>,
        cons: Option<&'rt mut Rt<&'static Cons>>,
    ) -> Self {
        Self { elem, cons }
    }
}

impl<'rt> StreamingIterator for ElemStreamIter<'rt> {
    type Item = Rt<GcObj<'static>>;

    fn advance(&mut self) {
        if let Some(cons) = &mut self.cons {
            let elem = self.elem.as_mut().expect("Element should never be None while Cons is Some");
            let car = unsafe { cons.bind_unchecked().car() };
            elem.set(car);
            match unsafe { cons.bind_unchecked().cdr().untag() } {
                Object::Cons(next) => {
                    let x = unsafe { std::mem::transmute::<&Cons, &Cons>(next) };
                    cons.set(x);
                }
                _ => {
                    self.cons = None;
                }
            }
        } else {
            self.elem = None;
        }
    }

    fn get(&self) -> Option<&Self::Item> {
        self.elem.as_deref()
    }
}

impl<'rt> ElemStreamIter<'rt> {
    pub(crate) fn is_empty(&self) -> bool {
        self.cons.is_none()
    }
}

#[macro_export]
macro_rules! rooted_iter {
    ($ident:ident, $value:expr, $cx:ident) => {
        // Create roots, but don't initialize them
        let mut elem;
        let mut cons;
        let mut root_elem;
        let mut root_cons;
        // use match to ensure that $value is not evaled inside the unsafe block
        let list: $crate::core::object::Gc<$crate::core::object::List> = match $value {
            // Convert the value into a list
            value => unsafe { $crate::core::gc::IntoRoot::into_root(value).try_into()? },
        };
        #[allow(unused_qualifications, unused_mut)]
        let mut $ident = if let $crate::core::object::List::Cons(head) = list.untag() {
            use $crate::core::{cons, gc, object};
            // If the list is not empty, then initialize the roots and put them
            // in the stack space reserved
            unsafe {
                elem = object::nil();
                cons = object::WithLifetime::with_lifetime(head);
                root_elem = gc::__StackRoot::new(&mut elem, $cx.get_root_set());
                root_cons = gc::__StackRoot::new(&mut cons, $cx.get_root_set());
                cons::ElemStreamIter::new(Some(root_elem.as_mut()), Some(root_cons.as_mut()))
            }
        } else {
            $crate::core::cons::ElemStreamIter::new(None, None)
        };
    };
}

#[cfg(test)]
mod test {
    use super::super::super::gc::{Context, RootSet};
    use crate::list;

    use super::*;

    #[test]
    fn elem_iter() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list![1, 2, 3, 4; cx];
        let iter = cons.as_list().unwrap();
        let vec: Result<Vec<_>> = iter.collect();
        assert_eq!(vec.unwrap(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn cons_iter() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list![1, 2, 3, 4; cx];
        let list: Gc<List> = cons.try_into().unwrap();
        let iter = list.conses();
        let expects = vec![1, 2, 3, 4];
        for (act, expect) in iter.zip(expects) {
            let actual = act.unwrap().car();
            assert_eq!(actual, expect);
        }
    }

    #[test]
    fn stream_iter() {
        let func = || -> Result<()> {
            let roots = &RootSet::default();
            let cx = &Context::new(roots);
            let cons = list![1, 2, 3, 4; cx];
            rooted_iter!(iter, cons, cx);
            for expect in 1..=4 {
                let actual = iter.next().unwrap().bind(cx);
                assert_eq!(actual, expect);
            }
            assert!(iter.is_empty());
            Ok(())
        };
        func().unwrap();
    }
}
