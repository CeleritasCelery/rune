use super::super::{
    gc::{Root, Rt},
    object::{Gc, GcObj, List, Object},
};
use streaming_iterator::StreamingIterator;

use anyhow::{anyhow, Result};

use super::Cons;

#[derive(Clone)]
pub(crate) struct ElemIter<'ob> {
    cons: Option<&'ob Cons>,
}

#[allow(clippy::multiple_inherent_impl)]
impl Cons {
    pub(crate) fn elements(&self) -> ElemIter {
        ElemIter { cons: Some(self) }
    }
}

impl<'ob> Gc<List<'ob>> {
    pub(crate) fn elements(self) -> ElemIter<'ob> {
        match self.get() {
            List::Nil => ElemIter { cons: None },
            List::Cons(cons) => ElemIter { cons: Some(cons) },
        }
    }

    pub(crate) fn conses(self) -> ConsIter<'ob> {
        ConsIter { list: self.get() }
    }
}

impl<'ob> Iterator for ElemIter<'ob> {
    type Item = Result<GcObj<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cons {
            Some(cons) => {
                (*self).cons = match cons.cdr().get() {
                    Object::Cons(next) => Some(next),
                    Object::Symbol(s) if s.nil() => None,
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
        match list.get() {
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
                self.list = match cons.cdr().get() {
                    Object::Cons(next) => List::Cons(next),
                    Object::Symbol(s) if s.nil() => List::Nil,
                    _ => return Some(Err(anyhow::anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons))
            }
        }
    }
}

pub(crate) struct ElemStreamIter<'rt, 'rs> {
    elem: Option<&'rt mut Root<'rs, 'rt, GcObj<'static>>>,
    cons: Option<&'rt mut Root<'rs, 'rt, &'static Cons>>,
}

impl<'rt, 'rs> ElemStreamIter<'rt, 'rs> {
    pub(crate) fn new(
        elem: Option<&'rt mut Root<'rs, 'rt, GcObj<'static>>>,
        cons: Option<&'rt mut Root<'rs, 'rt, &'static Cons>>,
    ) -> Self {
        Self { elem, cons }
    }
}

impl<'rt, 'id> StreamingIterator for ElemStreamIter<'rt, 'id> {
    type Item = Rt<GcObj<'static>>;

    fn advance(&mut self) {
        if let Some(cons) = &mut self.cons {
            let elem = self
                .elem
                .as_mut()
                .expect("Element should never be None while Cons is Some");
            let cons = unsafe { cons.deref_mut_unchecked() };
            let elem = unsafe { elem.deref_mut_unchecked() };
            let car = unsafe { cons.bind_unchecked().car() };
            elem.set(car);
            match unsafe { cons.bind_unchecked().cdr().get() } {
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
        self.elem.as_ref().map(AsRef::as_ref)
    }
}

impl<'rt, 'id> ElemStreamIter<'rt, 'id> {
    pub(crate) fn is_empty(&self) -> bool {
        self.cons.is_none()
    }
}

#[macro_export]
macro_rules! rooted_iter {
    ($ident:ident, $value:expr, $cx:ident) => {
        // Allocate stack space for potential roots
        #[allow(unused_assignments)]
        let mut root_elem = None;
        #[allow(unused_assignments)]
        let mut root_cons = None;
        // Create roots, but don't initialize them
        let mut gc_root_elem = unsafe { $crate::core::gc::Root::new($cx.get_root_set()) };
        let mut gc_root_cons = unsafe { $crate::core::gc::Root::new($cx.get_root_set()) };
        // Convert the value into a list
        let obj = unsafe { $crate::core::gc::IntoRoot::into_root($value) };
        let list: $crate::core::object::Gc<$crate::core::object::List> = obj.try_into()?;
        #[allow(unused_mut)]
        let mut $ident = if let $crate::core::object::List::Cons(cons) = list.get() {
            // If the list is not empty, then initialize the roots and put them
            // in the stack space reserved
            unsafe {
                root_elem = Some($crate::core::object::nil());
                root_cons = Some($crate::core::object::WithLifetime::with_lifetime(cons));
                $crate::core::cons::ElemStreamIter::new(
                    Some($crate::core::gc::Root::init(
                        &mut gc_root_elem,
                        root_elem.as_mut().unwrap(),
                    )),
                    Some($crate::core::gc::Root::init(
                        &mut gc_root_cons,
                        root_cons.as_mut().unwrap(),
                    )),
                )
            }
        } else {
            // If the list is empty, forget the roots and return an iterator
            // that will yield None
            std::mem::forget(gc_root_elem);
            std::mem::forget(gc_root_cons);
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
        for (act, expect) in iter.zip(expects.into_iter()) {
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
