use crate::core::arena::Root;

use super::super::{
    arena::{Arena, Rt},
    error::{Error, Type},
    object::{Gc, GcObj, List, Object},
};
use super::ConstrainLifetime;
use streaming_iterator::StreamingIterator;

use anyhow::{anyhow, Result};

use super::Cons;

#[derive(Clone)]
pub(crate) struct ElemIter<'ob> {
    cons: Option<&'ob Cons>,
}

impl<'brw> Cons {
    pub(crate) fn elements<'new>(&'brw self, arena: &'new Arena) -> ElemIter<'new> {
        ElemIter {
            cons: Some(self.constrain_lifetime(arena)),
        }
    }
}

impl<'ob> Gc<List<'ob>> {
    pub(crate) fn elements(self, arena: &'ob Arena) -> ElemIter<'ob> {
        match self.get() {
            List::Nil => ElemIter { cons: None },
            List::Cons(cons) => ElemIter {
                cons: Some(cons.constrain_lifetime(arena)),
            },
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
                    Object::Nil => None,
                    _ => return Some(Err(anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons.car()))
            }
            None => None,
        }
    }
}

impl<'ob> GcObj<'ob> {
    pub(crate) fn as_list<'gc>(self, arena: &'gc Arena) -> Result<ElemIter<'gc>> {
        match self.get() {
            Object::Cons(cons) => {
                let cons = cons.constrain_lifetime(arena);
                Ok(ElemIter { cons: Some(cons) })
            }
            Object::Nil => Ok(ElemIter { cons: None }),
            _ => Err(Error::from_object(Type::List, self).into()),
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
                    Object::Nil => List::Nil,
                    _ => return Some(Err(anyhow::anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons))
            }
        }
    }
}

pub(crate) struct ElemStreamIter<'rt, 'rs> {
    elem: Option<&'rt mut Root<'rs, GcObj<'static>>>,
    cons: Option<&'rt mut Root<'rs, &'static Cons>>,
}

impl<'rt, 'rs> ElemStreamIter<'rt, 'rs> {
    pub(crate) fn new(
        elem: Option<&'rt mut Root<'rs, GcObj<'static>>>,
        cons: Option<&'rt mut Root<'rs, &'static Cons>>,
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
macro_rules! element_iter {
    ($ident:ident, $obj:expr, $gc:ident) => {
        #[allow(unused_assignments)]
        let mut root_elem = None;
        #[allow(unused_assignments)]
        let mut root_cons = None;
        let mut gc_root_elem = unsafe { $crate::core::arena::Root::new($gc.get_root_set()) };
        let mut gc_root_cons = unsafe { $crate::core::arena::Root::new($gc.get_root_set()) };
        let list: $crate::core::object::Gc<$crate::core::object::List> = $obj.try_into()?;
        #[allow(unused_mut)]
        let mut $ident = if let $crate::core::object::List::Cons(cons) = list.get() {
            unsafe {
                root_elem = Some($crate::core::object::GcObj::NIL);
                root_cons = Some($crate::core::object::WithLifetime::with_lifetime(cons));
                $crate::core::cons::ElemStreamIter::new(
                    Some($crate::core::arena::Root::init(
                        &mut gc_root_elem,
                        root_elem.as_mut().unwrap(),
                    )),
                    Some($crate::core::arena::Root::init(
                        &mut gc_root_cons,
                        root_cons.as_mut().unwrap(),
                    )),
                )
            }
        } else {
            std::mem::forget(gc_root_elem);
            std::mem::forget(gc_root_cons);
            $crate::core::cons::ElemStreamIter::new(None, None)
        };
    };
}

#[cfg(test)]
mod test {
    use super::super::super::arena::RootSet;
    use crate::list;

    use super::*;

    #[test]
    fn elem_iter() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let cons = list![1, 2, 3, 4; arena];
        let iter = cons.as_list(arena).unwrap();
        let vec: Result<Vec<_>> = iter.collect();
        assert_eq!(vec.unwrap(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn cons_iter() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let cons: GcObj = list![1, 2, 3, 4; arena];
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
            let arena = &Arena::new(roots);
            let cons: GcObj = list![1, 2, 3, 4; arena];
            element_iter!(iter, cons, arena);
            for expect in 1..=4 {
                let actual = iter.next().unwrap().bind(arena);
                assert_eq!(actual, expect);
            }
            assert!(iter.is_empty());
            Ok(())
        };
        func().unwrap();
    }
}
