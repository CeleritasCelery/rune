use crate::{
    arena::{Arena, ConstrainLifetime, Gc, RootCons, RootObj},
    error::{Error, Type},
    object::{List, Object},
};
use streaming_iterator::StreamingIterator;

use anyhow::{anyhow, Result};

use super::Cons;

#[derive(Clone)]
pub(crate) struct ElemIter<'ob> {
    cons: Option<&'ob Cons<'ob>>,
    arena: &'ob Arena<'ob>,
}

impl<'brw, 'ob> Cons<'ob> {
    pub(crate) fn elements<'new>(&'brw self, arena: &'new Arena) -> ElemIter<'new> {
        ElemIter {
            cons: Some(self.constrain_lifetime(arena)),
            arena,
        }
    }
}

impl<'ob> List<'ob> {
    pub(crate) fn elements(self, arena: &'ob Arena) -> ElemIter<'ob> {
        match self {
            List::Nil => ElemIter { cons: None, arena },
            List::Cons(cons) => ElemIter {
                cons: Some((!cons).constrain_lifetime(arena)),
                arena,
            },
        }
    }

    pub(crate) fn conses(self, arena: &'ob Arena) -> ConsIter<'ob> {
        ConsIter { list: self, arena }
    }
}

impl<'ob> Iterator for ElemIter<'ob> {
    type Item = Result<Object<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cons {
            Some(cons) => {
                (*self).cons = match cons.cdr(self.arena) {
                    Object::Cons(next) => Some(!next),
                    Object::Nil(_) => None,
                    _ => return Some(Err(anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons.car(self.arena)))
            }
            None => None,
        }
    }
}

impl<'ob> Object<'ob> {
    pub(crate) fn as_list<'gc>(self, arena: &'gc Arena) -> Result<ElemIter<'gc>> {
        match self {
            Object::Cons(cons) => {
                let cons = (!cons).constrain_lifetime(arena);
                Ok(ElemIter {
                    cons: Some(cons),
                    arena,
                })
            }
            Object::Nil(_) => Ok(ElemIter { cons: None, arena }),
            _ => Err(Error::from_object(Type::List, self).into()),
        }
    }
}

impl<'ob> ElemIter<'ob> {
    pub(crate) fn is_empty(&self) -> bool {
        self.cons == None
    }

    pub(crate) fn len(&self) -> usize {
        self.clone().count()
    }
}

#[derive(Clone)]
pub(crate) struct ConsIter<'ob> {
    list: List<'ob>,
    arena: &'ob Arena<'ob>,
}

impl<'ob> Iterator for ConsIter<'ob> {
    type Item = Result<&'ob Cons<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.list {
            List::Nil => None,
            List::Cons(cons) => {
                self.list = match cons.cdr(self.arena) {
                    Object::Cons(next) => List::Cons(next),
                    Object::Nil(_) => List::Nil,
                    _ => return Some(Err(anyhow::anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(!cons))
            }
        }
    }
}

pub(crate) struct ElemStreamIter<'rt>(&'rt mut Option<(Gc<RootObj>, Gc<RootCons>)>);

impl<'rt> ElemStreamIter<'rt> {
    pub(crate) fn new(iter: &'rt mut Option<(Gc<RootObj>, Gc<RootCons>)>) -> Self {
        Self(iter)
    }
}

impl<'rt> StreamingIterator for ElemStreamIter<'rt> {
    type Item = Gc<RootObj>;

    fn advance(&mut self) {
        if let Some(iter) = &mut self.0 {
            let car = iter.1.obj().car.get();
            iter.0.set(car);
            match iter.1.obj().cdr.get() {
                Object::Cons(next) => {
                    let x = unsafe { std::mem::transmute::<&Cons, &Cons>(!next) };
                    iter.1.set(x);
                }
                _ => *self.0 = None,
            }
        }
    }

    fn get(&self) -> Option<&Self::Item> {
        match &self.0 {
            Some(iter) => Some(&iter.0),
            _ => None,
        }
    }
}

#[macro_export]
macro_rules! element_iter {
    ($ident:ident, $obj:expr) => {
        let mut root = None;
        #[allow(unused_qualifications)]
        let list: crate::object::List = $obj.try_into().unwrap();
        if let crate::object::List::Cons(x) = list {
            root = unsafe {
                Some((
                    Gc::new(crate::arena::RootObj::default()),
                    Gc::new(crate::arena::RootCons::new(!x)),
                ))
            };
        }
        let mut $ident = crate::cons::ElemStreamIter::new(&mut root);
    };
}

#[cfg(test)]
mod test {
    use crate::{arena::RootSet, cons, list};

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
        let cons = list![1, 2, 3, 4; arena];
        let cons: &Cons = cons.try_into().unwrap();
        let list = List::Cons(cons.into());
        let iter = list.conses(arena);
        let expect = vec![1, 2, 3, 4];
        for (act, exp) in iter.zip(expect.iter()) {
            assert_eq!(act.unwrap().car(arena), *exp);
        }
    }

    #[test]
    fn stream_iter() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let cons = list![1, 2, 3, 4; arena];
        element_iter!(iter, cons);
        let mut expect = vec![1, 2, 3, 4].into_iter();
        while let Some(elem) = iter.next() {
            let exp = expect.next().unwrap();
            assert_eq!(elem.obj(), exp);
        }
    }
}
