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

pub(crate) struct ElemStreamIter<'rt> {
    cons: Option<&'rt mut Gc<RootCons>>,
    elem: &'rt mut Gc<RootObj>,
}

impl<'rt> ElemStreamIter<'rt> {
    pub(crate) fn new(cons: &'rt mut Gc<RootCons>, elem: &'rt mut Gc<RootObj>) -> Self {
        Self {
            cons: Some(cons),
            elem,
        }
    }
}

impl<'rt> StreamingIterator for ElemStreamIter<'rt> {
    type Item = Gc<RootObj>;

    fn advance(&mut self) {
        if let Some(cons) = &mut self.cons {
            let car = cons.obj().car.get();
            self.elem.set(car);
            match cons.obj().cdr.get() {
                Object::Cons(next) => {
                    let x = unsafe { std::mem::transmute::<&Cons, &Cons>(!next) };
                    cons.set(x);
                }
                _ => self.cons = None,
            }
        }
    }

    fn get(&self) -> Option<&Self::Item> {
        match &self.cons {
            Some(_) => Some(self.elem),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{arena::RootSet, cons, list};

    use super::*;

    #[test]
    fn elem_iter() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let cons: Object = list![1, 2, 3, 4; arena];
        let iter = cons.as_list(arena).unwrap();
        let vec: Result<Vec<_>> = iter.collect();
        assert_eq!(vec.unwrap(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn cons_iter() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let cons: Object = list![1, 2, 3, 4; arena];
        if let Object::Cons(cons) = cons {
            let list = List::Cons(cons);
            let iter = list.conses(arena);
            let expect = vec![1, 2, 3, 4];
            for (act, exp) in iter.zip(expect.iter()) {
                assert_eq!(act.unwrap().car(arena), *exp);
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    fn stream_iter() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let cons: Object = list![1, 2, 3, 4; arena];
        if let Object::Cons(cons) = cons {
            let cons_root = unsafe { &mut Gc::new(RootCons::new(&cons)) };
            let obj_root = unsafe { &mut Gc::new(RootObj::default()) };
            let mut iter = ElemStreamIter::new(cons_root, obj_root);
            let mut expect = vec![1, 2, 3, 4].into_iter();
            while let Some(elem) = iter.next() {
                let exp = expect.next().unwrap();
                assert_eq!(elem.obj(), exp);
            }
        } else {
            unreachable!();
        }
    }
}
