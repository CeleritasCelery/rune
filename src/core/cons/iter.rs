use crate::core::{gc::Slot, object::List};

use super::super::{
    gc::Rt,
    object::{ListType, Object, ObjectType},
};
use super::Cons;
use anyhow::Result;

#[derive(Clone)]
pub(crate) struct ConsIter<'ob> {
    cons: Option<Result<&'ob Cons, ConsError>>,
    fast: Option<&'ob Cons>,
}

/// An iterator over cons cells. This iterator will detect circular lists and
/// non-nil list terminators.
impl<'ob> ConsIter<'ob> {
    fn new(cons: Option<&'ob Cons>) -> Self {
        Self { cons: cons.map(Ok), fast: cons }
    }

    pub(crate) fn fallible(self) -> fallible_iterator::Convert<Self> {
        fallible_iterator::convert(self)
    }
}

impl<'ob> Iterator for ConsIter<'ob> {
    type Item = Result<&'ob Cons, ConsError>;

    fn next(&mut self) -> Option<Self::Item> {
        let cons = match self.cons? {
            Ok(c) => c,
            Err(e) => return Some(Err(e)),
        };
        self.cons = match cons.cdr().untag() {
            ObjectType::Cons(next) => Some(Ok(next)),
            ObjectType::NIL => None,
            _ => Some(Err(ConsError::NonNilCdr)),
        };

        // Floyds cycle detection algorithm
        self.fast = advance(advance(self.fast));
        if let (Some(Ok(slow)), Some(fast)) = (self.cons, self.fast) {
            if std::ptr::eq(slow, fast) {
                self.cons = Some(Err(ConsError::CircularList));
            }
        }
        Some(Ok(cons))
    }
}

fn advance(cons: Option<&Cons>) -> Option<&Cons> {
    match cons?.cdr().untag() {
        ObjectType::Cons(next) => Some(next),
        _ => None,
    }
}

/// An iterator over the elements (car's) of a list. This iterator will detect circular
/// lists and non-nil list terminators.
#[derive(Clone)]
pub(crate) struct ElemIter<'ob>(ConsIter<'ob>);

impl ElemIter<'_> {
    pub(crate) fn len(&self) -> Result<usize, ConsError> {
        use fallible_iterator::FallibleIterator;
        self.clone().fallible().count()
    }

    /// Take the rest of the list as a cons.
    pub(crate) fn rest(&self) -> Result<Option<&Cons>, ConsError> {
        self.0.cons.transpose()
    }

    pub(crate) fn fallible(self) -> fallible_iterator::Convert<Self> {
        fallible_iterator::convert(self)
    }
}

impl<'ob> Iterator for ElemIter<'ob> {
    type Item = Result<Object<'ob>, ConsError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|x| x.map(|x| x.car()))
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum ConsError {
    NonNilCdr,
    CircularList,
}

impl std::fmt::Display for ConsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConsError::NonNilCdr => write!(f, "non-nil cdr at end of list"),
            ConsError::CircularList => write!(f, "Circular list"),
        }
    }
}

impl std::error::Error for ConsError {}

pub(crate) struct ElemStreamIter<'rt> {
    elem: Option<&'rt mut Rt<Slot<Object<'static>>>>,
    cons: Option<Result<&'rt mut Rt<Slot<&'static Cons>>, ConsError>>,
}

impl<'rt> ElemStreamIter<'rt> {
    pub(crate) fn new(
        elem: Option<&'rt mut Rt<Slot<Object<'static>>>>,
        cons: Option<&'rt mut Rt<Slot<&'static Cons>>>,
    ) -> Self {
        Self { elem, cons: cons.map(Ok) }
    }
}

impl<'rt> fallible_streaming_iterator::FallibleStreamingIterator for ElemStreamIter<'rt> {
    type Item = Rt<Slot<Object<'static>>>;
    type Error = ConsError;

    fn advance(&mut self) -> Result<(), ConsError> {
        if let Some(cons) = &mut self.cons {
            let cons = match cons {
                Ok(x) => x,
                Err(e) => return Err(*e),
            };
            let elem = self.elem.as_mut().expect("Element should never be None while Cons is Some");
            let car = unsafe { cons.bind_unchecked().car() };
            elem.set(car);
            match unsafe { cons.bind_unchecked().cdr().untag() } {
                ObjectType::Cons(next) => {
                    // dissociate the borrow of cons from cell
                    let x = unsafe { std::mem::transmute::<&Cons, &Cons>(next) };
                    cons.set(x);
                }
                ObjectType::NIL => self.cons = None,
                _ => self.cons = Some(Err(ConsError::NonNilCdr)),
            }
        } else {
            self.elem = None;
        }
        Ok(())
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

#[allow(clippy::multiple_inherent_impl)]
impl Cons {
    pub(crate) fn elements(&self) -> ElemIter {
        ElemIter(self.conses())
    }

    pub(crate) fn conses(&self) -> ConsIter {
        ConsIter::new(Some(self))
    }
}

impl<'ob> IntoIterator for &'ob Cons {
    type Item = <ElemIter<'ob> as Iterator>::Item;

    type IntoIter = ElemIter<'ob>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements()
    }
}

impl<'ob> List<'ob> {
    pub(crate) fn elements(self) -> ElemIter<'ob> {
        ElemIter(self.conses())
    }

    pub(crate) fn conses(self) -> ConsIter<'ob> {
        match self.untag() {
            ListType::Nil => ConsIter::new(None),
            ListType::Cons(cons) => ConsIter::new(Some(cons)),
        }
    }
}

impl<'ob> IntoIterator for List<'ob> {
    type Item = <ElemIter<'ob> as Iterator>::Item;

    type IntoIter = ElemIter<'ob>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements()
    }
}

impl<'ob> Object<'ob> {
    pub(crate) fn as_list(self) -> Result<ElemIter<'ob>> {
        let list: List = self.try_into()?;
        Ok(list.elements())
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
        let slot = match $value {
            value => unsafe { $crate::core::gc::IntoRoot::into_root(value) },
        };
        let list: $crate::core::object::List = (*slot).try_into()?;
        #[allow(unused_qualifications, unused_mut)]
        let mut $ident = if let $crate::core::object::ListType::Cons(head) = list.untag() {
            use $crate::core::{cons, gc, object};
            // If the list is not empty, then initialize the roots and put them
            // in the stack space reserved
            unsafe {
                elem = Slot::new(object::NIL);
                cons = Slot::new(object::WithLifetime::with_lifetime(head));
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
    use fallible_iterator::FallibleIterator;
    use fallible_streaming_iterator::FallibleStreamingIterator;

    use super::super::super::gc::{Context, RootSet};
    use rune_core::macros::list;

    use super::*;

    #[test]
    fn elem_iter() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list![1, 2, 3, 4; cx];
        let iter = cons.as_list().unwrap();
        let vec: Vec<_> = iter.fallible().collect().unwrap();
        assert_eq!(vec, vec![1, 2, 3, 4]);
    }

    #[test]
    fn circular_list() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list![1; cx];
        cons.as_cons().set_cdr(cons).unwrap();
        let mut iter = cons.as_list().unwrap();
        assert!(iter.next().unwrap().is_ok());
        assert!(iter.next().unwrap().is_err());

        let cons = list![1, 2, 3; cx];
        cons.as_cons().cdr().as_cons().cdr().as_cons().set_cdr(cons).unwrap();
        let iter = cons.as_list().unwrap();
        assert!(iter.fallible().nth(3).is_err());

        let cons = list![1, 2, 3, 4; cx];
        let middle = cons.as_cons().cdr().as_cons().cdr();
        middle.as_cons().cdr().as_cons().set_cdr(middle).unwrap();
        let iter = cons.as_list().unwrap();
        assert!(iter.fallible().nth(3).is_err());
    }

    #[test]
    fn cons_iter() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list![1, 2, 3, 4; cx];
        let list: List = cons.try_into().unwrap();
        let mut iter = list.conses();
        for expect in [1, 2, 3, 4] {
            let actual = iter.next().unwrap().unwrap().car();
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
                let actual = iter.next().unwrap().unwrap().bind(cx);
                assert_eq!(actual, expect);
            }
            assert!(iter.is_empty());
            Ok(())
        };
        func().unwrap();
    }
}
