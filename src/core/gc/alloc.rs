use crate::core::cons::Cons;
use crate::core::env::Symbol;
use crate::core::object::{ByteFn, LispFloat, LispHashTable, LispString, LispVec};
use std::fmt::Debug;

use super::Block;

/// The owner of an object allocation. No references to
/// the object can outlive this.
#[derive(Debug)]
pub(super) enum OwnedObject {
    Float(Box<LispFloat>),
    Cons(Box<Cons>),
    Vec(Box<LispVec>),
    HashTable(Box<LispHashTable>),
    String(Box<LispString>),
    Symbol(Box<Symbol>),
    ByteFn(Box<ByteFn>),
}

pub(in crate::core) trait AllocObject
where
    Self: Sized,
{
    type Output;
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output;
}

impl AllocObject for f64 {
    type Output = LispFloat;
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<C>::register(
            &mut objects,
            OwnedObject::Float(Box::new(LispFloat::new(self))),
        );
        let Some(OwnedObject::Float(x)) = objects.last() else {unreachable!()};
        x.as_ref()
    }
}

impl AllocObject for Cons {
    type Output = Cons;
    fn alloc_obj<const CONST: bool>(mut self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        if CONST {
            self.mark_const();
        }
        Block::<CONST>::register(&mut objects, OwnedObject::Cons(Box::new(self)));
        let Some(OwnedObject::Cons(x)) = objects.last() else {unreachable!()};
        x.as_ref()
    }
}

impl AllocObject for Symbol {
    type Output = Symbol;
    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<CONST>::register(&mut objects, OwnedObject::Symbol(Box::new(self)));
        let Some(OwnedObject::Symbol(x)) = objects.last() else {unreachable!()};
        x.as_ref()
    }
}

impl AllocObject for LispString {
    type Output = Self;

    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<C>::register(&mut objects, OwnedObject::String(Box::new(self)));
        let Some(OwnedObject::String(x)) = objects.last_mut() else {unreachable!()};
        x.as_ref()
    }
}

impl AllocObject for ByteFn {
    type Output = ByteFn;
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        let boxed = Box::new(self);
        Block::<C>::register(&mut objects, OwnedObject::ByteFn(boxed));
        let Some(OwnedObject::ByteFn(x)) = objects.last() else {unreachable!()};
        x.as_ref()
    }
}

impl AllocObject for LispVec {
    type Output = LispVec;

    fn alloc_obj<const CONST: bool>(mut self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        if CONST {
            self.make_const();
        }
        Block::<CONST>::register(&mut objects, OwnedObject::Vec(Box::new(self)));
        let Some(OwnedObject::Vec(x)) = objects.last() else {unreachable!()};
        x.as_ref()
    }
}

impl AllocObject for LispHashTable {
    type Output = Self;

    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        if CONST {
            self.make_const();
        }
        Block::<CONST>::register(&mut objects, OwnedObject::HashTable(Box::new(self)));
        let Some(OwnedObject::HashTable(x)) = objects.last() else {unreachable!()};
        x.as_ref()
    }
}
