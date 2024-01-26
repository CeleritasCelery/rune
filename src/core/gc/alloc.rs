use super::{Block, GcHeap};
use crate::core::cons::Cons;
use crate::core::object::{
    ByteFn, ByteFnInner, ByteString, ByteStringInner, LispBuffer, LispBufferInner, LispFloat,
    LispFloatInner, LispHashTable, LispHashTableInner, LispString, LispStringInner, LispVec,
    LispVecInner, SymbolCell,
};
use std::fmt::Debug;

/// The owner of an object allocation. No references to
/// the object can outlive this.
#[derive(Debug)]
pub(super) enum OwnedObject {
    Float(Box<LispFloat>),
    Cons(Box<Cons>),
    Vec(Box<LispVec>),
    HashTable(Box<LispHashTable>),
    String(Box<LispString>),
    ByteString(Box<ByteString>),
    Symbol(Box<SymbolCell>),
    ByteFn(Box<ByteFn>),
    Buffer(Box<LispBuffer>),
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
        let f = LispFloatInner(self);
        Block::<C>::register(&mut objects, OwnedObject::Float(Box::new(GcHeap::new(f, block))));
        let Some(OwnedObject::Float(x)) = objects.last() else { unreachable!() };
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
        let Some(OwnedObject::Cons(x)) = objects.last() else { unreachable!() };
        x.as_ref()
    }
}

impl AllocObject for SymbolCell {
    type Output = SymbolCell;
    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<CONST>::register(&mut objects, OwnedObject::Symbol(Box::new(self)));
        let Some(OwnedObject::Symbol(x)) = objects.last() else { unreachable!() };
        x.as_ref()
    }
}

impl AllocObject for LispStringInner {
    type Output = LispString;

    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<C>::register(&mut objects, OwnedObject::String(Box::new(GcHeap::new(self, block))));
        let Some(OwnedObject::String(x)) = objects.last_mut() else { unreachable!() };
        x.as_ref()
    }
}

impl AllocObject for ByteStringInner {
    type Output = ByteString;

    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<C>::register(
            &mut objects,
            OwnedObject::ByteString(Box::new(GcHeap::new(self, block))),
        );
        let Some(OwnedObject::ByteString(x)) = objects.last_mut() else { unreachable!() };
        x.as_ref()
    }
}

impl AllocObject for ByteFnInner {
    type Output = ByteFn;
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        let boxed = Box::new(GcHeap::new(self, block));
        Block::<C>::register(&mut objects, OwnedObject::ByteFn(boxed));
        let Some(OwnedObject::ByteFn(x)) = objects.last() else { unreachable!() };
        x.as_ref()
    }
}

impl AllocObject for LispVecInner {
    type Output = LispVec;

    fn alloc_obj<const CONST: bool>(mut self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        if CONST {
            self.make_const();
        }
        Block::<CONST>::register(
            &mut objects,
            OwnedObject::Vec(Box::new(GcHeap::new(self, block))),
        );
        let Some(OwnedObject::Vec(x)) = objects.last() else { unreachable!() };
        x.as_ref()
    }
}

impl AllocObject for LispHashTableInner {
    type Output = LispHashTable;

    fn alloc_obj<const CONST: bool>(mut self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        if CONST {
            self.make_const();
        }
        Block::<CONST>::register(
            &mut objects,
            OwnedObject::HashTable(Box::new(GcHeap::new(self, block))),
        );
        let Some(OwnedObject::HashTable(x)) = objects.last() else { unreachable!() };
        x.as_ref()
    }
}

impl AllocObject for LispBufferInner {
    type Output = LispBuffer;

    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        assert!(CONST, "Buffers must only be created in the shared block");
        let mut objects = block.objects.borrow_mut();
        Block::<CONST>::register(
            &mut objects,
            OwnedObject::Buffer(Box::new(GcHeap::new(self, block))),
        );
        let Some(OwnedObject::Buffer(x)) = objects.last() else { unreachable!() };
        x.as_ref()
    }
}
