#![allow(dead_code)]

use std::convert::{AsRef, AsMut};
use core::ptr::NonNull;
use std::ops::Deref;

#[derive(Debug)]
pub struct Gc<T> {
    pointer: NonNull<T>
}

impl<T> Gc<T> {
    pub fn new(x: T) -> Self {
        unsafe {
            Gc{pointer: NonNull::new_unchecked(Box::into_raw(Box::new(x)))}
        }
    }
}

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe {self.pointer.as_ref()}
    }
}

impl<T> AsMut<T> for Gc<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe {self.pointer.as_mut()}
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.pointer == rhs.pointer
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc{pointer: self.pointer}
    }
}

impl<T> Drop for Gc<T> {
    fn drop(&mut self) {
        // don't do anything here because we will let the garbage collector
        // handle dropping this memory
    }
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe{self.pointer.as_ref()}
    }
}
