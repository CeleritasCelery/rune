#![allow(dead_code)]

use std::convert::{AsRef, AsMut};
use core::ptr::NonNull;

#[derive(Debug)]
pub struct Gc<T> {
    inner: NonNull<T>
}

impl<T> Gc<T> {
    pub fn new(x: T) -> Self {
        unsafe {
            Gc{inner: NonNull::new_unchecked(Box::into_raw(Box::new(x)))}
        }
    }
}

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        unsafe {self.inner.as_ref()}
    }
}

impl<T> AsMut<T> for Gc<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe {self.inner.as_mut()}
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.inner == rhs.inner
    }
}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc{inner: self.inner}
    }
}

impl<T> Drop for Gc<T> {
    fn drop(&mut self) {
        // don't do anything here because we will let the garbage collector
        // handle dropping this memory
    }
}
