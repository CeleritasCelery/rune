#![feature(test)]
use std::ops::{Add, Sub};

extern crate test;

pub trait Numeric: Add<Output=Self> + Sub<Output=Self> + PartialEq + Copy {
    fn one() -> Self;
    fn add(x: Self, y: Self) -> Self;
}

impl Numeric for u8 {
    fn one() -> Self { 1 }
    fn add(x: Self, y: Self) -> Self {y.overflowing_add(x).0}
}

impl Numeric for u16 {
    fn one() -> Self { 1 }
    fn add(x: Self, y: Self) -> Self {y.overflowing_add(x).0}
}

impl Numeric for u32 {
    fn one() -> Self { 1 }
    fn add(x: Self, y: Self) -> Self {y.overflowing_add(x).0}
}

impl Numeric for u64 {
    fn one() -> Self { 1 }
    fn add(x: Self, y: Self) -> Self {y.overflowing_add(x).0}
}

impl Numeric for f64 {
    fn one() -> Self { 1.1 }
    fn add(x: Self, y: Self) -> Self {y.add(x)}
}

impl Numeric for f32 {
    fn one() -> Self { 1.1 }
    fn add(x: Self, y: Self) -> Self {y.add(x)}
}

mod array {
    use crate::Numeric;

    pub fn fill<T: Numeric> () -> Vec<T> {
        let mut array: Vec<T> = Vec::new();
        let mut num = T::one();
        for _i in 1..=1000000 {
            array.push(num);
            num = num.add(T::one());
        }
        array
    }

    pub fn sum<T: Numeric> (array: Vec<T>) -> T {
        let mut sum = T::one();
        for n in array {
            sum = sum.add(n);
        }
        sum
    }
}


mod cons {
    use std::collections::LinkedList;
    use crate::Numeric;

    pub fn fill<T: Numeric> () -> LinkedList<T> {
        let mut list: LinkedList<T> = LinkedList::new();
        let mut num = T::one();
        for _i in 1..=1000000 {
            list.push_back(num);
            num = num.add(T::one());
        }
        list
    }

    pub fn sum<T: Numeric> (list: LinkedList<T>) -> T {
        let mut sum = T::one();
        for n in list {
            sum = sum.add(n);
        }
        sum
    }
}


#[cfg(test)]
mod tests {
    use test::Bencher;
    use crate::array;
    use crate::cons;
    use std::collections::LinkedList;

    #[bench]
    fn array_u64 (b: &mut Bencher) {
        let array: Vec<u64> = array::fill();
        b.iter(|| array::sum(array.clone()))
    }

    #[bench]
    fn array_u32 (b: &mut Bencher) {
        let array: Vec<u32> = array::fill();
        b.iter(|| array::sum(array.clone()))
    }

    #[bench]
    fn array_u16 (b: &mut Bencher) {
        let array: Vec<u16> = array::fill();
        b.iter(|| array::sum(array.clone()))
    }

    #[bench]
    fn array_u8 (b: &mut Bencher) {
        let array: Vec<u8> = array::fill();
        b.iter(|| array::sum(array.clone()))
    }

    #[bench]
    fn array_f64 (b: &mut Bencher) {
        let array: Vec<f64> = array::fill();
        b.iter(|| array::sum(array.clone()))
    }

    #[bench]
    fn array_f32 (b: &mut Bencher) {
        let array: Vec<f32> = array::fill();
        b.iter(|| array::sum(array.clone()))
    }


    #[bench]
    fn cons_u64 (b: &mut Bencher) {
        let list: LinkedList<u64> = cons::fill();
        b.iter(|| cons::sum(list.clone()))
    }

    #[bench]
    fn cons_u32 (b: &mut Bencher) {
        let list: LinkedList<u32> = cons::fill();
        b.iter(|| cons::sum(list.clone()))
    }
}
