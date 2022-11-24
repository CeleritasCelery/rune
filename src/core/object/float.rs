use std::fmt::{Debug, Display};
use std::ops::Deref;

use crate::core::gc::{GcManaged, GcMark};

#[derive(PartialEq)]
pub(crate) struct LispFloat {
    gc: GcMark,
    float: f64,
}

impl LispFloat {
    pub(in crate::core) fn new(float: f64) -> Self {
        Self {
            gc: GcMark::default(),
            float,
        }
    }
}

impl Deref for LispFloat {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.float
    }
}

impl GcManaged for LispFloat {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Display for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let float = self.float;
        if float.fract() == 0.0_f64 {
            write!(f, "{float:.1}")
        } else {
            write!(f, "{float}")
        }
    }
}

impl Debug for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
