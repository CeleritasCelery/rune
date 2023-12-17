//! Time analysis
use crate::core::{
    env::{sym, Env},
    gc::{Context, Rt},
    object::GcObj,
};
use rune_core::macros::list;
use rune_macros::defun;
use std::time::SystemTime;

defvar!(CURRENT_TIME_LIST, true);

#[defun]
fn current_time<'ob>(cx: &'ob Context, env: &Rt<Env>) -> GcObj<'ob> {
    assert!(
        env.vars.get(sym::CURRENT_TIME_LIST).unwrap() == &sym::TRUE,
        "current-time-list is nil"
    );
    let duration = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("System time is before the epoch");

    let secs = duration.as_secs();
    let micros = duration.subsec_micros();
    let low = secs & 0xffff;
    let high = secs >> 16;

    list![high, low, micros, 0; cx]
}
