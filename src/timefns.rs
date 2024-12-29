//! Time analysis
use crate::core::{
    env::{sym, Env},
    error::{Type, TypeError},
    gc::{Context, Rt},
    object::{List, Object, ObjectType},
};
use rune_core::macros::list;
use rune_macros::defun;
use std::time::{Duration, SystemTime};

defvar!(CURRENT_TIME_LIST, true);

const LO_TIME_BITS: u32 = 16;
const TIMESPEC_HZ: u64 = 1000000000;
const LOG10_TIMESPEC_HZ: u32 = 9;
const TRILLION: u64 = 1_000_000_000_000;

// struct TicksHz {
//   /* Clock count as a Lisp integer.  */
//   Lisp_Object ticks;

//   /* Clock frequency (ticks per second) as a positive Lisp integer.  */
//   Lisp_Object hz;

// }

#[defun]
fn current_time<'ob>(cx: &'ob Context, env: &Rt<Env>) -> Object<'ob> {
    assert!(
        env.vars.get(sym::CURRENT_TIME_LIST).unwrap() == &sym::TRUE,
        "current-time-list is nil"
    );
    let duration = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("System time is before the epoch");

    make_lisp_time(cx, duration)
}

fn make_lisp_time<'ob>(cx: &'ob Context, duration: Duration) -> Object<'ob> {
    let secs = duration.as_secs();
    let nanos = duration.subsec_nanos();

    let low = secs & 0xffff;
    let high = secs >> LO_TIME_BITS;

    let micros = nanos / 1000;
    let picos = nanos % 1000 * 1000;
    list![high, low, micros, picos; cx]
}

// #[defun]
// fn time_add<'ob>(a: List<'ob>, b: List<'ob>) -> Result<List<'ob>> {
//     let [first, second, third, fourth] = a.elements().collect::<Result<Vec<_>, _>>()?[..] else {
//         bail!("Invalid time list {a}")
//     };
//     todo!()
// }

// Decode a Lisp timestamp SPECIFIED_TIME that represents a time.
//
// Return a (form, time) pair that is the form of SPECIFIED-TIME
// and the resulting C timestamp in CFORM form.
// If CFORM == CFORM_SECS_ONLY, ignore and do not validate any sub-second
// components of an old-format SPECIFIED_TIME.
//
// Signal an error if unsuccessful.
//
//  specified_time is one of:
//
//    nil
//      current time
//    NUMBER
//      that number of seconds
//    (A . B)    ; A, B : integer, B>0
//      A/B s
//    (A B C D)  ; A, B : integer, C, D : fixnum
//      (A * 2**16 + B + C / 10**6 + D / 10**12) s
//
//    The following specified_time forms are also supported,
//    for compatibility with older Emacs versions:
//
//    (A B)
//      like (A B 0 0)
//    (A B . C)  ; C : fixnum
//      like (A B C 0)
//    (A B C)
//      like (A B C 0)
#[derive(Debug, Clone, Copy)]
pub(crate) struct LispTime(Duration);

impl<'ob> TryFrom<Object<'ob>> for LispTime {
    type Error = TypeError;

    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        let duration = match obj.untag() {
            ObjectType::NIL => {
                let duration = SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .expect("System time is before the epoch");
                Ok(duration)
            }
            ObjectType::Cons(x) => {
                let high = x.car();
                let low = x.cdr();
                let mut usec = ObjectType::Int(0);
                let mut psec = ObjectType::Int(0);
                let mut hz = 0;

                if let ObjectType::Cons(l) = low.untag() {
                    let low = l.car();
                    let low_tail = l.cdr();

                    if let ObjectType::Cons(l) = low_tail.untag() {
                        usec = l.car().into();
                        let low_tail = l.cdr();

                        if let ObjectType::Cons(l) = low_tail.untag() {
                            psec = l.car().into();
                            hz = 1000000000000;
                        } else {
                            hz = 1000000;
                        }
                    } else if low_tail.is_nil() {
                        usec = low_tail.into();
                        hz = 1000000;
                    }
                    decode_time_components(high.untag(), low.untag(), usec, psec, hz)
                } else {
                    match (high.untag(), low.untag()) {
                        (ObjectType::Int(h), ObjectType::Int(l)) => {
                            Ok(ticks_hz_to_duration(h as u64, l as u64))
                        }
                        _ => Err("".into()),
                    }
                }
            }
            ObjectType::Int(x) => Ok(ticks_hz_to_duration(x as u64, 1)),
            ObjectType::Float(x) => decode_float_time(**x),
            _ => Err("".into()),
        };

        duration.map_err(|e| TypeError::new(Type::String, obj)).map(|d| LispTime(d))
    }
}

fn ticks_hz_to_duration(ticks: u64, hz: u64) -> Duration {
    if hz == TIMESPEC_HZ {
        let mut secs = ticks / TIMESPEC_HZ;
        let mut nanos = ticks % TIMESPEC_HZ;
        if nanos < 0 {
            secs -= 1;
            nanos += TIMESPEC_HZ;
        }
        return Duration::new(secs, nanos as u32);
    } else if hz == 1 {
        return Duration::from_secs(ticks);
    } else {
        let q = ticks as u128 * TIMESPEC_HZ as u128 / hz as u128;
        let nanos = q % TIMESPEC_HZ as u128;
        let secs = q / TIMESPEC_HZ as u128;
        return Duration::new(secs as u64, nanos as u32);
    }
}

fn decode_float_time(t: f64) -> Result<Duration, String> {
    if t == 0.0 {
        return Ok(ticks_hz_to_duration(0, 1));
    }

    let scale = double_integer_scale(t);
    if scale < 0 {
        return Err("Scale is negative, time too large".to_string());
    }

    let scaled = t * 2f64.powi(scale);
    if scaled.fract() != 0.0 {
        return Err("Scaled time is not an integer".to_string());
    }

    let ticks = scaled as u64;
    let hz = if scale < 0 { 1 } else { 2u64.pow(scale as u32) };

    let secs = ticks / hz;
    let nanos = ((ticks % hz) as f64 * 1_000_000_000.0 / hz as f64) as u32;

    if nanos >= 1_000_000_000 {
        return Err("Nanoseconds out of range".to_string());
    }

    Ok(Duration::new(secs, nanos))
}

fn double_integer_scale(d: f64) -> i32 {
    if d == 0.0 || d.is_nan() || d.is_infinite() {
        return 0; // Special case: scale is zero for zero, NaN, or infinity
    }

    let mut e = 0;
    let mut value = d.abs();

    while value.fract() != 0.0 {
        value *= f64::from(std::f32::RADIX);
        e -= 1;
    }

    while value % f64::from(std::f32::RADIX) == 0.0 && value > 1.0 {
        value /= f64::from(std::f32::RADIX);
        e += 1;
    }
    e
}

fn decode_time_components<'ob>(
    high: ObjectType<'ob>,
    low: ObjectType<'ob>,
    usec: ObjectType<'ob>,
    psec: ObjectType<'ob>,
    hz: u64,
) -> Result<Duration, String> {
    let elements = [high, low, usec, psec]
        .iter()
        .map(|e| {
            if let ObjectType::Int(x) = e {
                Ok::<i64, String>(*x)
            } else {
                Err("Invalid type".into())
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    if elements.len() != 4 {
        return Err("Invalid types".into());
    }
    let [high, low, mut us, mut ps] = [elements[0], elements[1], elements[2], elements[3]];

    /* Normalize out-of-range lower-order components by carrying
    each overflow into the next higher-order component.  */
    us += ps / 1000000 - (if ps % 1000000 < 0 { 1 } else { 0 });
    let s_from_us_ps = us / 1000000 - (if us % 1000000 < 0 { 1 } else { 0 });
    ps = ps % 1000000 + if ps % 1000000 < 0 { 1000000 } else { 0 };
    us = us % 1000000 + if us % 1000000 < 0 { 1000000 } else { 0 };

    let iticks = high * (1 << LO_TIME_BITS) + low + s_from_us_ps;

    if hz == TRILLION {
        if iticks.checked_mul(TRILLION as i64).is_some()
            && iticks.checked_add((us * 1000000 + ps) as i64).is_some()
        {
            let iticks = iticks * TRILLION as i64 + (us * 1000000 + ps) as i64;
            return Ok(ticks_hz_to_duration(iticks as u64, hz));
        }
    } else if hz == 1000000 {
        if iticks.checked_mul(1000000).is_some() && iticks.checked_add(us).is_some() {
            let iticks = iticks * 1000000 + us;
            return Ok(ticks_hz_to_duration(iticks as u64, hz));
        }
    }
    return Ok(ticks_hz_to_duration(iticks as u64, hz));
}

#[defun]
fn time_add<'ob>(a: LispTime, b: LispTime, cx: &'ob Context) -> Object<'ob> {
    let c = a.0 + b.0;
    // let c = SystemTime::now()
    // .duration_since(SystemTime::UNIX_EPOCH)
    // .expect("System time is before the epoch");
    make_lisp_time(cx, c)
}
