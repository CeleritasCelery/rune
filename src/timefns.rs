//! Time analysis
use crate::core::{
    env::{sym, Env},
    error::{Type, TypeError},
    gc::{Context, Rt},
    object::{FlexInt, List, Object, ObjectType},
};
use rune_core::macros::list;
use rune_macros::{defun, elprop};
use std::time::{Duration, SystemTime};

defvar!(CURRENT_TIME_LIST, true);

const LO_TIME_BITS: u32 = 16;
const TIMESPEC_HZ: i64 = 1_000_000_000;
const LOG10_TIMESPEC_HZ: u32 = 9;
const TRILLION: i64 = 1_000_000_000_000;
const MILLION: i64 = 1_000_000;

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
    // let secs = duration
    //     .as_secs()
    //     .checked_mul(TIMESPEC_HZ)
    //     .map(|acc| acc.checked_add(duration.subsec_nanos().into()))
    //     .flatten()
    //     .unwrap();

    // list![secs, TIMESPEC_HZ; cx]

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

#[derive(Debug, Clone)]
pub(crate) struct TicksHz {
    ticks: FlexInt,
    hz: FlexInt,
}

impl<'ob> TryFrom<Object<'ob>> for TicksHz {
    type Error = TypeError;

    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        let duration = match obj.untag() {
            ObjectType::NIL => TicksHz::now(),
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
                            hz = TRILLION;
                        } else {
                            hz = MILLION;
                        }
                    } else if low_tail.is_nil() {
                        usec = low_tail.into();
                        hz = MILLION;
                    }
                    decode_time_components(high.untag(), low.untag(), usec, psec, hz)
                } else {
                    match (high.untag(), low.untag()) {
                        (ObjectType::Int(h), ObjectType::Int(l)) => Ok(TicksHz { ticks: h, hz: l }),
                        _ => Err("".into()),
                    }
                }
            }
            ObjectType::Int(x) => Ok(TicksHz { ticks: x, hz: 1 }),
            ObjectType::Float(x) => decode_float_time(**x),
            _ => Err("".into()),
        };

        duration.map_err(|_| TypeError::new(Type::String, obj))
    }
}

impl TicksHz {
    /// Elapsed time since the Epoch
    pub fn now() -> Result<Self, String> {
        let duration = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("System time is before the epoch");

        TicksHz::try_from(duration)
    }

    pub fn arith(self, rhs: TicksHz, subtract: bool) -> Option<TicksHz> {
        if self.hz == rhs.hz {
            let ticks = if subtract {
                self.ticks.checked_sub(rhs.ticks)
            } else {
                self.ticks.checked_add(rhs.ticks)
            }
            .unwrap();
            Some(TicksHz { ticks, hz: self.hz })
        } else {
            let da = self.hz;
            let db = rhs.hz;
            let da_lt_db = da < db;
            let hzmin = self.hz.min(rhs.hz);

            /* The plan is to compute (na * (db/g) + nb * (da/g)) / lcm (da, db)
            where g = gcd (da, db).   */
            let g = gcd(da, db);
            let fa = da / g;
            let fb = db / g;

            /* ihz = fa * db.  This is equal to lcm (da, db).  */
            let ihz = fa.checked_mul(db).expect("Overflow in lcm computation");

            /* iticks = (fb * na) OP (fa * nb), where OP is + or -.  */
            let iticks = if subtract {
                fb.checked_mul(self.ticks).and_then(|x| x.checked_sub(fa * rhs.ticks))
            } else {
                fb.checked_mul(self.ticks).and_then(|x| x.checked_add(fa * rhs.ticks))
            }
            .expect("Overflow in tick computation");

            let ig = gcd(iticks, ihz);
            let mut norm_ticks = iticks / ig;
            let mut norm_hz = ihz / ig;

            if norm_hz < hzmin {
                let rescale = (hzmin + norm_hz - 1) / norm_hz;
                norm_ticks = norm_ticks.checked_mul(rescale).expect("Overflow in rescaling");
                norm_hz = norm_hz.checked_mul(rescale).expect("Overflow in rescaling");
            }

            Some(TicksHz { ticks: norm_ticks, hz: norm_hz })
        }
    }

    pub fn make_lisp_time<'ob>(self, cx: &'ob Context) -> Object<'ob> {
        let mut scaled_ticks =
            self.ticks.checked_mul(TRILLION).expect("Overflow in ticks * trillion") / self.hz;

        let full_ps = scaled_ticks % TRILLION;
        let hi = scaled_ticks / TRILLION;
        let us = (full_ps / 1_000_000) as i32;
        let ps = (full_ps % 1_000_000) as i32;
        let lo = (scaled_ticks / ((1 << LO_TIME_BITS) - 1)) as i32;
        scaled_ticks >>= LO_TIME_BITS;

        list![hi, lo, us, ps; cx]
    }
}

impl TryFrom<Duration> for TicksHz {
    type Error = String;

    fn try_from(duration: Duration) -> Result<Self, Self::Error> {
        let ns: i64 = duration
            .as_secs()
            .checked_mul(TIMESPEC_HZ as u64)
            .and_then(|ns| ns.checked_add(duration.subsec_nanos() as u64))
            .and_then(|ns| TryInto::<i64>::try_into(ns).ok())
            .ok_or("Overflow in duration conversion")?;

        Ok(TicksHz { ticks: ns, hz: TIMESPEC_HZ })
    }
}

// fn ticks_hz_to_duration(ticks: u64, hz: u64) -> Duration {
//     if hz == TIMESPEC_HZ {
//         let mut secs = ticks / TIMESPEC_HZ;
//         let mut nanos = ticks % TIMESPEC_HZ;
//         if nanos < 0 {
//             secs -= 1;
//             nanos += TIMESPEC_HZ;
//         }
//         return Duration::new(secs, nanos as u32);
//     } else if hz == 1 {
//         return Duration::from_secs(ticks);
//     } else {
//         let q = ticks as u128 * TIMESPEC_HZ as u128 / hz as u128;
//         let nanos = q % TIMESPEC_HZ as u128;
//         let secs = q / TIMESPEC_HZ as u128;
//         return Duration::new(secs as u64, nanos as u32);
//     }
// }

fn decode_float_time(t: f64) -> Result<TicksHz, String> {
    if t == 0.0 {
        return Ok(TicksHz { ticks: 0, hz: 1 });
    }

    let mut scale = double_integer_scale(t);
    if scale < 0 {
        scale = 0;
    }

    let scaled = t * 2f64.powi(scale);
    if scaled.fract() != 0.0 {
        return Err("Scaled time is not an integer".to_string());
    }

    let ticks = scaled as i64;
    let hz = if scale < 0 { 1 } else { 2u64.pow(scale as u32) };
    return Ok(TicksHz { ticks, hz: hz as i64 });
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
    hz: i64,
) -> Result<TicksHz, String> {
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
    us += ps / MILLION - (if ps % MILLION < 0 { 1 } else { 0 });
    let s_from_us_ps = us / MILLION - (if us % MILLION < 0 { 1 } else { 0 });
    ps = ps % MILLION + if ps % MILLION < 0 { MILLION } else { 0 };
    us = us % MILLION + if us % MILLION < 0 { MILLION } else { 0 };

    let iticks = high
        .checked_mul(1_i64 << LO_TIME_BITS)
        .and_then(|t| t.checked_add(low + s_from_us_ps));

    if let Some(iticks) = iticks {
        let iticks = if hz == TRILLION {
            iticks
                .checked_mul(MILLION)
                .and_then(|ticks| ticks.checked_add(us * MILLION + ps))
        } else if hz == MILLION {
            iticks.checked_mul(MILLION).and_then(|ticks| ticks.checked_add(us))
        } else {
            Some(iticks)
        };

        if let Some(ticks) = iticks {
            return Ok(TicksHz { ticks, hz });
        }
    }

    Err("yikes".into())
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a.abs()
}

#[defun]
#[elprop((u16, u16, u16, u16), (u16, u16, u16, u16))]
fn time_add<'ob>(a: TicksHz, b: TicksHz, cx: &'ob Context) -> Object<'ob> {
    let c = a.arith(b, false).unwrap();
    c.make_lisp_time(cx)
}

#[cfg(test)]
mod test {
    use crate::{core::gc::RootSet, interpreter::assert_lisp};

    use super::*;

    #[test]
    fn test_time_add() {
        // let roots = &RootSet::default();
        // let cx = &mut Context::new(roots);
        // // let a = list![9014139989043246_u64,19280505220977732_u64,26614407391310031_u64,25846759022428321_u64; cx];
        // // let a = list![9014139989043246_u64,19280505220977732_u64,26614407391310031_u64,25846759022428321_u64; cx];
        // let a = list![0,0,0,0; cx];
        // let b = list![0,0,0,0; cx];
        // // let b = list![35798992106863009,34883050868135289,25553851890024156,2709095831588624; cx];

        // LispTime::try_from(a).unwrap();
        // assert_lisp("(time-add '(0 0 0 0) '(0 0 0 0))", "(0 0 0 0)");
        // assert_lisp("(time-add '(0 0 0 0) '(0 0 0 1))", "(0 0 0 1)");
        // assert_lisp("(time-add '(0 0 0 0) '(0 8371702622960 8766624912443796 -4076542295867604))", "(0 0 0 1)");

        assert_lisp("(time-add '(33046 5079 16386 46222) '(18016 34998 64648 3816))", "(0 0 0 1)");
        // (time-add '(33046 5079 16386 46222) '(18016 34998 64648 3816))
        // (3346439309081034050038 . 1000000000000)


    }
}
