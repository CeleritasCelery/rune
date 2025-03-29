//! Time analysis
use crate::{
    arith::NumberValue, core::{
        cons::Cons,
        env::{sym, Env},
        gc::{Context, Rt},
        object::{IntoObject, Object, ObjectType},
    }, flexint::FlexInt, floatfns::double_integer_scale
};
use anyhow::anyhow;
use core::f64;
use libm::scalbn;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, ToPrimitive, Zero};
use rune_core::macros::list;
use rune_macros::defun;
use std::time::{Duration, SystemTime};

defvar!(CURRENT_TIME_LIST, true);

const LO_TIME_BITS: u32 = 16;
const TIMESPEC_HZ: i64 = 1_000_000_000;
const LOG10_TIMESPEC_HZ: u32 = 9;
const TRILLION: i64 = 1_000_000_000_000;
const MILLION: i64 = 1_000_000;

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
#[derive(Debug, Clone)]
pub(crate) struct TicksHz {
    /// Clock count as a Lisp integer.
    ticks: FlexInt,
    /// Clock frequency (ticks per second) as a positive Lisp integer.
    hz: FlexInt,
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

impl<'ob> TryFrom<Object<'ob>> for TicksHz {
    type Error = anyhow::Error;

    fn try_from(obj: Object<'ob>) -> anyhow::Result<Self> {
        let duration = match obj.untag() {
            ObjectType::NIL => TicksHz::now(),
            ObjectType::Cons(x) => {
                let high = x.car();
                let mut low = x.cdr();
                let mut usec = None;
                let mut psec = None;

                if let ObjectType::Cons(l) = low.untag() {
                    low = l.car();
                    let low_tail = l.cdr();

                    if let ObjectType::Cons(l) = low_tail.untag() {
                        usec = Some(l.car().into());
                        let low_tail = l.cdr();

                        if let ObjectType::Cons(l) = low_tail.untag() {
                            psec = Some(l.car().into());
                        }
                    }
                }
                decode_time_components(high.untag(), low.untag(), usec, psec)
            }
            ObjectType::Int(_) | ObjectType::BigInt(_) => {
                Ok(TicksHz { ticks: FlexInt::from(obj.untag()), hz: FlexInt::one() })
            }
            ObjectType::Float(x) => decode_float_time(**x),
            _ => Err(anyhow!("".to_string())),
        };
        duration
        // duration.map_err(|_| TypeError::new(Type::String, obj))
    }
}

impl TicksHz {
    /// Elapsed time since the Epoch
    pub fn now() -> anyhow::Result<Self> {
        let duration = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("System time is before the epoch");

        TicksHz::try_from(duration)
    }

    pub fn arith(self, rhs: TicksHz, subtract: bool) -> Option<TicksHz> {
        let (ticks, hz) = if self.hz == rhs.hz {
            //println!("{self:?} {rhs:?}");
            let ticks = if subtract { self.ticks - rhs.ticks } else { self.ticks + rhs.ticks };
            (ticks, self.hz)
            // Some(TicksHz { ticks, hz: self.hz })
        } else {
            // The plan is to decompose ta into na/da and tb into nb/db.
            // Start by computing da and db, their minimum (which will be
            // needed later) and the iticks temporary that will become
            // available once only their minimum is needed.
            let da = BigInt::from(self.hz);
            let db = BigInt::from(rhs.hz);

            let hzmin = da.clone().min(db.clone());
            /* The plan is to compute (na * (db/g) + nb * (da/g)) / lcm (da, db)
            where g = gcd (da, db).   */
            let g = da.gcd(&db);

            let fa = da / &g;
            let fb = db.clone() / g;

            /* ihz = fa * db.  This is equal to lcm (da, db).  */
            let mut ihz = fa.clone() * db;

            /* iticks = (fb * na) OP (fa * nb), where OP is + or -.  */
            let na = BigInt::from(self.ticks);
            let nb = BigInt::from(rhs.ticks);
            let mut iticks = fb * na;
            if subtract {
                iticks -= fa.clone() * nb;
            } else {
                iticks += fa * nb;
            }

            /* Normalize iticks/ihz by dividing both numerator and
            denominator by ig = gcd (iticks, ihz).  For speed, though,
            skip this division if ihz = 1.  */
            let ig = iticks.gcd(&ihz);
            if ig != BigInt::from(1) {
                iticks /= &ig;
                ihz /= ig;

                /* However, if dividing the denominator by ig would cause the
                denominator to become less than hzmin, rescale the denominator
                upwards by multiplying the normalized numerator and denominator
                so that the resulting denominator becomes at least hzmin.
                This rescaling avoids returning a timestamp that is less precise
                than both a and b.  */
                if ihz < hzmin {
                    /* Rescale straightforwardly.  Although this might not
                    yield the minimal denominator that preserves numeric
                    value and is at least hzmin, calculating such a
                    denominator would be too expensive because it would
                    require testing multisets of factors of lcm (da, db).  */
                    let rescale = hzmin / &ihz;
                    iticks *= &rescale;
                    ihz *= rescale;
                }
            }

            let hz = FlexInt::from(ihz);
            let ticks = FlexInt::from(iticks);
            (ticks, hz)
        };
        Some(TicksHz { ticks, hz })
    }

    pub fn make_lisp_time<'ob>(self, cx: &'ob Context) -> Object<'ob> {
        if self.hz.is_one() {
            return NumberValue::from(self.ticks).into_obj(cx);
        }

        if !trillion_factor(&self.hz) {
            // return list![NumberValue::from(self.ticks), NumberValue::from(self.hz); cx];
            return Cons::new(NumberValue::from(self.ticks), NumberValue::from(self.hz), cx).into();
        }

        /* mpz[0] = floor ((ticks * trillion) / hz).  */
        let mut scaled_ticks = BigInt::from(self.ticks);
        scaled_ticks *= TRILLION;
        //println!("scaled_ticks 1: {scaled_ticks}");
        scaled_ticks /= BigInt::from(self.hz);
        //println!("scaled_ticks 2: {scaled_ticks}");

        /* mpz[0] = floor (mpz[0] / trillion), with US = the high six digits of the
        12-digit remainder, and PS = the low six digits.  */
        let (scaled_ticks, remainder) = scaled_ticks.div_mod_floor(&BigInt::from(TRILLION));
        //println!("scaled_ticks 3: {scaled_ticks}, {remainder}");

        let full_ps = remainder.to_u64().unwrap();
        let us = full_ps / MILLION as u64;
        let ps = full_ps % MILLION as u64;

        /* mpz[0] = floor (mpz[0] / 1 << LO_TIME_BITS), with lo = remainder.  */
        let mut ulo =
            scaled_ticks.to_u64().unwrap_or(scaled_ticks.iter_u64_digits().next().unwrap());
        if scaled_ticks.is_negative() {
            ulo = (!ulo).wrapping_add(1); // Negate the value for unsigned types
        }

        //println!("scaled_ticks 4: {ulo}, {remainder}");
        let lo = ulo & ((1 << LO_TIME_BITS) - 1);
        //println!("scaled_ticks 5: {lo}, {remainder}");
        let hi = scaled_ticks.div_floor(&BigInt::from(2).pow(LO_TIME_BITS));
        let hi = FlexInt::from(hi);

        //println!("scaled_ticks 6: {hi:?}, {lo} {us} {ps}");

        list![NumberValue::from(hi), lo, us, ps; cx]
    }

    pub fn ticks_hz(&self) -> BigInt {
        let ticks = BigInt::from(self.ticks.clone());
        let hz = BigInt::from(self.hz.clone());
        ticks * hz
    }
}

impl TryFrom<Duration> for TicksHz {
    type Error = anyhow::Error;

    fn try_from(duration: Duration) -> anyhow::Result<Self> {
        let ns: i64 = duration
            .as_secs()
            .checked_mul(TIMESPEC_HZ as u64)
            .and_then(|ns| ns.checked_add(duration.subsec_nanos() as u64))
            .and_then(|ns| TryInto::<i64>::try_into(ns).ok())
            .ok_or(anyhow!("Overflow in duration conversion".to_string()))?;

        Ok(TicksHz { ticks: FlexInt::from(ns), hz: FlexInt::from(TIMESPEC_HZ) })
    }
}

// impl TryFrom<TicksHz> for Duration {
//     type Error = anyhow::Error;

//     fn try_from(t: TicksHz) -> Result<Self, Self::Error> {
//         if t.hz == TIMESPEC_HZ {
//             let mut s = t.ticks / TIMESPEC_HZ;
//             let mut ns = t.ticks % TIMESPEC_HZ;
//             if ns < 0 {
//                 s-=1;
//                 ns += TIMESPEC_HZ;
//             }
//             return Ok(Duration::new(s, ns));
//         }
//         else if t.hz == FlexInt::one() {
//             let mut ns = 0;
//         } else {
//             let mut q = t.ticks * FlexInt::Int(TIMESPEC_HZ);
//             q /=  t.hz; 
//             ns = 
//         }
//         todo!()
//     }
// }

impl PartialEq for TicksHz {
    fn eq(&self, other: &Self) -> bool {
        if self.ticks == other.ticks && self.hz == other.hz {
            return true;
        }

        let za = self.ticks_hz();
        let zb = other.ticks_hz();
        za == zb
    }
}

impl PartialOrd for TicksHz {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self == other {
            return Some(std::cmp::Ordering::Equal);
        }

        let za = self.ticks_hz();
        let zb = other.ticks_hz();
        za.partial_cmp(&zb)
    }
}

// impl Mul for TicksHz {
//     type Output = TicksHz;

//     fn mul(self, rhs: Self) -> Self::Output {
//         let mut ticks = BigInt::from(self.ticks);
//         ticks *= BigInt::from(rhs.ticks);

//         let mut hz = BigInt::from(self.hz);
//         hz *= BigInt::from(rhs.hz);

//         TicksHz { ticks: FlexInt::Big(ticks).shrink(), hz: FlexInt::Big(hz).shrink() }
//     }
// }

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

/// Convert the finite number T into an Emacs time *RESULT, truncating
/// toward minus infinity.  Signal an error if unsuccessful.
fn decode_float_time(t: f64) -> anyhow::Result<TicksHz> {
    if t == 0.0 {
        return Ok(TicksHz { ticks: FlexInt::zero(), hz: FlexInt::one() });
    }

    let mut scale = double_integer_scale(t);
    // Because SCALE treats trailing zeros in T as significant,
    // on typical platforms with IEEE floating point
    // (time-convert 3.5 t) yields (7881299347898368 . 2251799813685248),
    // a precision of 2**-51 s, not (7 . 2), a precision of 0.5 s.
    // Although numerically correct, this generates largish integers.
    // On 64bit systems, this should not matter very much, tho.
    if scale < 0 {
        // T is finite but so large that HZ would be less than 1 if
        // T's precision were represented exactly.  SCALE must be
        // nonnegative, as the (TICKS . HZ) representation requires
        // HZ to be at least 1.  So use SCALE = 0, which converts T to
        // (T . 1), which is the exact numeric value with too-large HZ,
        // which is typically better than signaling overflow.
        scale = 0;
    }

    // Compute TICKS, HZ such that TICKS / HZ exactly equals T, where HZ is
    // T's frequency or 1, whichever is greater.  Here, “frequency” means
    // 1/precision.  Cache HZ values in flt_radix_power.
    let scaled = scalbn(t, scale as i32);
    let ticks = FlexInt::try_from(scaled)?;
    if scaled.fract() != 0.0 {
        return Err(anyhow!("Scaled time is not an integer"));
    }

    // let hz = f64::RADIX.pow(scale as u32);
    let hz = BigInt::from(f64::RADIX).pow(scale as u32);
    Ok(TicksHz { ticks, hz: FlexInt::from(hz) })
}

fn normalize_components(
    high: &FlexInt,
    low: &FlexInt,
    us: i64,
    ps: i64,
) -> (BigInt, i64, i64) {
    // Normalize out-of-range lower-order components by carrying
    // each overflow into the next higher-order component.
    let mut us = us + ps / MILLION - (if ps % MILLION < 0 { 1 } else { 0 });
    let mut s = BigInt::from(us / MILLION - (if us % MILLION < 0 { 1 } else { 0 }));
    s += BigInt::from(low.clone());

    let h: BigInt = BigInt::from(high.clone()) * (1 << LO_TIME_BITS);
    s += h;

    let ps = ps % MILLION + if ps % MILLION < 0 { MILLION } else { 0 };
    us = us % MILLION + if us % MILLION < 0 { MILLION } else { 0 };
    (s, us, ps)
}

fn decode_time_components<'ob>(
    high: ObjectType<'ob>,
    low: ObjectType<'ob>,
    usec: Option<ObjectType<'ob>>,
    psec: Option<ObjectType<'ob>>,
) -> anyhow::Result<TicksHz> {
    match (high, low, usec, psec) {
        // Ticks . Hz format
        (
            ObjectType::Int(_) | ObjectType::BigInt(_),
            ObjectType::Int(_) | ObjectType::BigInt(_),
            None,
            None,
        ) => {
            let high = FlexInt::from(high);
            let low = FlexInt::from(low);
            let (s, _, _) = normalize_components(&high, &low, 0, 0);

            // if low > 0 {
            Ok(TicksHz { ticks: FlexInt::from(s), hz: FlexInt::one() })
            // }
            // return Err(anyhow!("Invalid input".to_string()));
        }

        // TIMEFORM_HI_LO_US
        (
            ObjectType::Int(_) | ObjectType::BigInt(_),
            ObjectType::Int(_) | ObjectType::BigInt(_),
            Some(ObjectType::Int(us)),
            None,
        ) => {
            let high = FlexInt::from(high);
            let low = FlexInt::from(low);

            let (s, us, _) = normalize_components(&high, &low, us, 0);
            let mut ticks: BigInt = us.into();
            ticks += s * MILLION;
            Ok(TicksHz { ticks: FlexInt::from(ticks), hz: FlexInt::from(MILLION) })
        }

        // TIMEFORM_HI_LO_US
        (
            ObjectType::Int(_) | ObjectType::BigInt(_),
            ObjectType::Int(_) | ObjectType::BigInt(_),
            Some(ObjectType::Int(us)),
            Some(ObjectType::Int(ps)),
        ) => {
            let high = FlexInt::from(high);
            let low = FlexInt::from(low);

            let (s, us, ps) = normalize_components(&high, &low, us, ps);
            let mut ticks = BigInt::from(us * MILLION + ps);
            ticks += s * TRILLION;
            Ok(TicksHz { ticks: FlexInt::from(ticks), hz: FlexInt::from(TRILLION) })
        }
        _ => {
            Err(anyhow!("Invalid input".to_string()))
        }
    }

    // let elements = [high, low, usec, psec]
    //     .iter()
    //     .map(|e| {
    //         if let ObjectType::Int(x) = e {
    //             Ok::<i64, String>(*x)
    //         } else {
    //             Err("Invalid type".into())
    //         }
    //     })
    //     .collect::<Result<Vec<_>, _>>()?;
    // if elements.len() != 4 {
    //     return Err("Invalid types".into());
    // }
    // let [high, low, mut us, mut ps] = [elements[0], elements[1], elements[2], elements[3]];

    // /* Normalize out-of-range lower-order components by carrying
    // each overflow into the next higher-order component.  */
    // us += ps / MILLION - (if ps % MILLION < 0 { 1 } else { 0 });
    // let s_from_us_ps = us / MILLION - (if us % MILLION < 0 { 1 } else { 0 });
    // ps = ps % MILLION + if ps % MILLION < 0 { MILLION } else { 0 };
    // us = us % MILLION + if us % MILLION < 0 { MILLION } else { 0 };

    // let iticks = high
    //     .checked_mul(1_i64 << LO_TIME_BITS)
    //     .and_then(|t| t.checked_add(low + s_from_us_ps));

    // if let Some(iticks) = iticks {
    //     let iticks = if hz == TRILLION {
    //         iticks
    //             .checked_mul(MILLION)
    //             .and_then(|ticks| ticks.checked_add(us * MILLION + ps))
    //     } else if hz == MILLION {
    //         iticks.checked_mul(MILLION).and_then(|ticks| ticks.checked_add(us))
    //     } else {
    //         Some(iticks)
    //     };

    //     if let Some(ticks) = iticks {
    //         return Ok(TicksHz { ticks, hz });
    //     }
    // }

    // Err("yikes".into())
}

/* True if the nonzero Lisp integer HZ divides evenly into a trillion.  */
fn trillion_factor(hz: &FlexInt) -> bool {
    FlexInt::from(TRILLION) % hz == FlexInt::zero() 

    //     // This should never happen, but just in case.
    //     return false;
    
    // match hz {
    //     FlexInt::Int(hz) => TRILLION % hz == 0,
    //     FlexInt::Big(hz) => {
    //         // println!("hz: {hz:?}, {:?}", (TRILLION % hz) == BigInt::zero());
    //         (TRILLION % hz) == BigInt::zero()
    //     }
    // }
}

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

#[defun]
fn time_add<'ob>(a: TicksHz, b: TicksHz, cx: &'ob Context) -> Object<'ob> {
    let c = a.arith(b, false).unwrap();
    c.make_lisp_time(cx)
}

#[defun]
fn time_subtract<'ob>(a: TicksHz, b: TicksHz, cx: &'ob Context) -> Object<'ob> {
    let c = a.arith(b, true).unwrap();
    c.make_lisp_time(cx)
}


#[defun]
fn time_less_p(a: TicksHz, b: TicksHz) -> bool {
    a < b 
}

#[defun]
fn time_equal_p(a: TicksHz, b: TicksHz) -> bool {
    a == b 
}

// #[defun]
// #[elprop((i64, i64, i64, i64), (i64, i64, i64, i64))]
// fn float_time<'ob>(a: Option<TicksHz>, cx: &'ob Context) -> Object<'ob> {

// }

// #[defun]
// fn format_time_string()

#[cfg(test)]
mod test {
    use crate::interpreter::assert_lisp;

    

    #[test]
    fn test_time_add() {
        assert_lisp(
            "(time-add '(578 6761 15345 28011) '(26781 8219 56413 24174))",
            "(27359 14980 71758 52185) ",
        );
        assert_lisp(
            "(time-add '(33046 5079 16386 46222) '(18016 34998 64648 3816))", //
            "(51062 40077 81034 50038)",
        );
        assert_lisp(
            "(time-add '(9167576206 6710658493503858 -31779043297708029 -26662356424297108) '(1751444559472681 -15450799159767784 -1045009584567109 -24549925996544854))",
            "(1751320362585227 53739 442441 158038)",
        );

        assert_lisp(
            "(time-add '(15202220679034998 19708391187959224 -13388414145485146 -34355959522033431) '(18617299152078542 3993087375474083 -13497200889589807 9243072305767880))",
            "(33819881486565166 31623 37830 734449)",
        );
        assert_lisp(
            "(time-add '(-9678716954026154 32040421007773903 18973386878378951 9374029063752645) '(9678089278379988 8645004998381716 -17322646841904096 -21270344831773934))",
            "(-6865287633 65071 159086 978711)",
        );

        assert_lisp(
            "(time-add '(-32355606475210398 15197760291358211 9976952979818759) '(11450241568845096 -14345038232468488 -15873143985443383))",
            "(-20905351894949246 52701 375376 0)"
        );

        assert_lisp(
            "(time-add '(16295676417614101 17368090104052081) '(-23668306398921068 7682917901861187))",
            "-483147627446927476044");

        // assert_lisp(
        //     "(time-add 14216861745549231102481053777920.0 -1017485722238583104829273024661169523243513882041182093510023188693686446790203400370539799296574903079051900245635938385920.0)",
        //     "(-1.0174857222385831e+123)"
        // );

        assert_lisp(
            "(time-add -0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001965852055000259 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010611187425753776)",
            "(-3978928856008595410489704739030624849591132114652307856791887789998671307017287597626378349269974641968600247031874992336196419439842935157270144975945397573566026872084921444224515383393932307 . 202402253307310618352495346718917307049556649764142118356901358027430339567995346891960383701437124495187077864316811911389808737385793476867013399940738509921517424276566361364466907742093216341239767678472745068562007483424692698618103355649159556340810056512358769552333414615230502532186327508646006263307707741093494784)",
        );

        assert_lisp(
            "(time-add -0.0 1931410850261338.8)", //
            "(29470990757 10586 750000 0)",
        );
        assert_lisp(
            "(time-add -0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007508620299460154 -0.000000000000000000000000000000000000000000000000000000000000000000034104408151173395)",
            "(-1872958890210515318531787335350717704559250187681278850651348449560086292515745048455431365 . 54918381281044877719855206392651145738155482401146443275155707673484345467181248416980477125291636439818370491131846864296975903997733150500592226328920457216)",
        );

        // (time-add '(33046 5079 16386 46222) '(18016 34998 64648 3816))
        // (3346439309081034050038 . 1000000000000)
    }
}
