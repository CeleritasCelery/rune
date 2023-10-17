use crate::{
    core::{
        env::{Env, INTERNED_SYMBOLS},
        error::{Type, TypeError},
        gc::{Context, Rt},
        object::{Gc, GcObj, LispBuffer, Object},
    },
    hashmap::HashMap,
};
use anyhow::{bail, Result};
use fn_macros::defun;
use lazy_static::lazy_static;
use std::sync::Mutex;

// static hashmap containing all the buffers
lazy_static! {
    static ref BUFFERS: Mutex<HashMap<String, &'static LispBuffer>> =
        Mutex::new(HashMap::default());
}

#[defun]
pub(crate) fn set_buffer<'ob>(
    buffer_or_name: GcObj<'ob>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    let buffer = resolve_buffer(buffer_or_name, cx)?;
    env.set_buffer(buffer)?;
    Ok(cx.add(buffer))
}

fn resolve_buffer<'ob>(buffer_or_name: GcObj, cx: &'ob Context) -> Result<&'ob LispBuffer> {
    match buffer_or_name.untag() {
        Object::Buffer(b) => Ok(b),
        Object::String(s) => {
            let name: &str = s.try_into()?;
            let buffer_list = BUFFERS.lock().unwrap();
            let Some(buffer) = buffer_list.get(name) else {
                bail!("No buffer named {}", name);
            };
            Ok(cx.bind(*buffer))
        }
        x => Err(TypeError::new(Type::String, x).into()),
    }
}

#[defun]
fn set_buffer_modified_p(flag: GcObj) -> GcObj {
    // TODO: implement
    flag
}

#[defun]
fn buffer_live_p(buffer: GcObj, env: &Rt<Env>) -> bool {
    match buffer.untag() {
        Object::Buffer(b) => env.with_buffer(Some(b), |_| {}).is_some(),
        _ => false,
    }
}

#[defun]
fn buffer_name(buffer: Option<Gc<&LispBuffer>>, env: &Rt<Env>) -> Option<String> {
    env.with_buffer(buffer.map(Gc::untag), |b| b.name.to_string())
}

#[defun]
pub(crate) fn get_buffer_create<'ob>(
    buffer_or_name: GcObj<'ob>,
    _inhibit_buffer_hooks: GcObj,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    match buffer_or_name.untag() {
        Object::String(x) => {
            let name = x.try_into()?;
            let mut buffer_list = BUFFERS.lock().unwrap();
            match buffer_list.get(name) {
                Some(b) => Ok(cx.add(*b)),
                None => {
                    // If not already in the global buffer list, create a new
                    // buffer and add it
                    let buffer: &'static _ = {
                        let global = INTERNED_SYMBOLS.lock().unwrap();
                        let buffer = global.create_buffer(name);
                        // SAFETY: This can be 'static because it is stored in the
                        // global block. Eventually it will be garbage collected
                        unsafe { &*(buffer as *const LispBuffer) }
                    };
                    buffer_list.insert(name.to_string(), buffer);
                    Ok(cx.add(buffer))
                }
            }
        }
        Object::Buffer(_) => Ok(buffer_or_name),
        other => Err(TypeError::new(Type::String, other).into()),
    }
}

/// Return a string that is the name of no existing buffer based on NAME.
///
/// If there is no live buffer named NAME, then return NAME.
/// Otherwise modify name by appending \<NUMBER\>, incrementing NUMBER
/// (starting at 2) until an unused name is found, and then return that name.
/// Optional second argument IGNORE specifies a name that is okay to use (if
/// it is in the sequence to be tried) even if a buffer with that name exists.
///
/// If NAME begins with a space (i.e., a buffer that is not normally
/// visible to users), then if buffer NAME already exists a random number
/// is first appended to NAME, to speed up finding a non-existent buffer.
#[defun]
fn generate_new_buffer_name(name: &str, ignore: Option<&str>) -> String {
    // check if the name exists
    let buffer_list = BUFFERS.lock().unwrap();
    let valid_name =
        |name: &str| ignore.is_some_and(|x| x == name) || !buffer_list.contains_key(name);

    let mut new_name = name.to_string();
    let mut number = 2;

    while !valid_name(&new_name) {
        if name.starts_with(' ') {
            // use rand to find uniq names faster
            let rand = rand::random::<u32>();
            new_name = format!("{name}-{rand}");
        } else {
            new_name = format!("{name}<{number}>");
            number += 1;
        }
    }
    new_name
}

#[defun]
fn kill_buffer(buffer_or_name: Option<GcObj>, cx: &Context, env: &mut Rt<Env>) -> bool {
    let buffer = match buffer_or_name {
        Some(buffer) => match resolve_buffer(buffer, cx) {
            Ok(b) => Some(b),
            Err(_) => return false,
        },
        None => None,
    };
    // todo, we need to select a new buffer
    #[allow(clippy::redundant_closure_for_method_calls)]
    env.with_buffer_mut(buffer, |b| b.kill()).unwrap_or(false)
}

#[cfg(test)]
mod test {
    use crate::core::env::sym;
    use crate::core::gc::RootSet;
    use crate::core::object::nil;

    use super::*;

    #[test]
    fn test_gen_new_buffer_name() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);

        let name = "gen_buffer_test";
        let new_name = generate_new_buffer_name(name, None);
        assert_eq!(new_name, "gen_buffer_test");

        get_buffer_create(cx.add(name), nil(), cx).unwrap();
        let new_name = generate_new_buffer_name(name, None);
        assert_eq!(new_name, "gen_buffer_test<2>");

        get_buffer_create(cx.add("gen_buffer_test<2>"), nil(), cx).unwrap();
        let new_name = generate_new_buffer_name(name, None);
        assert_eq!(new_name, "gen_buffer_test<3>");

        let new_name = generate_new_buffer_name(name, Some("gen_buffer_test<2>"));
        assert_eq!(new_name, "gen_buffer_test<2>");

        let new_name = generate_new_buffer_name(" gen_buffer_test", None);
        assert_eq!(new_name, " gen_buffer_test");

        get_buffer_create(cx.add(" gen_buffer_test"), nil(), cx).unwrap();
        let new_name = generate_new_buffer_name(" gen_buffer_test", None);
        assert!(new_name.starts_with(" gen_buffer_test-"));
    }

    #[test]
    fn test_create_buffer() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let buffer = get_buffer_create(cx.add("test_create_buffer"), sym::NIL.into(), cx).unwrap();
        assert!(matches!(buffer.untag(), Object::Buffer(_)));
    }
}
