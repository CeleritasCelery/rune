use crate::{
    core::{
        env::{Env, INTERNED_SYMBOLS},
        error::{Type, TypeError},
        gc::{Context, Rt},
        object::{GcObj, LispBuffer, Object},
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
fn set_buffer<'ob>(
    buffer_or_name: GcObj<'ob>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    let buffer: &LispBuffer = match buffer_or_name.untag() {
        Object::Buffer(b) => b,
        Object::String(s) => {
            let name: &str = s.try_into()?;
            let buffer_list = BUFFERS.lock().unwrap();
            let Some(buffer) = buffer_list.get(name) else {
                bail!("No buffer named {}", name);
            };
            cx.bind(*buffer)
        }
        x => bail!(TypeError::new(Type::String, x)),
    };
    env.set_buffer(buffer, cx)?;
    Ok(cx.add(buffer))
}

#[defun]
fn set_buffer_modified_p(flag: GcObj) -> GcObj {
    // TODO: implement
    flag
}

#[defun]
fn get_buffer_create<'ob>(
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

#[cfg(test)]
mod test {
    use crate::core::env::sym;
    use crate::core::gc::RootSet;

    use super::*;

    #[test]
    fn test_create_buffer() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let buffer = get_buffer_create(cx.add("test_create_buffer"), sym::NIL.into(), cx).unwrap();
        assert!(matches!(buffer.untag(), Object::Buffer(_)));
    }
}
