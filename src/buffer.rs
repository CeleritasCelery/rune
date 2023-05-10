use crate::{
    core::{
        error::{Type, TypeError},
        gc::Context,
        object::{Buffer, GcObj, Object, TagType},
    },
    hashmap::HashMap,
};
use anyhow::Result;
use fn_macros::defun;
use lazy_static::lazy_static;
use std::sync::Mutex;

// static hashmap containing all the buffers
lazy_static! {
    pub(crate) static ref BUFFERS: Mutex<HashMap<String, &'static Buffer>> =
        Mutex::new(HashMap::default());
}

#[defun]
fn set_buffer(buffer_or_name: GcObj) -> GcObj {
    // TODO: implement
    buffer_or_name
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
            let name: &str = x.try_into()?;
            match BUFFERS.lock().unwrap().get(name) {
                Some(x) => Ok(cx.bind(Object::Buffer(x).tag())),
                None => todo!("create a new buffer"),
            }
        }
        Object::Buffer(_) => Ok(buffer_or_name),
        other => Err(TypeError::new(Type::String, other).into()),
    }
}
