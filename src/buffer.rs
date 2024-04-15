//! Buffer operations.
use crate::{
    core::{
        env::{interned_symbols, Env},
        error::{Type, TypeError},
        gc::{Context, Rt},
        object::{Gc, LispBuffer, Object, ObjectType, NIL},
    },
    fns::slice_into_list,
};
use anyhow::{bail, Result};
use rune_core::hashmap::HashMap;
use rune_macros::defun;
use std::sync::Mutex;
use std::sync::OnceLock;

// static hashmap containing all the buffers
static BUFFERS: OnceLock<Mutex<HashMap<String, &'static LispBuffer>>> = OnceLock::new();

/// Helper function to avoid calling `get_or_init` on each of the calls to `lock()` on the Mutex.
///
/// TODO: Use `LazyLock`: <https://github.com/CeleritasCelery/rune/issues/34>
fn buffers() -> &'static Mutex<HashMap<String, &'static LispBuffer>> {
    BUFFERS.get_or_init(Mutex::default)
}

#[defun]
pub(crate) fn set_buffer<'ob>(
    buffer_or_name: Object<'ob>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    let buffer = resolve_buffer(buffer_or_name, cx)?;
    env.set_buffer(buffer)?;
    Ok(cx.add(buffer))
}

fn resolve_buffer<'ob>(buffer_or_name: Object, cx: &'ob Context) -> Result<&'ob LispBuffer> {
    match buffer_or_name.untag() {
        ObjectType::Buffer(b) => Ok(b),
        ObjectType::String(name) => {
            let buffer_list = buffers().lock().unwrap();
            let Some(buffer) = buffer_list.get(name.as_ref()) else {
                bail!("No buffer named {}", name);
            };
            Ok(cx.bind(*buffer))
        }
        x => Err(TypeError::new(Type::String, x).into()),
    }
}

#[defun]
fn set_buffer_modified_p(flag: Object) -> Object {
    // TODO: implement
    flag
}

#[defun]
fn buffer_live_p(buffer: Object, env: &Rt<Env>) -> bool {
    match buffer.untag() {
        ObjectType::Buffer(b) => env.with_buffer(Some(b), |_| {}).is_some(),
        _ => false,
    }
}

#[defun]
fn buffer_name(buffer: Option<Gc<&LispBuffer>>, env: &Rt<Env>) -> Option<String> {
    env.with_buffer(buffer.map(Gc::untag), |b| b.name.to_string())
}

#[defun]
pub(crate) fn get_buffer_create<'ob>(
    buffer_or_name: Object<'ob>,
    _inhibit_buffer_hooks: Option<Object>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    match buffer_or_name.untag() {
        ObjectType::String(name) => {
            let mut buffer_list = buffers().lock().unwrap();
            match buffer_list.get(name.as_ref()) {
                Some(b) => Ok(cx.add(*b)),
                None => {
                    // If not already in the global buffer list, create a new
                    // buffer and add it
                    let buffer: &'static _ = {
                        let global = interned_symbols().lock().unwrap();
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
        ObjectType::Buffer(_) => Ok(buffer_or_name),
        other => Err(TypeError::new(Type::BufferOrName, other).into()),
    }
}

#[defun]
pub(crate) fn get_buffer<'ob>(
    buffer_or_name: Object<'ob>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    match buffer_or_name.untag() {
        ObjectType::String(name) => {
            let buffer_list = buffers().lock().unwrap();
            match buffer_list.get(name.as_ref()) {
                Some(b) => Ok(cx.add(*b)),
                None => Ok(NIL),
            }
        }
        ObjectType::Buffer(_) => Ok(buffer_or_name),
        other => Err(TypeError::new(Type::BufferOrName, other).into()),
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
    let buffer_list = buffers().lock().unwrap();
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
fn kill_buffer(buffer_or_name: Option<Object>, cx: &Context, env: &mut Rt<Env>) -> bool {
    let buffer = match buffer_or_name {
        Some(buffer) => match resolve_buffer(buffer, cx) {
            Ok(b) => Some(b),
            Err(_) => return false,
        },
        None => None,
    };
    // todo, we need to select a new buffer
    #[allow(clippy::redundant_closure_for_method_calls)]
    let state = env.with_buffer_mut(buffer, |b| b.kill()).unwrap_or(false);
    env.current_buffer = None;
    state
}

#[defun]
fn buffer_base_buffer(_buffer: Option<()>) -> bool {
    // TODO: implement indirect buffers
    false
}

#[defun]
fn get_file_buffer(_filename: &str) -> bool {
    // TODO: implement file buffers
    false
}

#[defun]
fn buffer_list<'ob>(_frame: Option<()>, cx: &'ob Context) -> Object<'ob> {
    // TODO: implement frame parameter
    // TODO: remove this temp vector
    let mut buffer_list: Vec<Object> = Vec::new();
    for buffer in buffers().lock().unwrap().values() {
        buffer_list.push(cx.add(*buffer));
    }
    slice_into_list(&buffer_list, None, cx)
}

// TODO: buffer local
defvar!(FILL_COLUMN, 70);
defvar!(INDENT_TABS_MODE);
defvar!(LEFT_MARGIN, 0);
defvar!(INHIBIT_COMPACTING_FONT_CACHES);
defvar!(NO_UPDATE_AUTOLOADS);
defvar!(TAB_WIDTH, 8);
defvar!(TRUNCATE_LINES);
defvar!(WORD_WRAP);
defvar!(BIDI_DISPLAY_REORDERING);
defvar!(BUFFER_FILE_NAME);

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::gc::RootSet;

    #[test]
    fn test_gen_new_buffer_name() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);

        let name = "gen_buffer_test";
        let new_name = generate_new_buffer_name(name, None);
        assert_eq!(new_name, "gen_buffer_test");

        get_buffer_create(cx.add(name), Some(NIL), cx).unwrap();
        let new_name = generate_new_buffer_name(name, None);
        assert_eq!(new_name, "gen_buffer_test<2>");

        get_buffer_create(cx.add("gen_buffer_test<2>"), Some(NIL), cx).unwrap();
        let new_name = generate_new_buffer_name(name, None);
        assert_eq!(new_name, "gen_buffer_test<3>");

        let new_name = generate_new_buffer_name(name, Some("gen_buffer_test<2>"));
        assert_eq!(new_name, "gen_buffer_test<2>");

        let new_name = generate_new_buffer_name(" gen_buffer_test", None);
        assert_eq!(new_name, " gen_buffer_test");

        get_buffer_create(cx.add(" gen_buffer_test"), Some(NIL), cx).unwrap();
        let new_name = generate_new_buffer_name(" gen_buffer_test", None);
        assert!(new_name.starts_with(" gen_buffer_test-"));
    }

    #[test]
    fn test_create_buffer() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let buffer = get_buffer_create(cx.add("test_create_buffer"), Some(NIL), cx).unwrap();
        assert!(matches!(buffer.untag(), ObjectType::Buffer(_)));
    }
}
