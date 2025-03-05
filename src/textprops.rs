use crate::{
    core::{
        cons::Cons,
        env::Env,
        error::{Type, TypeError},
        gc::{Context, Rt, Slot},
        object::{Gc, ListType, Object, ObjectType, WithLifetime, NIL},
    },
    editfns::point_max,
    fns::{eq, plist_get},
};
use anyhow::{anyhow, bail, Result};
use rune_core::macros::list;
use rune_macros::defun;

use crate::core::object::BufferData;

#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PropertySetType {
    Replace,
    Prepend,
    Append,
}

/// Add the properties of PLIST to the interval I, or set
/// the value of I's property to the value of the property on PLIST
/// if they are different.
///
/// OBJECT should be the string or buffer the interval is in.
///
/// If DESTRUCTIVE, the function is allowed to reuse list values in the
/// properties.
///
/// Return true if this changes I (i.e., if any members of PLIST
/// are actually added to I's plist)
pub fn add_properties<'ob>(
    plist: Object<'ob>,
    mut obj_i: Object<'ob>,
    _set_type: PropertySetType,
    _destructive: bool,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    // TODO return type
    let Ok(plist) = Gc::<ListType>::try_from(plist) else { return Ok(obj_i) };
    let Ok(plist_i) = Gc::<ListType>::try_from(obj_i) else { return Ok(obj_i) };
    let mut iter = plist.elements();
    // iterate through plist, finding key1 and val1
    while let Some(key1) = iter.next() {
        let key1 = key1?;
        let Some(val1) = iter.next() else { return Ok(obj_i) };
        let mut found = false;

        let mut iter_i = plist_i.conses();
        // iterate through i's plist, finding (key2, val2) and set val2 if key2 == key1;
        while let Some(key2_cons) = iter_i.next() {
            let Some(val2_cons) = iter_i.next() else { return Ok(obj_i) };
            if eq(key1, key2_cons?.car()) {
                // TODO this should depend on set_type
                val2_cons?.set_car(val1?)?;
                found = true;
                break;
            }
        }
        // if no key1 found, append them
        if !found {
            let pl = plist_i.untag();
            let new_cons = Cons::new(key1, Cons::new(val1?, pl, cx), cx);
            obj_i = new_cons.into();
        }
    }
    Ok(obj_i)
}

/// Helper function to safely modify buffer data.
///
/// Takes an object (buffer or nil) and environment, and applies the given function
/// to the buffer's data. Handles both current buffer and other buffers safely.
///
/// # Arguments
/// * `object` - The buffer object to modify (or nil for current buffer)
/// * `env` - The environment containing buffer state
/// * `func` - Function to apply to the buffer's data
///
/// # Returns
/// Result containing the return value from func or an error
fn modify_buffer_data<'ob, T>(
    object: Object<'ob>,
    env: &'ob mut Rt<Env>,
    func: impl FnOnce(&mut BufferData) -> Result<T>,
) -> Result<T> {
    if object.is_nil() {
        let data = env.current_buffer.get_mut();
        func(data)
    } else {
        let current_buf = env.current_buffer.buf_ref;
        if let ObjectType::Buffer(b) = object.untag() {
            if b == current_buf {
                let data = env.current_buffer.get_mut();
                func(data)
            } else {
                let mut open_buf = b.lock()?;
                func(open_buf.get_mut())
            }
        } else {
            Err(anyhow!(TypeError::new(Type::BufferOrString, object.untag())))
        }
    }
}

/// Return the list of properties of the character at POSITION in OBJECT.
/// If the optional second argument OBJECT is a buffer (or nil, which means
/// the current buffer), POSITION is a buffer position (integer or marker).
///
/// If OBJECT is a string, POSITION is a 0-based index into it.
///
/// If POSITION is at the end of OBJECT, the value is nil, but note that
/// buffer narrowing does not affect the value.  That is, if OBJECT is a
/// buffer or nil, and the buffer is narrowed and POSITION is at the end
/// of the narrowed buffer, the result may be non-nil.
///
/// If you want to display the text properties at point in a human-readable
/// form, use the `describe-text-properties' command.
#[defun]
pub fn text_properties_at<'ob>(
    position: usize,
    object: Object<'ob>,
    env: &'ob mut Rt<Env>,
) -> Result<Object<'ob>> {
    let tree = if eq(object, NIL) {
        &env.current_buffer.get().textprops
    } else {
        let obj = object.untag();
        match obj {
            ObjectType::Buffer(buf) => &buf.lock().unwrap().textprops,
            ObjectType::String(str) => {
                todo!()
            }
            _ => {
                bail!(TypeError::new(Type::BufferOrString, obj))
            }
        }
    };
    let a = tree.find(position).map(|a| *a.val).unwrap_or(NIL);
    return Ok(unsafe { a.with_lifetime() });
}

/// Return the value of POSITION's property PROP, in OBJECT.
/// OBJECT should be a buffer or a string; if omitted or nil, it defaults
/// to the current buffer.
///
/// If POSITION is at the end of OBJECT, the value is nil, but note that
/// buffer narrowing does not affect the value.  That is, if the buffer is
/// narrowed and POSITION is at the end of the narrowed buffer, the result
/// may be non-nil.
#[defun]
pub fn get_text_property<'ob>(
    position: usize,
    prop: Object<'ob>,
    object: Object<'ob>,
    env: &'ob mut Rt<Env>,
) -> Result<Object<'ob>> {
    let props = text_properties_at(position, object, env)?;
    // TODO see lookup_char_property, should also lookup
    // 1. category
    // 2. char_property_alias_alist
    // 3.  default_text_properties
    plist_get(props, prop)
}

/// Set one property of the text from START to END.
/// The third and fourth arguments PROPERTY and VALUE
/// specify the property to add.
/// If the optional fifth argument OBJECT is a buffer (or nil, which means
/// the current buffer), START and END are buffer positions (integers or
/// markers).  If OBJECT is a string, START and END are 0-based indices into it.
#[defun]
pub fn put_text_property<'ob>(
    start: usize,
    end: usize,
    property: Object<'ob>,
    value: Object<'ob>,
    object: Object<'ob>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<()> {
    let prop = list!(property, value; cx);
    let prop = Slot::new(prop);
    modify_buffer_data(object, env, |data| {
        let tree = &mut data.textprops_with_lifetime();
        tree.insert(start, end, prop, cx);
        Ok(())
    })
}
/// Return the position of next property change.
/// Scans characters forward from POSITION in OBJECT till it finds
/// a change in some text property, then returns the position of the change.
/// If the optional second argument OBJECT is a buffer (or nil, which means
/// the current buffer), POSITION is a buffer position (integer or marker).
/// If OBJECT is a string, POSITION is a 0-based index into it.
/// Return nil if LIMIT is nil or omitted, and the property is constant all
/// the way to the end of OBJECT; if the value is non-nil, it is a position
/// greater than POSITION, never equal.
///
/// If the optional third argument LIMIT is non-nil, don't search
/// past position LIMIT; return LIMIT if nothing is found before LIMIT.
#[defun]
pub fn next_property_change<'ob>(
    position: usize,
    object: Object<'ob>,
    limit: Option<usize>,
    env: &'ob mut Rt<Env>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    let point_max = point_max(env)?;
    let end = limit.unwrap_or(point_max);
    // let node = tree.find(position);
    modify_buffer_data(object, env, |data| -> Result<Object<'ob>> {
        let tree = data.textprops_with_lifetime();
        let prop = tree.tree.find_intersect_min(position..end);
        match prop {
            Some(p) => {
                let range = p.key;
                // empty property before interval, so prop changed just at range start
                if range.start > position {
                    return Ok(cx.add(position));
                }
                if range.end >= end {
                    return Ok(cx.add(limit));
                }

                // range is fully inside region, prop changes at end
                return Ok(cx.add(range.end));
            }
            None => {
                let result = match limit {
                    Some(n) => cx.add(n),
                    None => NIL,
                };
                Ok(result)
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        buffer::get_buffer_create,
        core::{
            env::intern,
            gc::{Context, RootSet},
        },
        fns::plist_get,
        intervals::IntervalTree,
    };
    use rune_core::macros::{list, root};

    use super::*;

    #[test]
    fn test_add_properties() {
        let roots = &RootSet::default();
        let mut context = Context::new(roots);
        let cx = &mut context;
        // let cons1 = Cons::new("start", Cons::new(7, Cons::new(5, 9, cx), cx), cx);
        let plist_1 = list![intern(":a", cx), 1, intern(":b", cx), 2; cx];
        let plist_2 = list![intern(":a", cx), 4, intern(":c", cx), 5; cx];
        let plist_1 =
            add_properties(plist_2, plist_1, PropertySetType::Replace, false, cx).unwrap();
        let plist_1 = dbg!(plist_1);
        let a = plist_get(plist_1, intern(":a", cx).into()).unwrap();
        let b = plist_get(plist_1, intern(":b", cx).into()).unwrap();
        let c = plist_get(plist_1, intern(":c", cx).into()).unwrap();
        assert_eq!(a, 4);
        assert_eq!(b, 2);
        assert_eq!(c, 5);
    }

    #[test]
    fn test_text_properties_at() -> Result<()> {
        let roots = &RootSet::default();
        let mut context = Context::new(roots);
        let cx = &mut context;
        root!(env, new(Env), cx);

        let buf = get_buffer_create(cx.add("test"), None, cx)?;
        // let cons1 = Cons::new("start", Cons::new(7, Cons::new(5, 9, cx), cx), cx);
        let n = text_properties_at(0, buf, env)?;
        assert!(n.is_nil());

        let a = intern(":a", cx);
        let a = cx.add(a);
        put_text_property(0, 1, a, cx.add(3), buf, env, cx)?;
        let n = text_properties_at(0, buf, env)?;
        let val = plist_get(n, a)?;
        assert!(eq(val, cx.add(3)));
        Ok(())
    }
}
