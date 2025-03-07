use crate::{
    core::{
        cons::Cons,
        env::Env,
        error::{Type, TypeError},
        gc::{Context, Rt, Slot},
        object::{Gc, ListType, NIL, Object, ObjectType, WithLifetime},
    },
    fns::eq,
    intervals::textget,
};
use anyhow::{Result, anyhow, bail};
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
            ObjectType::String(_str) => {
                todo!()
            }
            _ => {
                bail!(TypeError::new(Type::BufferOrString, obj))
            }
        }
    };
    let a = tree.find(position).map(|a| *a.val).unwrap_or(NIL);
    Ok(unsafe { a.with_lifetime() })
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
    textget(props, prop)
}

#[defun]
#[allow(unused)]
pub fn get_char_property_and_overlay<'ob>(
    position: usize,
    prop: Object<'ob>,
    object: Object<'ob>,
    env: &'ob mut Rt<Env>,
) -> Result<Object<'ob>> {
    todo!()
}

#[defun]
#[allow(unused)]
pub fn get_char_property<'ob>(
    position: usize,
    prop: Object<'ob>,
    object: Object<'ob>,
    env: &'ob mut Rt<Env>,
) -> Result<Object<'ob>> {
    todo!()
}

// TODO also missing `next-char-property-change` and 3 other similar functions.

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
    modify_buffer_data(object, env, |data| -> Result<Object<'ob>> {
        let point_max = data.text.len_chars() + 1;
        let end = limit.unwrap_or(point_max);
        let tree = data.textprops_with_lifetime();
        // NOTE this can be optimized
        tree.clean();
        let prop = tree.tree.find_intersect_min(position..end);

        match prop {
            Some(p) => {
                let range = p.key;
                if range.start <= position {
                    // should have range.end > position
                    if range.end < end { Ok(cx.add(range.end)) } else { Ok(cx.add(limit)) }
                } else {
                    // empty property before interval, so prop changed just at range start
                    Ok(cx.add(range.start))
                }

                // range is fully inside region, prop changes at end
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

/// Return the position of next property change for a specific property.
/// Scans characters forward from POSITION till it finds
/// a change in the PROP property, then returns the position of the change.
/// If the optional third argument OBJECT is a buffer (or nil, which means
/// the current buffer), POSITION is a buffer position (integer or marker).
/// If OBJECT is a string, POSITION is a 0-based index into it.
/// The property values are compared with `eq'.
/// Return nil if LIMIT is nil or omitted, and the property is constant all
/// the way to the end of OBJECT; if the value is non-nil, it is a position
/// greater than POSITION, never equal.
///
/// If the optional fourth argument LIMIT is non-nil, don't search
/// past position LIMIT; return LIMIT if nothing is found before LIMIT.
#[defun]
pub fn next_single_property_change<'ob>(
    position: usize,
    prop: Object<'ob>,
    object: Object<'ob>,
    limit: Option<usize>,
    env: &'ob mut Rt<Env>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    modify_buffer_data(object, env, |data| -> Result<Object<'ob>> {
        let point_max = data.text.len_chars() + 1;
        let end = limit.unwrap_or(point_max);
        let tree = data.textprops_with_lifetime();
        // NOTE this can be optimized
        tree.clean();
        let iter = tree.iter(position, end);

        let mut val = None;
        for (interval, props) in iter {
            let text_prop = textget(props, prop)?;
            match val {
                Some(v) => {
                    if eq(v, text_prop) {
                        continue;
                    } else {
                        return Ok(cx.add(interval.start));
                    }
                }
                None => val = Some(text_prop),
            }
        }
        Ok(cx.add(limit))
    })
}

/// Return the position of previous property change.
/// Scans characters backwards from POSITION in OBJECT till it finds
/// a change in some text property, then returns the position of the change.
/// If the optional second argument OBJECT is a buffer (or nil, which means
/// the current buffer), POSITION is a buffer position (integer or marker).
/// If OBJECT is a string, POSITION is a 0-based index into it.
/// Return nil if LIMIT is nil or omitted, and the property is constant all
/// the way to the start of OBJECT; if the value is non-nil, it is a position
/// less than POSITION, never equal.
///
/// If the optional third argument LIMIT is non-nil, don't search
/// back past position LIMIT; return LIMIT if nothing is found until LIMIT.
#[defun]
pub fn previous_property_change<'ob>(
    position: usize,
    object: Object<'ob>,
    limit: Option<usize>,
    env: &'ob mut Rt<Env>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    modify_buffer_data(object, env, |data| -> Result<Object<'ob>> {
        let point_min = 1;
        let start = limit.unwrap_or(point_min);
        let end = position;
        let tree = data.textprops_with_lifetime();
        // NOTE this can be optimized
        tree.clean();
        let prop = tree.tree.find_intersect_max(start..end);

        match prop {
            Some(p) => {
                let interval = p.key;
                if interval.end >= position {
                    // should have range.start < position
                    if interval.start > start {
                        Ok(cx.add(interval.start))
                    } else {
                        Ok(cx.add(limit))
                    }
                } else {
                    // empty property after interval, so prop changed just at its end
                    Ok(cx.add(interval.end))
                }

                // range is fully inside region, prop changes at end
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

/// Return the position of next property change for a specific property.
/// Scans characters forward from POSITION till it finds
/// a change in the PROP property, then returns the position of the change.
/// If the optional third argument OBJECT is a buffer (or nil, which means
/// the current buffer), POSITION is a buffer position (integer or marker).
/// If OBJECT is a string, POSITION is a 0-based index into it.
/// The property values are compared with `eq'.
/// Return nil if LIMIT is nil or omitted, and the property is constant all
/// the way to the end of OBJECT; if the value is non-nil, it is a position
/// greater than POSITION, never equal.
///
/// If the optional fourth argument LIMIT is non-nil, don't search
/// past position LIMIT; return LIMIT if nothing is found before LIMIT.
#[defun]
pub fn previous_single_property_change<'ob>(
    position: usize,
    prop: Object<'ob>,
    object: Object<'ob>,
    limit: Option<usize>,
    env: &'ob mut Rt<Env>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    modify_buffer_data(object, env, |data| -> Result<Object<'ob>> {
        let point_min = 1;
        let start = limit.unwrap_or(point_min);
        let tree = data.textprops_with_lifetime();
        // NOTE this can be optimized
        tree.clean();
        let iter = tree.iter_reverse(start, position);

        let mut val = None;
        for (interval, props) in iter {
            let text_prop = textget(props, prop)?;
            match val {
                Some(v) => {
                    if eq(v, text_prop) {
                        continue;
                    } else {
                        return Ok(cx.add(interval.start));
                    }
                }
                None => val = Some(text_prop),
            }
        }
        Ok(cx.add(limit))
    })
}

/// Completely replace properties of text from START to END.
/// The third argument PROPERTIES is the new property list.
/// If the optional fourth argument OBJECT is a buffer (or nil, which means
/// the current buffer), START and END are buffer positions (integers or
/// markers).  If OBJECT is a string, START and END are 0-based indices into it.
/// If PROPERTIES is nil, the effect is to remove all properties from
/// the designated part of OBJECT.
#[defun]
pub fn set_text_properties<'ob>(
    start: usize,
    end: usize,
    properties: Object<'ob>,
    object: Object<'ob>,
    env: &mut Rt<Env>,
) -> Result<()> {
    modify_buffer_data(object, env, |data| -> Result<()> {
        let tree = data.textprops_with_lifetime();
        tree.set_properties(start, end, properties);
        Ok(())
    })
}

/// Remove some properties from text from START to END.
/// The third argument PROPERTIES is a property list
/// whose property names specify the properties to remove.
/// \(The values stored in PROPERTIES are ignored.)
/// If the optional fourth argument OBJECT is a buffer (or nil, which means
/// the current buffer), START and END are buffer positions (integers or
/// markers).  If OBJECT is a string, START and END are 0-based indices into it.
/// Return t if any property was actually removed, nil otherwise.
///
/// Use `set-text-properties' if you want to remove all text properties.
#[defun]
pub fn remove_text_properties<'ob>(
    start: usize,
    end: usize,
    properties: Object<'ob>,
    object: Object<'ob>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<()> {
    modify_buffer_data(object, env, |data| -> Result<()> {
        let tree = data.textprops_with_lifetime();
        tree.delete(start, end, list![properties; cx])
    })
}

/// /* Remove some properties from text from START to END.
/// The third argument LIST-OF-PROPERTIES is a list of property names to remove.
/// If the optional fourth argument OBJECT is a buffer (or nil, which means
/// the current buffer), START and END are buffer positions (integers or
/// markers).  If OBJECT is a string, START and END are 0-based indices into it.
/// Return t if any property was actually removed, nil otherwise.
#[defun]
pub fn remove_list_of_text_properties<'ob>(
    start: usize,
    end: usize,
    list_of_properties: Object<'ob>,
    object: Object<'ob>,
    env: &mut Rt<Env>,
) -> Result<()> {
    modify_buffer_data(object, env, |data| -> Result<()> {
        let tree = data.textprops_with_lifetime();
        tree.delete(start, end, list_of_properties)
    })
}

/// Check text from START to END for property PROPERTY equaling VALUE.
/// If so, return the position of the first character whose property PROPERTY
/// is `eq' to VALUE.  Otherwise return nil.
/// If the optional fifth argument OBJECT is a buffer (or nil, which means
/// the current buffer), START and END are buffer positions (integers or
/// markers).  If OBJECT is a string, START and END are 0-based indices into it.
#[defun]
pub fn text_properties_any<'ob>(
    start: usize,
    end: usize,
    property: Object<'ob>,
    value: Object<'ob>,
    object: Object<'ob>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    modify_buffer_data(object, env, |data| -> Result<Object<'ob>> {
        let tree = data.textprops_with_lifetime();
        let iter = tree.iter(start, end);
        for (interval, props) in iter {
            let val = textget(props, property)?;
            if !eq(val, value) {
                return Ok(cx.add(interval.start));
            }
        }
        Ok(NIL)
    })
}

/// Check text from START to END for property PROPERTY not equaling VALUE.
/// If so, return the position of the first character whose property PROPERTY
/// is not `eq' to VALUE.  Otherwise, return nil.
/// If the optional fifth argument OBJECT is a buffer (or nil, which means
/// the current buffer), START and END are buffer positions (integers or
/// markers).  If OBJECT is a string, START and END are 0-based indices into it.
#[defun]
pub fn text_properties_not_all<'ob>(
    start: usize,
    end: usize,
    property: Object<'ob>,
    value: Object<'ob>,
    object: Object<'ob>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    modify_buffer_data(object, env, |data| -> Result<Object<'ob>> {
        let tree = data.textprops_with_lifetime();
        let iter = tree.iter(start, end);
        for (interval, props) in iter {
            let val = textget(props, property)?;
            if eq(val, value) {
                return Ok(cx.add(interval.start));
            }
        }
        Ok(NIL)
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        buffer::{BUFFERS, get_buffer_create},
        core::{
            env::intern,
            gc::{Context, RootSet},
        },
        fns::plist_get,
    };
    use rune_core::macros::{list, root};

    use super::*;

    #[test]
    fn test_add_properties() {
        let roots = &RootSet::default();
        let mut context = Context::new(roots);
        let cx = &mut context;
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
    fn test_next_property_change() -> Result<()> {
        let roots = &RootSet::default();
        let mut context = Context::new(roots);
        let cx = &mut context;
        root!(env, new(Env), cx);

        let buf = get_buffer_create(cx.add("test_next_property_change"), None, cx)?;
        if let ObjectType::Buffer(b) = buf.untag() {
            b.lock()
                .unwrap()
                .get_mut()
                .text
                .insert("lorem ipsum quia dolor sit amet, consectetur, adipisci velit.");
        }

        let a = intern(":a", cx);
        let a = cx.add(a);

        // Add properties at different ranges
        put_text_property(0, 5, a, cx.add(1), buf, env, cx)?;
        put_text_property(5, 10, a, cx.add(2), buf, env, cx)?;
        put_text_property(15, 20, a, cx.add(3), buf, env, cx)?;

        // Test property changes
        let change = next_property_change(0, buf, None, env, cx)?;
        assert_eq!(change, cx.add(5)); // Change at end of first range

        let change = next_property_change(5, buf, None, env, cx)?;
        assert_eq!(change, cx.add(10)); // Change at end of second range

        let change = next_property_change(10, buf, None, env, cx)?;
        assert_eq!(change, cx.add(15)); // Change at start of third range

        let change = next_property_change(15, buf, None, env, cx)?;
        assert_eq!(change, cx.add(20)); // Change at end of third range

        // Test with limit
        let change = next_property_change(0, buf, Some(8), env, cx)?;
        assert_eq!(change, cx.add(5)); // Change within limit

        let change = next_property_change(5, buf, Some(8), env, cx)?;
        assert_eq!(change, cx.add(8)); // Limit reached

        // Test no change case
        let change = next_property_change(20, buf, None, env, cx)?;
        assert!(change.is_nil()); // No changes after last property

        BUFFERS.lock().unwrap().clear();
        Ok(())
    }

    #[test]
    fn test_next_single_property_change() -> Result<()> {
        let roots = &RootSet::default();
        let mut context = Context::new(roots);
        let cx = &mut context;
        root!(env, new(Env), cx);

        let buf = get_buffer_create(cx.add("test_next_single_property_change"), None, cx)?;
        if let ObjectType::Buffer(b) = buf.untag() {
            b.lock()
                .unwrap()
                .get_mut()
                .text
                .insert("lorem ipsum quia dolor sit amet, consectetur, adipisci velit.");
        }

        let a = intern(":a", cx);
        let a = cx.add(a);
        let b = intern(":b", cx);
        let b = cx.add(b);

        // Add properties at different ranges
        put_text_property(1, 5, a, cx.add(1), buf, env, cx)?;
        put_text_property(5, 8, a, cx.add(2), buf, env, cx)?;
        put_text_property(10, 15, b, cx.add(4), buf, env, cx)?;
        put_text_property(15, 20, a, cx.add(3), buf, env, cx)?;

        // Test property changes for :a
        let change = next_single_property_change(1, a, buf, None, env, cx)?;
        assert_eq!(change, cx.add(5)); // Change at end of first range

        let change = next_single_property_change(5, a, buf, None, env, cx)?;
        assert_eq!(change, cx.add(8)); // Change at end of second range

        let change = next_single_property_change(10, a, buf, None, env, cx)?;
        assert_eq!(change, cx.add(15)); // Change at start of third range

        let change = next_single_property_change(15, a, buf, None, env, cx)?;
        assert_eq!(change, cx.add(20)); // Change at end of third range

        // Test with limit
        let change = next_single_property_change(1, a, buf, Some(8), env, cx)?;
        assert_eq!(change, cx.add(5)); // Change within limit

        let change = next_single_property_change(5, a, buf, Some(8), env, cx)?;
        assert_eq!(change, cx.add(8)); // Limit reached

        // Test no change case
        let change = next_single_property_change(20, a, buf, None, env, cx)?;
        assert!(change.is_nil()); // No changes after last property

        // Test property changes for :b
        let change = next_single_property_change(1, b, buf, None, env, cx)?;
        assert_eq!(change, cx.add(10)); // Change at start of b range

        let change = next_single_property_change(10, b, buf, None, env, cx)?;
        assert_eq!(change, cx.add(15)); // Change at end of b range

        BUFFERS.lock().unwrap().clear();
        Ok(())
    }

    #[test]
    fn test_remove_text_properties() -> Result<()> {
        let roots = &RootSet::default();
        let mut context = Context::new(roots);
        let cx = &mut context;
        root!(env, new(Env), cx);

        let buf = get_buffer_create(cx.add("test_remove_text_properties"), None, cx)?;
        if let ObjectType::Buffer(b) = buf.untag() {
            b.lock().unwrap().get_mut().text.insert("test text");
        }

        let a = intern(":a", cx);
        let a = cx.add(a);
        let b = intern(":b", cx);
        let b = cx.add(b);

        // Add properties
        put_text_property(1, 5, a, cx.add(1), buf, env, cx)?;
        put_text_property(5, 10, b, cx.add(2), buf, env, cx)?;

        // Remove property :a
        remove_text_properties(1, 10, a, buf, env, cx)?;

        // Verify :a was removed
        let props = text_properties_at(3, buf, env)?;
        assert!(props.is_nil());

        // Verify :b remains
        let props = text_properties_at(5, buf, env)?;
        let val = plist_get(props, b)?;
        assert_eq!(val, cx.add(2));

        // Try removing non-existent property
        remove_text_properties(0, 10, a, buf, env, cx)?;

        BUFFERS.lock().unwrap().clear();
        Ok(())
    }

    #[test]
    fn test_text_properties_at() -> Result<()> {
        let roots = &RootSet::default();
        let mut context = Context::new(roots);
        let cx = &mut context;
        root!(env, new(Env), cx);

        let buf = get_buffer_create(cx.add("test_text_properties_at"), None, cx)?;
        let n = text_properties_at(0, buf, env)?;
        assert!(n.is_nil());

        let a = intern(":a", cx);
        let a = cx.add(a);
        put_text_property(0, 1, a, cx.add(3), buf, env, cx)?;
        let n = text_properties_at(0, buf, env)?;
        let val = plist_get(n, a)?;
        assert!(eq(val, cx.add(3)));

        BUFFERS.lock().unwrap().clear();
        Ok(())
    }
}
