use crate::{
    core::{
        cons::Cons,
        env::Env,
        gc::{Context, Rt},
        object::{Gc, IntoObject, ListType, Object, ObjectType, NIL, TRUE},
    },
    editfns::point_max,
    fns::{eq, plist_get},
    intervals::IntervalTree,
};
use anyhow::{bail, Result};
use rune_core::macros::list;
use rune_macros::defun;

#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PropertySetType {
    Replace,
    Prepend,
    Append,
}

pub fn buffer_textprops(buffer: Object) {
    
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
) -> Result<(Object<'ob>, bool)> {
    // TODO return type
    let mut changed = false;
    let Ok(plist) = Gc::<ListType>::try_from(plist) else { return Ok((obj_i, false)) };
    let Ok(plist_i) = Gc::<ListType>::try_from(obj_i) else { return Ok((obj_i, false)) };
    let mut iter = plist.elements();
    // iterate through plist, finding key1 and val1
    while let Some(key1) = iter.next() {
        let key1 = key1?;
        let Some(val1) = iter.next() else { return Ok((obj_i, changed)) };
        let mut found = false;

        let mut iter_i = plist_i.conses();
        // iterate through i's plist, finding (key2, val2) and set val2 if key2 == key1;
        while let Some(key2_cons) = iter_i.next() {
            let Some(val2_cons) = iter_i.next() else { return Ok((obj_i, changed)) };
            if eq(key1, key2_cons?.car()) {
                // TODO this should depend on set_type
                val2_cons?.set_car(val1?)?;
                changed = true;
                found = true;
                break;
            }
        }
        // if no key1 found, append them
        if !found {
            let pl = plist_i.untag();
            changed = true;
            let new_cons = Cons::new(key1, Cons::new(val1?, pl, cx), cx);
            obj_i = new_cons.into();
        }
    }
    Ok((obj_i, changed))
}

/// Get the property interval tree for OBJECT. OBJECT is a buffer or string.
/// If OBJECT is NIL, return current buffer's interval tree.
fn prop_tree_for_object<'ob>(
    object: Object<'ob>,
    env: &'ob mut Rt<Env>,
    cx: &'ob Context,
) -> Result<&'ob mut IntervalTree<'ob>> {
    let obj = if object.is_nil() {
        // let a = env.current_buffer.get().lisp_buffer(cx).into_obj(cx).as_obj_copy();
        env.current_buffer.buf_ref.into_obj(cx).as_obj_copy()
    } else {
        object
    };
    let tree = env
        .buffer_textprops
        .get_mut(obj)
        .ok_or(anyhow::anyhow!("no properties for object"))?;
    Ok(tree.bind_mut(cx))
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
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    let tree = prop_tree_for_object(object, env, cx)?;
    if let Some(prop) = tree.find(position) {
        // TODO lifetimes don't match; don't know why yet
        // unsafe {
        //     return Ok(prop.val.with_lifetime());
        // }
        return Ok(prop.val.as_obj());
    }
    Ok(NIL)
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
    cx: &'ob Context,
) -> Result<Object<'ob>> {
    let props = text_properties_at(position, object, env, cx)?;
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
    env: &'ob mut Rt<Env>,
    cx: &'ob Context,
) -> Result<()> {
    let tree = prop_tree_for_object(object, env, cx)?;
    let prop: Object<'ob> = list!(property, value; cx);
    tree.insert(start, end, crate::core::gc::Slot::new(prop), cx);
    Ok(())
}

// #[defun]
// pub fn next_property_change<'ob>(
//     position: usize,
//     object: Object<'ob>,
//     limit: Object<'ob>,
//     env: &'ob mut Rt<Env<'ob>>,
//     cx: &'ob Context,
// ) -> Result<Object<'ob>> {
//     let tree = prop_tree_for_object(object, env, cx)?;
//     // let point_max = point_max(env)?;
//     // let node = tree.find(position);
//     // let nodes = tree.find_intersects(TextRange::new(position, point_max));

//     // If LIMIT is t, return start of next interval--don't
//     // bother checking further intervals.
//     // if eq(limit, TRUE) {
//     //     let pos = if let Some(node) = node {
//     //         if let Some(n) = node.next() {
//     //             n.key.end
//     //         } else {
//     //             return Ok(NIL);
//     //         }
//     //     } else {
//     //         // we don't have a text property interval around position
//     //         if let Some(n) =
//     //             tree.find_intersects(TextRange::new(position, point_max(env)?)).first().copied()
//     //         {
//     //             n.key.start
//     //         } else {
//     //             return Ok(NIL);
//     //         }
//     //     };
//     //     return Ok(cx.add(pos));
//     // };

//     todo!()
// }

#[cfg(test)]
mod tests {
    use crate::{
        buffer::get_buffer_create,
        core::{
            env::intern,
            gc::{Context, RootSet},
            object::LispBuffer,
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
        // let cons1 = Cons::new("start", Cons::new(7, Cons::new(5, 9, cx), cx), cx);
        let plist_1 = list![intern(":a", cx), 1, intern(":b", cx), 2; cx];
        let plist_2 = list![intern(":a", cx), 4, intern(":c", cx), 5; cx];
        let (plist_1, changed) =
            add_properties(plist_2, plist_1, PropertySetType::Replace, false, cx).unwrap();
        let plist_1 = dbg!(plist_1);
        let a = plist_get(plist_1, intern(":a", cx).into()).unwrap();
        let b = plist_get(plist_1, intern(":b", cx).into()).unwrap();
        let c = plist_get(plist_1, intern(":c", cx).into()).unwrap();
        assert_eq!(changed, true);
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
        if env.buffer_textprops.get(buf).is_none() {
            env.buffer_textprops.insert(buf, IntervalTree::new());
        }
        // let cons1 = Cons::new("start", Cons::new(7, Cons::new(5, 9, cx), cx), cx);
        let n = text_properties_at(0, buf, env, cx)?;
        assert!(n.is_nil());

        let a = intern(":a", cx);
        let a = cx.add(a);
        put_text_property(0, 1, a, cx.add(3), buf, env, cx)?;
        let n = text_properties_at(0, buf, env, cx)?;
        let val = plist_get(n, a)?;
        assert!(eq(val, cx.add(3)));
        Ok(())
    }
}
