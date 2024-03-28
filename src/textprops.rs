use crate::{
    core::{
        cons::Cons,
        env::Env,
        gc::{Context, Rt},
        object::{Gc, ListType, Object, ObjectType, WithLifetime, NIL, TRUE},
    },
    editfns::point_max,
    fns::{eq, plist_get},
    intervals::{IntervalTree, TextRange},
};
use anyhow::{bail, Result};
use rune_macros::defun;
use rune_core::macros::list;

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

pub fn plist_get_str<'ob>(plist: Object<'ob>, prop: &str) -> Result<Object<'ob>> {
    let Ok(plist) = Gc::<ListType>::try_from(plist) else { return Ok(NIL) };

    // TODO: this function should never fail. Need to implement safe iterator
    let mut iter = plist.elements();
    while let Some(cur_prop) = iter.next() {
        let Some(value) = iter.next() else { return Ok(NIL) };
        if let ObjectType::Symbol(sym) = cur_prop?.untag() {
            if sym.to_string() == prop {
                return Ok(value?);
            }
        }
    }
    Ok(NIL)
}

/// Get the property interval tree for OBJECT. OBJECT is a buffer or string.
/// If OBJECT is NIL, return current buffer's interval tree.
fn prop_tree_for_object<'a, 'ob, 'env>(
    object: Object<'ob>,
    env: &'a mut Rt<Env<'env>>,
) -> Result<&'a mut IntervalTree<'env>> {
    let tree = match object.untag() {
        ObjectType::Buffer(buf) => env.get_buffer_textprops(buf)?,
        ObjectType::NIL => {
            let Some(tree) = env.current_buffer_textprops() else { bail!("no current buffer") };
            tree
        }
        _ => {
            bail!("wrong type argument: not a buffer");
        }
    };
    Ok(tree)
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
    env: &mut Rt<Env>,
) -> Result<Object<'ob>> {
    let tree = prop_tree_for_object(object, env)?;
    if let Some(prop) = tree.find(position) {
        // TODO lifetimes don't match; don't know why yet
        unsafe {
            return Ok(prop.val.with_lifetime());
        }
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
    env: &mut Rt<Env>,
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
    cx: &'ob Context,
    env: &mut Rt<Env>,
) -> Result<()> {
    let tree = prop_tree_for_object(object, env)?;
    let prop: Object<'ob> = list!(property, value; cx);
    tree.insert(TextRange::new(start, end), prop, cx);
    Ok(())
}

#[defun]
pub fn next_property_change<'ob>(
    position: usize,
    object: Object<'ob>,
    limit: Object<'ob>,
    cx: &'ob Context,
    env: &mut Rt<Env>,
) -> Result<Object<'ob>> {
    let tree = prop_tree_for_object(object, env)?;
    // let point_max = point_max(env)?;
    // let node = tree.find(position);
    // let nodes = tree.find_intersects(TextRange::new(position, point_max));

    // If LIMIT is t, return start of next interval--don't
    // bother checking further intervals.
    // if eq(limit, TRUE) {
    //     let pos = if let Some(node) = node {
    //         if let Some(n) = node.next() {
    //             n.key.end
    //         } else {
    //             return Ok(NIL);
    //         }
    //     } else {
    //         // we don't have a text property interval around position
    //         if let Some(n) =
    //             tree.find_intersects(TextRange::new(position, point_max(env)?)).first().copied()
    //         {
    //             n.key.start
    //         } else {
    //             return Ok(NIL);
    //         }
    //     };
    //     return Ok(cx.add(pos));
    // };

    todo!()
}

#[cfg(test)]
mod tests {
    use crate::{
        core::{
            env::intern,
            gc::{Context, RootSet},
        },
        fns::plist_get,
    };
    use rune_core::macros::list;

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
}
