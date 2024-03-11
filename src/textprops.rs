use crate::{
    core::{
        cons::Cons,
        gc::Context,
        object::{Gc, ListType, Object},
    },
    fns::eq,
};
use anyhow::Result;

#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PropertySetType {
    Replace,
    Prepend,
    Append,
}

// Add the properties of PLIST to the interval I, or set
//    the value of I's property to the value of the property on PLIST
//    if they are different.

//    OBJECT should be the string or buffer the interval is in.

//    If DESTRUCTIVE, the function is allowed to reuse list values in the
//    properties.

//    Return true if this changes I (i.e., if any members of PLIST
//    are actually added to I's plist)
pub fn add_properties<'ob>(
    plist: Object<'ob>,
    obj_i: &mut Object<'ob>,
    _set_type: PropertySetType,
    _destructive: bool,
    cx: &'ob Context,
) -> Result<bool> {
    // TODO return type
    let mut changed = false;
    let Ok(plist) = Gc::<ListType>::try_from(plist) else { return Ok(false) };
    let Ok(plist_i) = Gc::<ListType>::try_from(*obj_i) else { return Ok(false) };
    let mut iter = plist.elements();
    // iterate through plist, finding key1 and val1
    while let Some(key1) = iter.next() {
        let key1 = key1?;
        let Some(val1) = iter.next() else { return Ok(changed) };
        let mut found = false;

        let mut iter_i = plist_i.conses();
        // iterate through i's plist, finding (key2, val2) and set val2 if key2 == key1;
        while let Some(key2_cons) = iter_i.next() {
            let Some(val2_cons) = iter_i.next() else { return Ok(changed) };
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
            *obj_i = new_cons.into(); // TODO this does not work
        }
    }
    Ok(changed)
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
        let mut plist_1 = list![intern(":a", cx), 1, intern(":b", cx), 2; cx];
        let plist_2 = list![intern(":a", cx), 4, intern(":c", cx), 5; cx];
        let changed =
            add_properties(plist_2, &mut plist_1, PropertySetType::Replace, false, cx).unwrap();
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
