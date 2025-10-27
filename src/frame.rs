//         <------------ Outer Frame Width ----------->
//         ____________________________________________
//      ^(0)  ________ External/Outer Border _______   |
//      | |  |_____________ Title Bar ______________|  |
//      | | (1)_____________ Menu Bar ______________|  | ^
//      | | (2)_____________ Tool Bar ______________|  | ^
//      | | (3)_____________ Tab Bar _______________|  | ^
//      | |  |  _________ Internal Border ________  |  | ^
//      | |  | |   ^                              | |  | |
//      | |  | |   |                              | |  | |
// Outer  |  | | Inner                            | |  | Native
// Frame  |  | | Frame                            | |  | Frame
// Height |  | | Height                           | |  | Height
//      | |  | |   |                              | |  | |
//      | |  | |<--+--- Inner Frame Width ------->| |  | |
//      | |  | |   |                              | |  | |
//      | |  | |___v______________________________| |  | |
//      | |  |___________ Internal Border __________|  | v
//      v |___________ External/Outer Border __________|
//            <-------- Native Frame Width -------->

use std::sync::{LazyLock, Mutex};

use crate::{
    alloc::list, core::{
        env::{intern, Env, INTERNED_SYMBOLS},
        gc::{Context, IntoRoot, Rt, Slot},
        object::{FrameConfig, LispFrame, Object, ObjectType, Symbol, WindowConfig, WithLifetime, NIL},
    }, Gc
};
use anyhow::Result;
use rune_core::{hashmap::HashMap, macros::list};
use rune_macros::defun;

type FrameMap = HashMap<String, &'static LispFrame>;
pub(crate) static FRAMES: LazyLock<Mutex<FrameMap>> = LazyLock::new(Mutex::default);

#[defun]
fn framep(object: Object) -> bool {
    matches!(object.untag(), ObjectType::Frame(_))
}

#[defun]
fn frame_live_p(object: Object) -> bool {
    // TODO
    matches!(object.untag(), ObjectType::Frame(_))
}

#[defun]
fn window_system<'ob>(frame: Object<'ob>, cx: &'ob Context<'ob>) -> Object<'ob> {
    cx.add(intern("rune", cx))
}

#[defun]
pub fn make_terminal_frame<'ob>(
    parameters: Object,
    cx: &'ob Context,
    env: &mut Rt<Env>,
) -> Result<Object<'ob>> {
    // TODO width and height should be obtained from parameters
    // and various default parameters
    let width = 800.0; // Default width
    let height = 600.0; // Default height

    // Create a new frame with the specified or default dimensions
    let frame = FrameConfig::new(width, height);
    let lispframe: &'static LispFrame = {
        let global = INTERNED_SYMBOLS.lock().unwrap();
        let params = Slot::new(parameters);
        let lispframe = LispFrame::create(frame, params, global.global_block());
        unsafe { &*(lispframe as *const LispFrame) }
    };

    FRAMES.lock().unwrap().insert(String::new(), lispframe);
    let result = cx.add(lispframe);
    Ok(result)
}

#[defun]
fn select_frame(frame: Object, _norecord: Option<bool>, env: &mut Rt<Env>) {
    // TODO how to deal with slot?
    env.selected_frame = Some(unsafe { frame.into_root() })
}

#[defun]
pub(crate) fn selected_frame<'ob>(env: &'ob Rt<Env>) -> Object<'ob> {
    env.selected_frame
        .as_ref()
        .map(|f| unsafe { (**f).with_lifetime() })
        .unwrap_or(NIL)
}

/// NOTE this function is implemented in elisp because it is gui platform-specific.
///
/// Return geometric attributes of FRAME.
/// FRAME must be a live frame and defaults to the selected one.
#[defun]
fn frame_geometry<'ob>(
    frame: Option<Object>,
    cx: &'ob Context,
    env: &mut Rt<Env>,
) -> Result<Object<'ob>> {
    if let Some(f) =
        frame
            .or(env.selected_frame.as_ref().map(|f| **f))
            .and_then(|f| match f.untag() {
                ObjectType::Frame(f) => Some(f),
                _ => None,
            })
    {
        let data = f.data();
        let outpos = data.config.layout.get(data.config.layout.root)?;
        let outer_position = list!(intern("outer-position", cx),
            outpos.location.x as i64,
            outpos.location.y as i64,
            ; cx);

        let outer_size = list!(intern("outer-size", cx),
            outpos.size.width as i64,
            outpos.size.height as i64,
            ; cx);

        let out_border = outpos.border;
        let external_border_size = list!(intern("external-border-size", cx),
            out_border.right as i64, out_border.bottom as i64, ; cx);

        let outer_border_width = list!(intern("outer-border-width", cx), 0, ; cx);

        let title_bar_size = list!(intern("title-bar-size", cx), 0, 0, ; cx);

        let menu_bar_external = list!(intern("menu-bar-external", cx), true, ; cx);

        let menu_bar_size = list!(intern("menu-bar-size", cx), 0, 0, ; cx);

        let tab_bar_size = list!(intern("tab-bar-size", cx), 0, 0, ; cx);

        let tool_bar_external = list!(intern("tool-bar-external", cx), true, ; cx);

        let tool_bar_position = list!(intern("tool-bar-position", cx), intern("top", cx), ; cx);

        let tool_bar_size = list!(intern("tool-bar-size", cx), 0, 0, ; cx);

        let internal_border_width = list!(intern("internal-border-width", cx), 0, ; cx);
        let a = list!(external_border_size, outer_border_width, title_bar_size, menu_bar_external, menu_bar_size, tab_bar_size, tool_bar_external, tool_bar_position, tool_bar_size, internal_border_width; cx);
        return Ok(a);
    }

    Ok(NIL)
}

#[defun]
fn modify_frame_parameters<'ob>(
    frame: Object<'ob>,
    parameters: Object<'ob>,
    cx: &'ob Context,
    env: &mut Rt<Env>,
) {
    todo!()
}

fn as_frame(object: Object) -> Option<&LispFrame> {
    match object.untag() {
        ObjectType::Frame(f) => Some(f),
        _ => None,
    }
}

defvar!(INHIBIT_X_RESOURCES);
defvar!(MINIBUFFER_PROMPT_PROPERTIES, list![""]);

#[defun]
fn frame_list<'ob>(env: &Rt<Env>, cx: &'ob Context) -> Object<'ob> {
    let frames: Vec<Object> = FRAMES.lock().unwrap().values().map(|x| cx.add(*x)).collect();
    return list(&frames, cx)
}