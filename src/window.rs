use anyhow::Result;
use rune_macros::defun;

use crate::{
    buffer::get_buffer,
    core::object::{IntoObject, LispBuffer, LispFrame, LispWindow, Object, ObjectType, TagType, WithLifetime},
    frame::selected_frame,
    Context, Env, Gc, Rt,
};

#[defun]
fn windowp(object: Object) -> bool {
    matches!(object.untag(), ObjectType::Window(_))
}

#[defun]
fn window_frame(window: Object) -> Result<Object> {
    match window.untag() {
        ObjectType::Window(w) => {
            let frame: Object = w.get_frame().into();
            Ok(frame)
        }
        _ => todo!(),
    }
}

#[defun]
fn selected_window<'ob>(env: &Rt<Env>, cx: &'ob Context) -> Result<Object<'ob>> {
    let frame = selected_frame(env);
    // println!("{frame:?}");
    match frame.untag() {
        ObjectType::Frame(f) => {
            let w = f.selected_window();
            let w = w.tag().into_obj(&cx);
            Ok(w)
        }
        _ => todo!(),
    }
}

#[defun]
fn set_window_buffer<'ob>(
    window: Gc<&LispWindow>,
    buffer_or_name: Object<'ob>,
    keep_margins: Option<Object<'ob>>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<()> {
    let buf = get_buffer(buffer_or_name, cx)?;
    let ObjectType::Buffer(buf) = buf.untag() else { anyhow::bail!("not a buffer") };
    let content = env.current_buffer.get().get().text.to_string();
    // let content = buf.lock().unwrap().text.to_string();
    let w = window.untag();
    // println!("content: {content}");
    // println!("{buffer_or_name:?}");
    w.modify_data(|data| data.set_buffer(buf));
    println!("{buffer_or_name:?}");
    todo!();
    Ok(())
}

#[defun]
fn set_frame_selected_window(
    frame: Gc<&LispFrame>,
    window: Gc<&LispWindow>,
    norecord: Object,
    env: &mut Rt<Env>,
) {
    frame.untag().set_selected_window(window.untag().id());
}