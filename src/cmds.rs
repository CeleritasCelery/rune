use anyhow::{anyhow, Result};
use rune_macros::defun;

use crate::{core::env::ArgSlice, editfns::{insert, line_beginning_position, point_min}, Context, Env, Rt};

#[defun]
fn self_insert_command(n: usize, c: char, env: &mut Rt<Env>, cx: &Context) -> Result<()> {
    let c = cx.add(c);
    let buf = env.current_buffer.get_mut();
    for _ in 0..n {
        env.stack.push(c);
    }
    insert(ArgSlice::new(n), env, cx)?;
    Ok(())
}

#[defun]
fn delete_char(n: i64, kill_flag: Option<bool>, env: &mut Rt<Env>) -> Result<()> {
    let buf = env.current_buffer.get_mut();
    if n > 0 {
        buf.text.delete_forwards(n as usize);
    } else {
        buf.text.delete_backwards((-n) as usize);
    }
    Ok(())
}

#[defun]
fn forward_char(n: usize, env: &mut Rt<Env>) {
    move_point(n, true, env)
}

#[defun]
fn backward_char(n: usize, env: &mut Rt<Env>) {
    move_point(n, false, env)
}

#[defun]
fn beginning_of_line(n: Option<usize>, env: &mut Rt<Env>) -> Result<()> {
    let bol = line_beginning_position(n, env)?;
    let buf = env.current_buffer.get_mut();
    buf.text.set_cursor(bol);
    Ok(())
}


fn move_point(n: usize, forward: bool, env: &mut Rt<Env>) {
    let buf = env.current_buffer.get_mut();
    let pos = buf.text.cursor().chars();
    let new_pos = if forward {
        buf.text.len_chars().min(pos + n)
    } else {
        if pos > n { pos - n } else { 1 }
    };

    buf.text.set_cursor(new_pos);
}