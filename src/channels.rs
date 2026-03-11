//! Channel operations for CSP-style concurrency.

use crate::core::{
    cons::Cons,
    gc::Context,
    object::{
        ChannelReceiver, ChannelSender, Gc, IntoObject, NIL, Object, Symbol, make_channel_pair,
    },
};
use rune_macros::defun;
use std::time::Duration;

#[defun]
fn make_channel<'ob>(capacity: Option<i64>, cx: &'ob Context) -> Object<'ob> {
    let cap = capacity.unwrap_or(1).max(1) as usize;
    let (sender, receiver) = make_channel_pair(cap);
    // Return (sender . receiver) as a cons pair
    Cons::new(sender.into_obj(&cx.block), receiver.into_obj(&cx.block), cx).into()
}

#[defun]
fn channel_send<'ob>(sender: Gc<&ChannelSender>, obj: Object<'ob>) -> Object<'ob> {
    match sender.send(obj) {
        Ok(()) => NIL,
        Err(e) => Symbol::from(e).into(),
    }
}

#[defun]
fn channel_recv<'ob>(receiver: Gc<&ChannelReceiver>, cx: &'ob Context) -> Object<'ob> {
    match receiver.recv(cx) {
        Ok(obj) => obj,
        Err(e) => Symbol::from(e).into(),
    }
}

#[defun]
fn channel_try_send<'ob>(sender: Gc<&ChannelSender>, obj: Object<'ob>) -> Object<'ob> {
    match sender.try_send(obj) {
        Ok(()) => NIL,
        Err(e) => Symbol::from(e).into(),
    }
}

#[defun]
fn channel_try_recv<'ob>(receiver: Gc<&ChannelReceiver>, cx: &'ob Context) -> Object<'ob> {
    match receiver.try_recv(cx) {
        Ok(obj) => obj,
        Err(e) => Symbol::from(e).into(),
    }
}

#[defun]
fn channel_send_timeout<'ob>(
    sender: Gc<&ChannelSender>,
    obj: Object<'ob>,
    timeout_secs: f64,
) -> Object<'ob> {
    let timeout = Duration::from_secs_f64(timeout_secs.max(0.0));
    match sender.send_timeout(obj, timeout) {
        Ok(()) => NIL,
        Err(e) => Symbol::from(e).into(),
    }
}

#[defun]
fn channel_recv_timeout<'ob>(
    receiver: Gc<&ChannelReceiver>,
    timeout_secs: f64,
    cx: &'ob Context,
) -> Object<'ob> {
    let timeout = Duration::from_secs_f64(timeout_secs.max(0.0));
    match receiver.recv_timeout(cx, timeout) {
        Ok(obj) => obj,
        Err(e) => Symbol::from(e).into(),
    }
}

/// Close the sender side of a channel.
///
/// After closing, no more values can be sent through this sender.
/// Any receivers waiting on this channel will be woken up.
/// If all senders are closed or dropped, receivers will get 'channel-closed.
#[defun]
fn channel_sender_close<'ob>(sender: Gc<&ChannelSender>) -> Object<'ob> {
    sender.close();
    NIL
}

/// Close the receiver side of a channel.
///
/// After closing, no more values can be received from this receiver.
/// This is useful for "moving" a receiver to another thread by cloning
/// it into a new context and then closing the original.
///
/// This is idempotent - calling it multiple times has no additional effect.
#[defun]
fn channel_receiver_close<'ob>(receiver: Gc<&ChannelReceiver>) -> Object<'ob> {
    receiver.close();
    NIL
}
