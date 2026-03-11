//! Channel types for CSP-style concurrency.
//!
//! Channels provide communication between threads via message passing.
//! Objects are deep-copied into the ChannelManager's block when sent,
//! and deep-copied into the receiver's context when received.

use super::{CloneIn, Gc, Object, Symbol, TagType};
use crate::{
    channel_manager::{self, ChannelId, RecvError, SendError},
    core::{
        env::sym,
        gc::{Block, Context, GcHeap, GcState, Trace},
    },
    derive_GcMoveable,
};
use std::{
    fmt,
    sync::Arc,
    time::{Duration, Instant},
};

// Re-export error types for Symbol conversion
impl From<SendError> for Symbol<'static> {
    fn from(err: SendError) -> Self {
        match err {
            SendError::Closed => sym::CHANNEL_CLOSED,
            SendError::Full => sym::CHANNEL_FULL,
            SendError::Timeout => sym::CHANNEL_TIMEOUT,
        }
    }
}

impl From<RecvError> for Symbol<'static> {
    fn from(err: RecvError) -> Self {
        match err {
            RecvError::Closed => sym::CHANNEL_CLOSED,
            RecvError::Empty => sym::CHANNEL_EMPTY,
            RecvError::Timeout => sym::CHANNEL_TIMEOUT,
        }
    }
}

/// The sending half of a channel.
pub(crate) struct ChannelSender(pub(in crate::core) GcHeap<ChannelSenderInner>);

derive_GcMoveable!(ChannelSender);

pub(in crate::core) struct ChannelSenderInner {
    pub(in crate::core) channel_id: ChannelId,
    pub(in crate::core) manager: Arc<channel_manager::ChannelManager>,
}

impl ChannelSender {
    pub(in crate::core) fn new(
        channel_id: ChannelId,
        manager: Arc<channel_manager::ChannelManager>,
        constant: bool,
    ) -> Self {
        manager.increment_sender(channel_id);
        Self(GcHeap::new(ChannelSenderInner { channel_id, manager }, constant))
    }

    /// Send an object through the channel, blocking if full.
    /// Returns Err(SendError::Closed) if the receiver has been dropped.
    pub(crate) fn send<'ob>(&self, obj: Object<'ob>) -> Result<(), SendError> {
        self.0.manager.send(self.0.channel_id, obj)
    }

    /// Try to send without blocking.
    /// Returns Err(SendError::Full) if the channel is full.
    /// Returns Err(SendError::Closed) if the receiver has been dropped.
    pub(crate) fn try_send<'ob>(&self, obj: Object<'ob>) -> Result<(), SendError> {
        self.0.manager.try_send(self.0.channel_id, obj)
    }

    /// Send with a timeout.
    /// Returns Err(SendError::Timeout) if the timeout expires.
    /// Returns Err(SendError::Closed) if the receiver has been dropped.
    pub(crate) fn send_timeout<'ob>(
        &self,
        obj: Object<'ob>,
        timeout: Duration,
    ) -> Result<(), SendError> {
        let start = Instant::now();

        loop {
            match self.try_send(obj) {
                Ok(()) => return Ok(()),
                Err(SendError::Full) => {
                    if start.elapsed() >= timeout {
                        return Err(SendError::Timeout);
                    }
                    // Sleep briefly before retrying
                    std::thread::sleep(Duration::from_millis(1));
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Explicitly close the sender side of the channel.
    /// This wakes up any receivers waiting on recv() and causes them to return Err(RecvError::Closed).
    /// This is idempotent - calling close() multiple times has no additional effect.
    pub(crate) fn close(&self) {
        self.0.manager.close_sender(self.0.channel_id);
    }
}

/// The receiving half of a channel.
pub(crate) struct ChannelReceiver(pub(in crate::core) GcHeap<ChannelReceiverInner>);

derive_GcMoveable!(ChannelReceiver);

pub(in crate::core) struct ChannelReceiverInner {
    pub(in crate::core) channel_id: ChannelId,
    pub(in crate::core) manager: Arc<channel_manager::ChannelManager>,
}

impl ChannelReceiver {
    pub(in crate::core) fn new(
        channel_id: ChannelId,
        manager: Arc<channel_manager::ChannelManager>,
        constant: bool,
    ) -> Self {
        manager.increment_receiver(channel_id);
        Self(GcHeap::new(ChannelReceiverInner { channel_id, manager }, constant))
    }

    /// Receive an object from the channel, blocking if empty.
    /// Returns Err(RecvError::Closed) if all senders have been dropped.
    pub(crate) fn recv<'ob>(&self, cx: &'ob Context) -> Result<Object<'ob>, RecvError> {
        self.0.manager.recv(self.0.channel_id, &cx.block)
    }

    /// Try to receive without blocking.
    /// Returns Err(RecvError::Empty) if the channel is empty.
    /// Returns Err(RecvError::Closed) if all senders have been dropped.
    pub(crate) fn try_recv<'ob>(&self, cx: &'ob Context) -> Result<Object<'ob>, RecvError> {
        self.0.manager.try_recv(self.0.channel_id, &cx.block)
    }

    /// Receive with a timeout.
    /// Returns Err(RecvError::Timeout) if the timeout expires.
    /// Returns Err(RecvError::Closed) if all senders have been dropped.
    pub(crate) fn recv_timeout<'ob>(
        &self,
        cx: &'ob Context,
        timeout: Duration,
    ) -> Result<Object<'ob>, RecvError> {
        let start = Instant::now();

        loop {
            match self.try_recv(cx) {
                Ok(obj) => return Ok(obj),
                Err(RecvError::Empty) => {
                    if start.elapsed() >= timeout {
                        return Err(RecvError::Timeout);
                    }
                    // Sleep briefly before retrying
                    std::thread::sleep(Duration::from_millis(1));
                }
                Err(e) => return Err(e),
            }
        }
    }

    /// Explicitly close the receiver side of the channel.
    /// This wakes up any senders waiting on send() and causes them to return Err(SendError::Closed).
    /// This is idempotent - calling close() multiple times has no additional effect.
    ///
    /// Useful for "moving" a receiver to another thread:
    /// the original receiver can be closed after being cloned into a new context.
    pub(crate) fn close(&self) {
        self.0.manager.close_receiver(self.0.channel_id);
    }
}

/// Create a new channel pair with the specified capacity.
pub(crate) fn make_channel_pair(capacity: usize) -> (ChannelSender, ChannelReceiver) {
    let manager = channel_manager::get_manager();
    let (sender_id, receiver_id) = manager.new_channel_pair(capacity);

    let sender = ChannelSender::new(sender_id, manager.clone(), false);
    let receiver = ChannelReceiver::new(receiver_id, manager, false);
    (sender, receiver)
}

// Drop implementations to decrement counters
impl Drop for ChannelSenderInner {
    fn drop(&mut self) {
        self.manager.close_sender(self.channel_id);
        self.manager.cleanup_channel(self.channel_id);
    }
}

impl Drop for ChannelReceiverInner {
    fn drop(&mut self) {
        self.manager.close_receiver(self.channel_id);
        self.manager.cleanup_channel(self.channel_id);
    }
}

// GC tracing (channels themselves don't hold GC objects, state is in the manager)
impl Trace for ChannelSenderInner {
    fn trace(&self, _state: &mut GcState) {
        // No GC objects in sender
    }
}

impl Trace for ChannelReceiverInner {
    fn trace(&self, _state: &mut GcState) {
        // No GC objects in receiver
    }
}

// CloneIn implementations for cross-context copying
impl<'new> CloneIn<'new, &'new Self> for ChannelSender {
    fn clone_in<const C: bool>(&self, _bk: &'new Block<C>) -> Gc<&'new Self> {
        let new_sender = ChannelSender::new(self.0.channel_id, self.0.manager.clone(), false);

        // SAFETY: Sender just holds an Arc and channel ID, safe to transmute
        unsafe { std::mem::transmute::<Gc<&Self>, Gc<&'new Self>>((&new_sender).tag()) }
    }
}

impl<'new> CloneIn<'new, &'new Self> for ChannelReceiver {
    fn clone_in<const C: bool>(&self, _bk: &'new Block<C>) -> Gc<&'new Self> {
        let new_receiver = ChannelReceiver::new(self.0.channel_id, self.0.manager.clone(), false);

        // SAFETY: Receiver just holds an Arc and channel ID, safe to transmute
        unsafe { std::mem::transmute::<Gc<&Self>, Gc<&'new Self>>((&new_receiver).tag()) }
    }
}

// IntoObject implementations are in tagged.rs

// Debug implementations
impl fmt::Debug for ChannelSender {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<channel-sender:{}>", self.0.channel_id)
    }
}

impl fmt::Display for ChannelSender {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<channel-sender>")
    }
}

impl fmt::Debug for ChannelReceiver {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<channel-receiver:{}>", self.0.channel_id)
    }
}

impl fmt::Display for ChannelReceiver {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<channel-receiver>")
    }
}

// Implement object_trait_impls macro pattern manually
impl PartialEq for ChannelSender {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.0, &*other.0)
    }
}

impl Eq for ChannelSender {}

impl PartialEq for ChannelReceiver {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.0, &*other.0)
    }
}

impl Eq for ChannelReceiver {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{
        cons::Cons,
        gc::{Context, RootSet},
        object::{IntoObject, ObjectType},
    };

    #[test]
    fn test_basic_send_recv() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (sender, receiver) = make_channel_pair(1);

        // Send an integer
        sender.send(cx.add(42)).unwrap();

        // Receive it
        let result = receiver.recv(&cx).unwrap();
        if let ObjectType::Int(n) = result.untag() {
            assert_eq!(n, 42);
        } else {
            panic!("Expected integer");
        }
    }

    #[test]
    fn test_double_clone_verification() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (sender, receiver) = make_channel_pair(1);

        // Create a string
        let original = cx.add("test string");

        // Send it
        sender.send(original).unwrap();

        // Receive it
        let received = receiver.recv(&cx).unwrap();

        // Verify content is same
        if let ObjectType::String(s1) = original.untag() {
            if let ObjectType::String(s2) = received.untag() {
                assert_eq!(s1.as_ref(), s2.as_ref());

                // Verify they are different allocations (double-copy worked)
                assert_ne!(
                    s1.as_ptr() as usize,
                    s2.as_ptr() as usize,
                    "Strings should be different allocations"
                );
            } else {
                panic!("Expected string");
            }
        } else {
            panic!("Expected string");
        }
    }

    #[test]
    fn test_channel_closed_on_sender_drop() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (sender, receiver) = make_channel_pair(1);

        // Drop sender
        drop(sender);

        // Try to receive - should get Closed error
        let result = receiver.recv(&cx);
        assert!(matches!(result, Err(RecvError::Closed)));
    }

    #[test]
    fn test_channel_full() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (sender, receiver) = make_channel_pair(1);

        // Fill the channel
        sender.send(cx.add(1)).unwrap();

        // Try to send again without blocking - should get Full error
        let result = sender.try_send(cx.add(2));
        assert!(matches!(result, Err(SendError::Full)));

        // Receive the first message
        let _ = receiver.recv(&cx).unwrap();

        // Now we should be able to send again
        sender.try_send(cx.add(2)).unwrap();

        // Clean up - receive the second message
        let _ = receiver.recv(&cx).unwrap();
    }

    #[test]
    fn test_channel_empty() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (_sender, receiver) = make_channel_pair(1);

        // Try to receive from empty channel - should get Empty error
        let result = receiver.try_recv(&cx);
        assert!(matches!(result, Err(RecvError::Empty)));
    }

    #[test]
    fn test_complex_objects() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (sender, receiver) = make_channel_pair(5);

        // Send a string
        let str_obj = cx.add("hello");
        sender.send(str_obj).unwrap();

        // Send a cons cell (list)
        let list = Cons::new(cx.add(1), cx.add(2), &cx).into_obj(&cx.block);
        sender.send(list.into()).unwrap();

        // Send a vector
        let vec_obj = cx.add(vec![cx.add(10), cx.add(20), cx.add(30)]);
        sender.send(vec_obj).unwrap();

        // Receive and verify string
        let recv_str = receiver.recv(&cx).unwrap();
        if let ObjectType::String(s) = recv_str.untag() {
            assert_eq!(s.as_ref(), "hello");
        } else {
            panic!("Expected string");
        }

        // Receive and verify cons
        let recv_list = receiver.recv(&cx).unwrap();
        if let ObjectType::Cons(cons) = recv_list.untag() {
            let car_obj = cons.car();
            if let ObjectType::Int(car) = car_obj.untag() {
                assert_eq!(car, 1);
            } else {
                panic!("Expected int in car");
            }
        } else {
            panic!("Expected cons");
        }

        // Receive and verify vector
        let recv_vec = receiver.recv(&cx).unwrap();
        if let ObjectType::Vec(v) = recv_vec.untag() {
            assert_eq!(v.len(), 3);
            if let ObjectType::Int(n) = v[0].get().untag() {
                assert_eq!(n, 10);
            }
        } else {
            panic!("Expected vector");
        }
    }

    #[test]
    fn test_timeout() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (_sender, receiver) = make_channel_pair(1);

        // Try to receive with very short timeout - should timeout
        let result = receiver.recv_timeout(&cx, Duration::from_millis(10));
        assert!(matches!(result, Err(RecvError::Timeout)));
    }

    #[test]
    fn test_multiple_messages() {
        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (sender, receiver) = make_channel_pair(10);

        // Send multiple messages
        for i in 0..10 {
            sender.send(cx.add(i)).unwrap();
        }

        // Receive them in order
        for i in 0..10 {
            let result = receiver.recv(&cx).unwrap();
            if let ObjectType::Int(n) = result.untag() {
                assert_eq!(n, i);
            } else {
                panic!("Expected integer");
            }
        }
    }

    #[test]
    fn test_channel_sender_is_send() {
        fn assert_send<T: Send>() {}
        assert_send::<ChannelSender>();
    }

    #[test]
    fn test_channel_receiver_is_send() {
        fn assert_send<T: Send>() {}
        assert_send::<ChannelReceiver>();
    }

    #[test]
    fn test_cross_thread_channel() {
        use std::thread;

        let roots = RootSet::default();
        let cx = Context::new(&roots);

        let (sender, receiver) = make_channel_pair(5);

        // Send from spawned thread
        let handle = thread::spawn(move || {
            let roots2 = RootSet::default();
            let cx2 = Context::new(&roots2);
            for i in 0..5 {
                sender.send(cx2.add(i * 10)).unwrap();
            }
        });

        // Receive in main thread
        for i in 0..5 {
            let result = receiver.recv(&cx).unwrap();
            if let ObjectType::Int(n) = result.untag() {
                assert_eq!(n, i * 10);
            } else {
                panic!("Expected integer");
            }
        }

        handle.join().unwrap();
    }

    #[test]
    fn test_concurrent_channel_stress() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicUsize, Ordering};
        use std::thread;

        let roots = RootSet::default();
        let _cx = Context::new(&roots);

        let send_count = Arc::new(AtomicUsize::new(0));
        let recv_count = Arc::new(AtomicUsize::new(0));

        // Spawn multiple sender/receiver thread pairs
        let mut handles = vec![];
        for idx in 0..4 {
            let (sender, receiver) = make_channel_pair(10);
            let send_counter = Arc::clone(&send_count);
            let recv_counter = Arc::clone(&recv_count);

            // Sender thread
            handles.push(thread::spawn(move || {
                let roots = RootSet::default();
                let cx = Context::new(&roots);
                for i in 0..20 {
                    let value = (idx * 100 + i) as i64;
                    sender.send(cx.add(value)).unwrap();
                    send_counter.fetch_add(1, Ordering::SeqCst);
                }
            }));

            // Receiver thread
            handles.push(thread::spawn(move || {
                let roots = RootSet::default();
                let cx = Context::new(&roots);
                for _ in 0..20 {
                    let result = receiver.recv(&cx).unwrap();
                    // Verify it's an integer
                    assert!(matches!(result.untag(), ObjectType::Int(_)));
                    recv_counter.fetch_add(1, Ordering::SeqCst);
                }
            }));
        }

        // Wait for all threads to complete
        for handle in handles {
            handle.join().unwrap();
        }

        // Verify all messages were sent and received
        assert_eq!(send_count.load(Ordering::SeqCst), 80);
        assert_eq!(recv_count.load(Ordering::SeqCst), 80);
    }
}
