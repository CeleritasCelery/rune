//! Channel Manager - manages all channel state in a dedicated garbage-collected block
//!
//! This module provides a singleton ChannelManager that
//! can be accessed from multiple threads.
//! It has its own Block and manages its roots manually using the Heap.
//! All channel operations go through this manager, allowing proper garbage
//! collection of in-transit objects.

use crate::core::{
    gc::{__HeapRoot, Block, Context, Slot, ThreadSafeRootSet, collect_garbage_raw},
    object::{CloneIn, Object},
};
use std::sync::OnceLock;
use std::{
    collections::{HashMap, VecDeque},
    sync::{
        Arc, Condvar, Mutex, RwLock,
        atomic::{AtomicU64, AtomicUsize, Ordering},
    },
};

/// Unique identifier for a channel
pub(crate) type ChannelId = u64;

/// GC state for the channel manager, all guarded by a single Mutex to prevent deadlocks
struct GcState {
    /// The block where all in-transit objects are stored
    block: Block<false>,
    /// Static reference to the root set (points into gc_state.roots)
    roots_static: &'static ThreadSafeRootSet,
    /// Next GC threshold (in bytes)
    next_limit: usize,
}

impl GcState {
    fn new(roots_static: &'static ThreadSafeRootSet) -> Self {
        Self {
            block: Block::new_local_unchecked(),
            roots_static,
            next_limit: Context::MIN_GC_BYTES,
        }
    }
}

/// Data for a single channel stored in the manager
struct ChannelData {
    /// Queue of objects rooted via HeapRoot
    queue: VecDeque<__HeapRoot<Slot<Object<'static>>>>,
    /// Maximum capacity of the channel
    capacity: usize,
    /// Number of active senders
    sender_count: usize,
    /// Number of active receivers
    receiver_count: usize,
    /// Whether senders have been explicitly closed
    sender_closed: bool,
    /// Whether receivers have been explicitly closed
    receiver_closed: bool,
}

impl ChannelData {
    fn new(capacity: usize) -> Self {
        Self {
            queue: VecDeque::with_capacity(capacity),
            capacity,
            sender_count: 0,
            receiver_count: 0,
            sender_closed: false,
            receiver_closed: false,
        }
    }

    fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    fn is_full(&self) -> bool {
        self.queue.len() >= self.capacity
    }
}

/// State for a single channel, with synchronization
struct ChannelState {
    data: Mutex<ChannelData>,
    /// Condvar to wake senders waiting for space
    not_full: Condvar,
    /// Condvar to wake receivers waiting for data
    not_empty: Condvar,
}

impl ChannelState {
    fn new(capacity: usize) -> Self {
        Self {
            data: Mutex::new(ChannelData::new(capacity)),
            not_full: Condvar::new(),
            not_empty: Condvar::new(),
        }
    }
}

/// The channel manager state
pub(crate) struct ChannelManager {
    /// GC state (block, roots, and next_limit) all guarded by a single Mutex
    /// We also leak a static reference to the RootSet for HeapRoot creation
    gc_state: Mutex<GcState>,
    /// Ensures mutual exclusion between GC and channel operations.
    /// Channel operations acquire read lock when accessing rooted data,
    /// GC acquires write lock for exclusive access during trace.
    gc_access: RwLock<()>,
    /// All channels managed by this manager
    channels: RwLock<HashMap<ChannelId, Arc<ChannelState>>>,
    /// Counter for generating unique channel IDs
    next_channel_id: AtomicU64,
    /// Counter for receives since last GC
    recv_count: AtomicUsize,
    /// Trigger GC after this many receives
    gc_threshold: usize,
}

impl ChannelManager {
    fn new() -> Self {
        // It is safe to leak the rootset here, `ChannelManager` is intended
        // to be lazily instantiated once for the whole remaining lifetime of
        // the program
        let roots_static: &'static ThreadSafeRootSet =
            Box::leak(Box::new(ThreadSafeRootSet::default()));

        Self {
            gc_state: Mutex::new(GcState::new(roots_static)),
            gc_access: RwLock::new(()),
            channels: RwLock::new(HashMap::new()),
            next_channel_id: AtomicU64::new(1),
            recv_count: AtomicUsize::new(0),
            gc_threshold: 10,
        }
    }

    /// Create a new channel and return its ID
    fn create_channel(&self, capacity: usize) -> ChannelId {
        let id = self.next_channel_id.fetch_add(1, Ordering::SeqCst);
        let mut channels = self.channels.write().unwrap();
        channels.insert(id, Arc::new(ChannelState::new(capacity)));
        id
    }

    /// Increment sender count for a channel
    pub(crate) fn increment_sender(&self, channel_id: ChannelId) {
        let channels = self.channels.read().unwrap();
        if let Some(state) = channels.get(&channel_id) {
            let mut data = state.data.lock().unwrap();
            data.sender_count += 1;
        }
    }

    /// Increment receiver count for a channel
    pub(crate) fn increment_receiver(&self, channel_id: ChannelId) {
        let channels = self.channels.read().unwrap();
        if let Some(state) = channels.get(&channel_id) {
            let mut data = state.data.lock().unwrap();
            data.receiver_count += 1;
        }
    }

    /// Try to send an object to a channel (non-blocking)
    pub(crate) fn try_send<'ob>(
        &self,
        channel_id: ChannelId,
        obj: Object<'ob>,
    ) -> Result<(), SendError> {
        let channels = self.channels.read().unwrap();
        let state = channels.get(&channel_id).ok_or(SendError::Closed)?;
        let state = Arc::clone(state);
        drop(channels);

        let mut data = state.data.lock().unwrap();

        if data.receiver_count == 0 || data.receiver_closed {
            return Err(SendError::Closed);
        }

        if data.is_full() {
            return Err(SendError::Full);
        }

        // Clone the object into the manager's block
        let _gc_guard = self.gc_access.read().unwrap();
        let gc_state = self.gc_state.lock().unwrap();
        let cloned = unsafe {
            let obj_static = obj.clone_in(&gc_state.block);
            std::mem::transmute::<Object<'_>, Object<'static>>(obj_static)
        };
        // Wrap in HeapRoot to protect from GC
        let rooted = __HeapRoot::new(Slot::new(cloned), gc_state.roots_static);
        drop(gc_state);
        drop(_gc_guard);

        data.queue.push_back(rooted);

        state.not_empty.notify_one();

        Ok(())
    }

    /// Send an object to a channel (blocking)
    pub(crate) fn send<'ob>(
        &self,
        channel_id: ChannelId,
        obj: Object<'ob>,
    ) -> Result<(), SendError> {
        let channels = self.channels.read().unwrap();
        let state = channels.get(&channel_id).ok_or(SendError::Closed)?;
        let state = Arc::clone(state);
        drop(channels);

        let mut data = state.data.lock().unwrap();

        while data.is_full() {
            if data.receiver_count == 0 || data.receiver_closed {
                return Err(SendError::Closed);
            }
            data = state.not_full.wait(data).unwrap();
        }

        if data.receiver_count == 0 || data.receiver_closed {
            return Err(SendError::Closed);
        }

        // Clone the object into the manager's block
        let _gc_guard = self.gc_access.read().unwrap();
        let gc_state = self.gc_state.lock().unwrap();
        let cloned = unsafe {
            let obj_static = obj.clone_in(&gc_state.block);
            std::mem::transmute::<Object<'_>, Object<'static>>(obj_static)
        };
        // Wrap in HeapRoot to protect from GC
        let rooted = __HeapRoot::new(Slot::new(cloned), gc_state.roots_static);
        drop(gc_state);
        drop(_gc_guard);

        data.queue.push_back(rooted);

        state.not_empty.notify_one();

        Ok(())
    }

    /// Try to receive an object from a channel (non-blocking)
    pub(crate) fn try_recv<'ob>(
        &self,
        channel_id: ChannelId,
        target_block: &'ob Block<false>,
    ) -> Result<Object<'ob>, RecvError> {
        let channels = self.channels.read().unwrap();
        let state = channels.get(&channel_id).ok_or(RecvError::Closed)?;
        let state = Arc::clone(state);
        drop(channels);

        let mut data = state.data.lock().unwrap();

        if let Some(rooted) = data.queue.pop_front() {
            let _gc_guard = self.gc_access.read().unwrap();
            let obj: Object<'static> = rooted.get_inner();
            let result = obj.clone_in(target_block);
            drop(_gc_guard);

            // HeapRoot is dropped here, unrooting the object
            drop(rooted);
            drop(data);

            state.not_full.notify_one();

            let recv_count = self.recv_count.fetch_add(1, Ordering::SeqCst) + 1;
            if recv_count >= self.gc_threshold {
                self.recv_count.store(0, Ordering::SeqCst);
                self.collect_garbage();
            }

            Ok(result)
        } else if (data.sender_count == 0 || data.sender_closed) && data.is_empty() {
            Err(RecvError::Closed)
        } else {
            Err(RecvError::Empty)
        }
    }

    /// Receive an object from a channel (blocking)
    pub(crate) fn recv<'ob>(
        &self,
        channel_id: ChannelId,
        target_block: &'ob Block<false>,
    ) -> Result<Object<'ob>, RecvError> {
        let channels = self.channels.read().unwrap();
        let state = channels.get(&channel_id).ok_or(RecvError::Closed)?;
        let state = Arc::clone(state);
        drop(channels);

        let mut data = state.data.lock().unwrap();

        while data.is_empty() {
            if data.sender_count == 0 || data.sender_closed {
                return Err(RecvError::Closed);
            }
            data = state.not_empty.wait(data).unwrap();
        }

        if let Some(rooted) = data.queue.pop_front() {
            let _gc_guard = self.gc_access.read().unwrap();
            let obj: Object<'static> = rooted.get_inner();
            let result = obj.clone_in(target_block);
            drop(_gc_guard);

            // HeapRoot is dropped here, unrooting the object
            drop(rooted);
            drop(data);

            state.not_full.notify_one();

            let recv_count = self.recv_count.fetch_add(1, Ordering::SeqCst) + 1;
            if recv_count >= self.gc_threshold {
                self.recv_count.store(0, Ordering::SeqCst);
                self.collect_garbage();
            }

            Ok(result)
        } else {
            Err(RecvError::Closed)
        }
    }

    /// Close the sender side of a channel
    pub(crate) fn close_sender(&self, channel_id: ChannelId) {
        let channels = self.channels.read().unwrap();
        if let Some(state) = channels.get(&channel_id) {
            let mut data = state.data.lock().unwrap();
            if data.sender_count > 0 {
                data.sender_count -= 1;
            }
            if data.sender_count == 0 {
                data.sender_closed = true;
                drop(data);
                state.not_empty.notify_all();
            }
        }
    }

    /// Close the receiver side of a channel
    pub(crate) fn close_receiver(&self, channel_id: ChannelId) {
        let channels = self.channels.read().unwrap();
        if let Some(state) = channels.get(&channel_id) {
            let mut data = state.data.lock().unwrap();
            if data.receiver_count > 0 {
                data.receiver_count -= 1;
            }
            if data.receiver_count == 0 {
                data.receiver_closed = true;
                // Clear the queue (HeapRoots will be dropped and unrooted)
                let _gc_guard = self.gc_access.read().unwrap();
                data.queue.clear();
                drop(_gc_guard);
                drop(data);
                state.not_full.notify_all();
            }
        }
    }

    /// Trigger garbage collection in the manager's block
    fn collect_garbage(&self) {
        let _gc_guard = self.gc_access.write().unwrap();
        let mut gc_state = self.gc_state.lock().unwrap();

        // SAFETY: We have exclusive access via both locks:
        // - gc_access write lock ensures no concurrent HeapRoot access
        // - gc_state mutex ensures exclusive access to Block's RefCells
        unsafe {
            let GcState { ref mut block, ref mut next_limit, roots_static } = *gc_state;
            collect_garbage_raw(block, roots_static, false, next_limit);
        }
    }

    /// Remove a channel that has no more senders or receivers
    pub(crate) fn cleanup_channel(&self, channel_id: ChannelId) {
        let channels = self.channels.read().unwrap();
        let should_remove = if let Some(state) = channels.get(&channel_id) {
            let data = state.data.lock().unwrap();
            data.sender_count == 0 && data.receiver_count == 0
        } else {
            false
        };
        drop(channels);

        if should_remove {
            let mut channels = self.channels.write().unwrap();
            channels.remove(&channel_id);
        }
    }
}

// SAFETY: ChannelManager is safe to Send/Sync across threads because:
// 1. All state is protected by Mutex or is Atomic
// 2. The Block<false> contains RefCell fields (drop_stack, lisp_hashtables) but:
//    - These are only accessed during GC, which holds the gc_state mutex
//    - The mutex ensures no concurrent access to these RefCells
//    - No references to Block internals escape the mutex guard
// 3. The ThreadSafeRootSet uses Mutex internally and is 'static
// 4. HeapRoot instances in channel queues are Send/Sync
// 5. All public methods are designed to be called concurrently from multiple threads

unsafe impl Send for ChannelManager {}
unsafe impl Sync for ChannelManager {}

/// Global singleton channel manager
static CHANNEL_MANAGER: OnceLock<Arc<ChannelManager>> = OnceLock::new();

/// Get a reference to the global channel manager
pub(crate) fn get_manager() -> Arc<ChannelManager> {
    Arc::clone(CHANNEL_MANAGER.get_or_init(|| Arc::new(ChannelManager::new())))
}

/// Error type for send operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SendError {
    /// Receiver has been dropped
    Closed,
    /// Channel is full (only for try_send)
    Full,
    /// Send operation timed out
    Timeout,
}

/// Error type for receive operations
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RecvError {
    /// All senders have been dropped
    Closed,
    /// Channel is empty (only for try_recv)
    Empty,
    /// Receive operation timed out
    Timeout,
}

// Public API for channel operations
impl ChannelManager {
    pub(crate) fn new_channel_pair(&self, capacity: usize) -> (ChannelId, ChannelId) {
        let id = self.create_channel(capacity);
        (id, id)
    }
}
