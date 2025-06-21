#[repr(u64)]
enum CharBits {
    Alt = 0x0400000,
    Super = 0x0800000,
    Hyper = 0x1000000,
    Shift = 0x2000000,
    Ctl = 0x4000000,
    Meta = 0x8000000,
}

pub const CHAR_MODIFIER_MASK: u64 = {
    CharBits::Alt as u64
        | CharBits::Super as u64
        | CharBits::Hyper as u64
        | CharBits::Shift as u64
        | CharBits::Ctl as u64
        | CharBits::Meta as u64
};

// Check quit-flag and quit if it is non-nil.  Typing C-g does not
// directly cause a quit; it only sets Vquit_flag.  So the program
// needs to call maybe_quit at times when it is safe to quit.  Every
// loop that might run for a long time or might not exit ought to call
// maybe_quit at least once, at a safe place.  Unless that is
// impossible, of course.  But it is very desirable to avoid creating
// loops where maybe_quit is impossible.
//
// If quit-flag is set to `kill-emacs' the SIGINT handler has received
// a request to exit Emacs when it is safe to do.
//
// When not quitting, process any pending signals.
pub fn maybe_quit() {
    // TODO: Implement maybe_quit
}
