//! Multi-threaded elisp support.
use crate::core::{
    env::Env,
    gc::{Block, Context, RootSet},
    object::{CloneIn, GcObj},
};
use rune_core::macros::root;
use rune_macros::defun;
use std::thread::{self, JoinHandle};

#[defun]
fn go(obj: GcObj) {
    go_internal(obj);
}

fn go_internal(obj: GcObj) -> JoinHandle<()> {
    let block = Block::new_local_unchecked();
    let sexp = obj.clone_in(&block);
    let raw = sexp.into_raw();
    crate::debug::enable_debug();
    thread::spawn(move || {
        let roots = &RootSet::default();
        let cx = &mut Context::from_block(block, roots);
        root!(env, Env::default(), cx);
        let obj = unsafe { GcObj::from_raw(raw) };
        root!(obj, cx);
        _ = crate::interpreter::eval(obj, None, env, cx);
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_go() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let obj = cx.add("test string");
        go_internal(obj).join().unwrap();
    }

    #[test]
    fn test_go_eval() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let threads = [
            go_internal(crate::reader::read("(if nil 1 2 3)", cx).unwrap().0),
            go_internal(crate::reader::read("(progn (defvar foo 1) foo)", cx).unwrap().0),
            go_internal(crate::reader::read("(progn (defvar foo 1) (makunbound 'foo) (let ((fn #'(lambda () (defvar foo 3))) (foo 7)) (funcall fn)) foo)", cx).unwrap().0),
        ];
        for thread in threads {
            thread.join().unwrap();
        }
    }

    #[test]
    fn test_go_message() {
        let roots = &RootSet::default();
        println!("hello main thread");
        let cx = &mut Context::new(roots);
        let obj = crate::reader::read("(message \"hello from thread\")", cx).unwrap().0;
        go_internal(obj).join().unwrap();
    }
}
