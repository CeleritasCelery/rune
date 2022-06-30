use fn_macros::defun;

#[defun]
fn kill_emacs() -> bool {
    false
}

defvar!(EMACS_VERSION, "emacs-version", "27.1");
defvar!(SYSTEM_TYPE, "system-type", "darwin");
defvar!(DUMP_MODE, "dump-mode");

defsubr!(kill_emacs; VARS => {EMACS_VERSION, SYSTEM_TYPE, DUMP_MODE});
