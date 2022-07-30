use fn_macros::defun;

#[defun]
fn kill_emacs() -> bool {
    false
}

defvar!(EMACS_VERSION, "emacs-version", "27.1");
defvar!(SYSTEM_TYPE, "system-type", "darwin");
defvar!(DUMP_MODE, "dump-mode");
defvar!(COMMAND_LINE_ARGS, "command-line-args", list!(""));
defvar!(DEFAULT_DIRECTORY, "default-directory", "");
defvar!(NONINTERACTIVE, "noninteractive", true);

defsubr!(
    kill_emacs;
    VARS => {
        EMACS_VERSION,
        SYSTEM_TYPE,
        DUMP_MODE,
        COMMAND_LINE_ARGS,
        DEFAULT_DIRECTORY,
        NONINTERACTIVE
    }
);
