use fn_macros::defun;

#[defun]
fn kill_emacs() -> bool {
    false
}

defvar!(EMACS_VERSION, "27.1");
defvar!(SYSTEM_TYPE, "darwin");
defvar!(DUMP_MODE);
defvar!(COMMAND_LINE_ARGS, list!(""));
defvar!(DEFAULT_DIRECTORY, "");
defvar!(NONINTERACTIVE, true);

define_symbols!(
    FUNCS => {
        kill_emacs,
    }
    VARS => {
        EMACS_VERSION,
        SYSTEM_TYPE,
        DUMP_MODE,
        COMMAND_LINE_ARGS,
        DEFAULT_DIRECTORY,
        NONINTERACTIVE
    }
);
