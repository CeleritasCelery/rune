use rune_macros::defun;

#[defun]
fn kill_emacs() {}

defvar!(EMACS_VERSION, "27.1");
defvar!(SYSTEM_TYPE, "darwin");
defvar!(DUMP_MODE);
defvar!(COMMAND_LINE_ARGS, list![""]);
defvar!(DEFAULT_DIRECTORY, "");
defvar_bool!(NONINTERACTIVE, true);
defvar!(AFTER_INIT_TIME);
