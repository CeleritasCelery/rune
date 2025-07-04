use anyhow::{Result, bail};
use std::{
    cell::RefCell,
    process::{Child, Stdio},
    thread, time,
};

mod strategies;
pub(crate) use strategies::*;

thread_local! {
    static INFERIOR_EMACS: RefCell<InferiorEmacs> = RefCell::new(InferiorEmacs::default());
}

struct InferiorEmacs {
    pub socket_name: String,
    emacs_started: bool,
    daemon: Option<Child>,
}

impl Default for InferiorEmacs {
    fn default() -> Self {
        Self {
            socket_name: format!("rune-proptest-{:?}", thread::current().id()),
            emacs_started: false,
            daemon: None,
        }
    }
}

pub fn start_emacs() -> Result<()> {
    INFERIOR_EMACS.with_borrow_mut(|inf_emacs| {
        if inf_emacs.emacs_started {
            return Ok(());
        }

        let fg_daemon_arg = format!("--fg-daemon={}", &inf_emacs.socket_name);
        inf_emacs.daemon = Some(
            std::process::Command::new("emacs")
                .args(["-Q", &fg_daemon_arg, "--eval", "(setq debug-on-error t)"])
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?,
        );

        let mut max_retries = 20;
        while !inf_emacs.emacs_started {
            max_retries -= 1;
            if max_retries == 0 {
                bail!("Cannot start Emacs daemon");
            }

            thread::sleep(time::Duration::from_millis(50));
            inf_emacs.emacs_started = std::process::Command::new("emacsclient")
                .args(["--socket-name", &inf_emacs.socket_name])
                .args(["--eval", "(version)"])
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .output()
                .is_ok_and(|output| output.status.code().is_some_and(|c| c == 0));
        }

        Ok(())
    })
}

pub fn eval(lisp: &str) -> Result<String> {
    INFERIOR_EMACS.with_borrow_mut(|inf_emacs| {
        if !inf_emacs.emacs_started {
            bail!("Emacs daemon wasn't started");
        }
        let emacs_client = std::process::Command::new("emacsclient")
            .args(["--socket-name", &inf_emacs.socket_name])
            .args(["--eval", lisp])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("Failed to run emacsclient");
        let output = String::from_utf8_lossy(&emacs_client.stdout);
        // I think emacsclient wraps the output by 80 columns, so we have to undo this
        Ok(output.replace("\n   ", "").trim_end_matches("\n").to_string())
    })
}

#[macro_export]
macro_rules! assert_elprop {
    ($($form:tt)*) => {{
        let lisp_form = format!($($form)*);
        crate::library::elprop::start_emacs().unwrap();
        let emacs_result = crate::library::elprop::eval(&lisp_form).unwrap();

        let roots = &crate::core::gc::RootSet::default();
        let cx = &mut crate::core::gc::Context::new(roots);
        crate::core::env::sym::init_symbols();
        rune_core::macros::root!(env, init(crate::core::env::Env::default()), cx);
        let rune_result = {
            let obj = crate::reader::read(&lisp_form, cx).unwrap().0;
            rune_core::macros::root!(obj, cx);
            match crate::interpreter::eval(obj, None, env, cx) {
                Ok(val) => format!("{val}"),
                Err(e) => format!("Error: {e}"),
            }
        };
        if !(rune_result == emacs_result) {
            panic!(
                r#"assertion `Rune vs Emacs` failed:
  Rune: {rune_result}
 Emacs: {emacs_result}"#
            )
        }
    };};
}
