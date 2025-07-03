use anyhow::{Result, bail};
use std::{
    process::{Child, Stdio},
    thread::sleep,
    time,
};

pub(crate) struct InferiorEmacs {
    pub socket_name: String,
    emacs_started: bool,
    daemon: Option<Child>,
}

impl Default for InferiorEmacs {
    fn default() -> Self {
        Self { socket_name: String::from("rune-test"), emacs_started: false, daemon: None }
    }
}

impl InferiorEmacs {
    pub fn new() -> Self {
        InferiorEmacs { socket_name: String::from("rune-test"), ..InferiorEmacs::default() }
    }

    pub fn start_daemon(&mut self) -> Result<()> {
        if self.emacs_started {
            return Ok(());
        }

        let fg_daemon_arg = format!("--fg-daemon={}", &self.socket_name);
        self.daemon = Some(
            std::process::Command::new("emacs")
                .args(["-Q", &fg_daemon_arg, "--eval", "(setq debug-on-error t)"])
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?,
        );

        let mut max_retries = 20;
        while !self.emacs_started {
            max_retries -= 1;
            if max_retries == 0 {
                bail!("Cannot start Emacs daemon");
            }

            sleep(time::Duration::from_millis(50));
            self.emacs_started = std::process::Command::new("emacsclient")
                .args(["--socket-name", &self.socket_name])
                .args(["--eval", "(version)"])
                .output()
                .is_ok_and(|output| output.status.code().is_some_and(|c| c == 0));
        }

        Ok(())
    }

    pub fn eval(&mut self, lisp: &str) -> Result<String> {
        if !self.emacs_started {
            bail!("Emacs daemon wasn't started");
        }
        let emacs_client = std::process::Command::new("emacsclient")
            .args(["--socket-name", &self.socket_name])
            .args(["--eval", lisp])
            .output()
            .expect("Failed to run emacsclient");
        if emacs_client.status.code().unwrap() != 0 {
            let client_output = String::from_utf8_lossy(&emacs_client.stderr);
            eprintln!("{}", &client_output);
            let client_output = String::from_utf8_lossy(&emacs_client.stdout);
            eprintln!("{}", &client_output);
        }
        let output = String::from_utf8_lossy(&emacs_client.stdout);
        Ok(output.trim_end_matches("\n").to_string())
    }
}

#[macro_export]
macro_rules! assert_elprop {
    ($($form:tt)*) => {{
        let lisp_form = format!($($form)*);
        let mut inf_emacs = crate::library::elprop::InferiorEmacs::new();
        inf_emacs.start_daemon().unwrap();
        let emacs_result = inf_emacs.eval(&lisp_form).unwrap();

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
