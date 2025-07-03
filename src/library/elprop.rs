use anyhow::{Result, bail};
use num_bigint::BigInt;
use proptest::prelude::*;
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
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
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
            .args(["--eval", dbg!(lisp)])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("Failed to run emacsclient");
        let output = String::from_utf8_lossy(&emacs_client.stdout);
        Ok(output.trim_end_matches("\n").to_string())
    }
}

impl std::fmt::Display for ArbitraryType {
    #[expect(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::string::ToString;
        match self {
            ArbitraryType::String(s) => {
                write!(f, "\"")?;
                for c in s.chars() {
                    match c {
                        '\0' => write!(f, "\\\\0")?,
                        '\n' => write!(f, "\\n")?,
                        '\t' => write!(f, "\\t")?,
                        '\r' => write!(f, "\\r")?,
                        '\\' => write!(f, "\\\\")?,
                        '\"' => write!(f, "\\\"")?,
                        c => write!(f, "{c}")?,
                    }
                }
                write!(f, "\"")
            }
            ArbitraryType::Float(n) => {
                if n.fract() == 0.0_f64 {
                    write!(f, "{n:.1}")
                } else {
                    write!(f, "{n}")
                }
            }
            ArbitraryType::Cons(list) => {
                let mut cells: Vec<_> = list.0.iter().map(ToString::to_string).collect();
                let len = list.0.len();
                let dot_end = list.1;
                if dot_end && len >= 2 {
                    cells.insert(len - 1, ".".to_owned());
                }
                let string = cells.join(" ");
                write!(f, "'({string})")
            }
            ArbitraryType::Symbol(s) => write!(f, "'{s}"),
            ArbitraryType::Integer(n) => write!(f, "{n}"),
            ArbitraryType::Boolean(b) => {
                if *b {
                    write!(f, "t")
                } else {
                    write!(f, "nil")
                }
            }
            ArbitraryType::Unknown(obj) => write!(f, "{obj}"),
            ArbitraryType::UnibyteString(s) => {
                write!(f, "\"")?;
                for c in s.chars() {
                    match c {
                        '\n' => write!(f, "\\n")?,
                        '\t' => write!(f, "\\t")?,
                        '\r' => write!(f, "\\r")?,
                        '\\' => write!(f, "\\\\")?,
                        '"' => write!(f, "\\\"")?,
                        c => write!(f, "{c}")?,
                    }
                }
                write!(f, "\"")
            }
            ArbitraryType::Nil => write!(f, "nil"),
            ArbitraryType::Vector(vec) => {
                let cells: Vec<_> = vec.iter().map(ToString::to_string).collect();
                let string = cells.join(" ");
                write!(f, "[{string}]")
            }
            ArbitraryType::HashTable(vec) => {
                write!(f, "#s(hash-table data (")?;
                for (key, value) in vec {
                    write!(f, "{key} {value} ")?;
                }
                write!(f, "))")
            }
            ArbitraryType::Record((name, members)) => {
                let cells: Vec<_> = members.iter().map(ToString::to_string).collect();
                let string = cells.join(" ");
                write!(f, "(record '{name} {string})")
            }
            ArbitraryType::Function(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryType::ByteFn(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryType::Byte(n) => write!(f, "{n}"),
            ArbitraryType::Buffer(name) => {
                write!(f, "(generate-new-buffer {name})")
            }
            ArbitraryType::Subr(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryType::Char(chr) => match chr {
                '\n' => write!(f, "?\\n"),
                '\t' => write!(f, "?\\t"),
                '\r' => write!(f, "?\\r"),
                '\u{0B}' => write!(f, "?\\v"),
                '\u{0C}' => write!(f, "?\\f"),
                '\u{1B}' => write!(f, "?\\e"),
                '\u{7F}' => write!(f, "?\\d"),
                '\u{08}' => write!(f, "?\\b"),
                '\u{07}' => write!(f, "?\\a"),
                '(' | ')' | '[' | ']' | '\\' | '"' => write!(f, "?\\{chr}"),
                chr => write!(f, "?{chr}"),
            },
            ArbitraryType::BigInt(n) => write!(f, "{n}"),
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub(crate) enum ArbitraryType {
    String(String),
    Float(f64),
    Cons((Vec<ArbitraryType>, bool)),
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Unknown(Box<ArbitraryType>),
    UnibyteString(String),
    Vector(Vec<ArbitraryType>),
    HashTable(Vec<(ArbitraryType, ArbitraryType)>),
    Record((String, Vec<ArbitraryType>)),
    Nil,
    Function(u8),
    ByteFn(u8),
    Byte(u8),
    Char(char),
    Buffer(String),
    Subr(u8),
    BigInt(BigInt),
}

pub(crate) fn arb_custom_string(string: &str) -> BoxedStrategy<ArbitraryType> {
    proptest::string::string_regex(&string)
        .expect("Invalid proptest regex")
        .prop_map(ArbitraryType::String)
        .boxed()
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
