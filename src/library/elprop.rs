use anyhow::{Result, bail};
use fancy_regex::Regex;
use std::io::{Read, Write};
#[cfg(target_os = "windows")]
use std::net::TcpStream;
#[cfg(unix)]
use std::os::unix::net::UnixStream;
use std::{
    cell::RefCell,
    env,
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
                let mut emacs_output = String::new();
                let mut buf = String::new();
                if let Some(daemon) = inf_emacs.daemon.take() {
                    if let Some(mut stderr) = daemon.stderr {
                        let _ = stderr.read_to_string(&mut buf);
                    }
                    emacs_output.push_str(&buf);
                    if let Some(mut stdout) = daemon.stdout {
                        let _ = stdout.read_to_string(&mut buf);
                    }
                    emacs_output.push_str(&buf);
                    bail!("Cannot start Emacs daemon. Emacs output:\n{emacs_output}");
                }
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

// In STR, insert a & before each &, each space, each newline, and any
// initial -.  Change spaces to underscores, too, so that the return
// value never contains a space.
//
// Does not change the string.  Outputs the result to S.
fn quote_argumet(lisp: &str) -> String {
    lisp.char_indices().fold(String::new(), |mut result, (i, c)| {
        let mut qc = String::new();
        if i == 0 && c == '-' {
            qc.push_str("&-");
        } else if c == ' ' {
            qc.push_str("&_");
        } else if c == '\n' {
            qc.push_str("&n");
        } else if c == '&' {
            qc.push_str("&&");
        } else {
            qc.push(c);
        }
        result.push_str(&qc);
        result
    })
}

// The inverse of quote_argument.  Remove quoting in string STR by
// modifying the addressed string in place.  Return STR.
fn unquote_argument(quoted_lisp: &str) -> String {
    let mut result = String::new();
    let mut iter = quoted_lisp.chars();
    while let Some(c) = iter.next() {
        if c == '&' {
            let c = iter
                .next()
                .unwrap_or_else(|| panic!("Malformed message from Emacs: {}", &quoted_lisp));
            result.push(match c {
                '_' => ' ',
                'n' => '\n',
                _ => c,
            })
        } else {
            result.push(c);
        }
    }
    result
}

#[cfg(target_os = "linux")]
fn socket_dir() -> String {
    let xdg_runtime_dir = env::var("XDG_RUNTIME_DIR").unwrap();
    format!("{xdg_runtime_dir}/emacs")
}

#[cfg(target_os = "macos")]
fn socket_dir() -> String {
    let uid = users::get_current_uid();
    let tmpdir = env::var("TMPDIR").or::<String>(Ok("/tmp".to_string())).unwrap();
    format!("{}/emacs{}", tmpdir, uid)
}

#[cfg(target_os = "windows")]
fn socket_dir() -> String {
    let appdata = env::var("APPDATA").unwrap();
    format!("{appdata}\\emacs\\server")
}

#[cfg(target_family = "unix")]
fn build_stream(socket_name: &str) -> UnixStream {
    let socket_path = format!("{}/{}", socket_dir(), socket_name);
    UnixStream::connect(&socket_path).unwrap()
}

#[cfg(target_os = "windows")]
fn build_stream(socket_name: &str) -> TcpStream {
    let server_file_path = format!("{}/{}", socket_dir(), socket_name);

    // The server file might not be immediately available after start.
    // We'll retry a few times to read it.
    let server_info = (0..20)
        .find_map(|_| {
            thread::sleep(time::Duration::from_millis(50));
            std::fs::read_to_string(&server_file_path).ok()
        })
        .unwrap_or_else(|| panic!("Could not read emacs server file: {server_file_path}"));

    let addr = server_info
        .split_whitespace()
        .next()
        .unwrap_or_else(|| panic!("Malformed emacs server file content: {server_info}"));
    std::net::TcpStream::connect(addr).unwrap()
}

pub fn eval(lisp: &str) -> Result<String> {
    INFERIOR_EMACS.with_borrow_mut(|inf_emacs| {
        if !inf_emacs.emacs_started {
            bail!("Emacs daemon wasn't started");
        }

        let mut stream = build_stream(&inf_emacs.socket_name);
        let mut request_payload = Vec::new();
        let lisp = quote_argumet(lisp);
        request_payload.extend_from_slice(format!("-eval {lisp}\n").as_bytes());
        stream.write_all(&request_payload)?;
        stream.flush()?;

        let mut buf = Vec::<u8>::new();
        let _ = stream.read_to_end(&mut buf)?;
        let response = &String::from_utf8_lossy(&buf);

        let messages = response
            .split('\n')
            .filter(|s| !s.is_empty())
            .map(|s| Message::from(&unquote_argument(s)))
            .filter(|m| m.kind == MessageKind::Print);
        let full_response = messages.map(|m| m.body).collect::<String>();
        Ok(full_response)
    })
}

#[derive(Debug, PartialEq)]
enum MessageKind {
    EmacsPid,
    Print,
}

#[derive(Debug)]
struct Message {
    kind: MessageKind,
    body: String,
}

impl Message {
    pub fn from(s: &str) -> Self {
        let kind_re = Regex::new("^-([^ ]+) ").unwrap();
        let raw_kind = kind_re.captures(s).unwrap().unwrap().get(1).unwrap().as_str();
        let kind = match raw_kind {
            "emacs-pid" => MessageKind::EmacsPid,
            "print" | "print-nonl" => MessageKind::Print,
            _ => panic!("Unknown message kind: {raw_kind}"),
        };
        let body = kind_re
            .split(s)
            .last()
            .unwrap()
            .unwrap()
            .replace("\n   ", "")
            .trim_end_matches("\n")
            .to_string();
        Message { kind, body }
    }
}

#[macro_export]
macro_rules! assert_elprop {
    ($($form:tt)*) => {{
        let lisp_form = format!($($form)*);
        $crate::library::elprop::start_emacs().unwrap();
        let emacs_result = $crate::library::elprop::eval(&lisp_form).unwrap();

        let roots = &$crate::core::gc::RootSet::default();
        let cx = &mut $crate::core::gc::Context::new(roots);
        $crate::core::env::sym::init_symbols();
        rune_core::macros::root!(env, init($crate::core::env::Env::default()), cx);
        let rune_result = {
            let obj = $crate::reader::read(&lisp_form, cx).unwrap().0;
            rune_core::macros::root!(obj, cx);
            match $crate::interpreter::eval(obj, None, env, cx) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        start_emacs().unwrap();
        let result = eval("(+ 40 2)").unwrap();
        assert_eq!("42", result);
        assert_elprop!["(make-vector 250 ?a)"];
    }
}
