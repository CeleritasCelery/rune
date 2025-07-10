use anyhow::{Context, Result, bail};
use fancy_regex::Regex;
use std::io::Read;
#[cfg(target_os = "windows")]
use std::net::TcpStream;
#[cfg(unix)]
use std::os::unix::net::UnixStream;
use std::{
    cell::RefCell,
    env,
    io::Write,
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
    #[cfg(unix)]
    stream: Option<UnixStream>,
    #[cfg(target_os = "windows")]
    stream: Option<TcpStream>,
    #[cfg(target_os = "windows")]
    auth_string: Option<String>,
}

impl Default for InferiorEmacs {
    fn default() -> Self {
        Self {
            socket_name: format!("rune-proptest-{:?}", thread::current().id()),
            emacs_started: false,
            daemon: None,
            stream: None,
            #[cfg(target_os = "windows")]
            auth_string: None,
        }
    }
}

#[cfg(target_os = "windows")]
impl Drop for InferiorEmacs {
    fn drop(&mut self) {
        self.maybe_stop_daemon();
    }
}

impl InferiorEmacs {
    pub fn send(&mut self, messages: Vec<Message>) -> Result<()> {
        let mut request_payload = vec![];

        #[cfg(target_os = "windows")]
        if let Some(ref auth_string) = self.auth_string {
            request_payload
                .extend_from_slice(format!("-auth {auth_string} -current-frame").as_bytes());
        } else {
            bail!("Cannot send message to Emacs because auth_string is not set");
        }

        #[cfg(unix)]
        request_payload.extend_from_slice("-current-frame".as_bytes());

        for msg in messages {
            let mut msg_str = String::new();
            msg_str.push_str(&format!(" -{}", msg.kind));
            if let Some(ref body) = msg.body {
                msg_str.push_str(&format!(" {}", quote_argumet(body)));
            }
            request_payload.extend_from_slice(msg_str.as_bytes());
        }
        request_payload.extend_from_slice("\n".as_bytes());
        if let Some(ref mut stream) = self.stream {
            stream.write_all(&request_payload).context("Cannot write to Emacs socket")?;
            stream.flush().context("Cannot flush Emacs socket")?;
            Ok(())
        } else {
            bail!("Cannot send a message because stream wasn't established");
        }
    }

    fn recv_messages(&mut self) -> Result<Vec<Message>> {
        if let Some(ref mut stream) = self.stream {
            let mut buf = Vec::<u8>::new();
            stream.read_to_end(&mut buf)?;

            Ok(String::from_utf8_lossy(&buf)
                .split('\n')
                .filter(|s| !s.is_empty())
                .map(|s| Message::from(&unquote_argument(s)))
                .filter(|m| m.kind == MessageKind::Print || m.kind == MessageKind::Error)
                .collect())
        } else {
            bail!("Cannot receive messages because stream wasn't established");
        }
    }
}

#[cfg(target_os = "windows")]
impl InferiorEmacs {
    pub fn maybe_stop_daemon(&mut self) -> Option<Child> {
        if let Some(daemon) = self.daemon.take() {
            if self.emacs_started {
                std::process::Command::new("taskkill")
                    .args(["/F", "/T", "/PID", &daemon.id().to_string()])
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped())
                    .status()
                    .expect("Cannot run taskkill to kill Emacs daemon");
                self.emacs_started = false;
            }
            return Some(daemon);
        }
        None
    }

    // Content of the Emacs server file on Windows looks like this:
    // 127.0.0.1:51022 8120
    // <64 char long password>
    fn connect_to_emacs(&mut self) -> Result<()> {
        use std::fs;

        let server_file_path = format!("{}\\{}", self.socket_dir(), self.socket_name);
        let mut server_file = fs::File::open(server_file_path.clone())
            .with_context(|| format!("Cannot open Emacs server file at {server_file_path}"))?;
        let mut server_file_content = String::new();
        server_file
            .read_to_string(&mut server_file_content)
            .context("Cannot read Emacs server file at {server_file_path}")?;
        let malformed_ctx =
            || format!("Malformed emacs server file content:\n {server_file_content}");
        let mut server_file_lines = server_file_content.split("\n");
        let addr = server_file_lines
            .next()
            .with_context(malformed_ctx)?
            .split(" ")
            .next()
            .with_context(malformed_ctx)?;
        self.auth_string = Some(server_file_lines.next().with_context(malformed_ctx)?.to_string());
        self.stream = Some(std::net::TcpStream::connect(addr).context("Cannot connect to Emacs")?);
        self.emacs_started = true;
        Ok(())
    }

    fn socket_dir(&self) -> String {
        let appdata = env::var("APPDATA").unwrap();
        format!("{appdata}\\.emacs.d\\server")
    }
}

#[cfg(unix)]
impl InferiorEmacs {
    pub fn maybe_stop_daemon(&mut self) -> Option<Child> {
        if let Some(mut daemon) = self.daemon.take() {
            if self.emacs_started {
                daemon.kill().expect("Emacs couldn't be killed");
            }
            return Some(daemon);
        }
        None
    }

    pub fn connect_to_emacs(&mut self) -> Result<()> {
        let socket_path = format!("{}/{}", self.socket_dir(), self.socket_name);
        self.stream = Some(UnixStream::connect(&socket_path).context("Cannot connect to Emacs")?);
        self.emacs_started = true;
        Ok(())
    }

    #[cfg(target_os = "linux")]
    fn socket_dir(&self) -> String {
        let xdg_runtime_dir = env::var("XDG_RUNTIME_DIR").unwrap();
        format!("{xdg_runtime_dir}/emacs")
    }

    #[cfg(target_os = "macos")]
    fn socket_dir(&self) -> String {
        let uid = users::get_current_uid();
        let tmpdir = env::var("TMPDIR").or::<String>(Ok("/tmp".to_string())).unwrap();
        format!("{}/emacs{}", tmpdir, uid)
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

        let mut max_retries = 3;
        while !inf_emacs.emacs_started {
            thread::sleep(time::Duration::from_millis(50));
            max_retries -= 1;
            let connect_result = inf_emacs.connect_to_emacs();
            if connect_result.is_ok() {
                return Ok(());
            } else if max_retries == 0 {
                let mut stderr = String::new();
                let mut stdout = String::new();
                if let Some(mut daemon) = inf_emacs.maybe_stop_daemon() {
                    if let Some(mut s) = daemon.stdout.take() {
                        let mut buf = Vec::<u8>::new();
                        let _ = s.read_to_end(&mut buf);
                        stdout.push_str(&String::from_utf8_lossy(&buf));
                    };
                    if let Some(mut s) = daemon.stderr.take() {
                        let mut buf = Vec::<u8>::new();
                        let _ = s.read_to_end(&mut buf);
                        stderr.push_str(&String::from_utf8_lossy(&buf));
                    };
                }
                return connect_result.context(format!(
                    "Cannot start Emacs!\nEmacs daemon output:\n {stderr}\n{stdout}"
                ));
            }
        }

        unreachable!();
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

pub fn eval(lisp: &str) -> Result<String> {
    INFERIOR_EMACS.with_borrow_mut(|inf_emacs| {
        if !inf_emacs.emacs_started {
            bail!("Emacs daemon wasn't started");
        }

        inf_emacs.connect_to_emacs()?;
        inf_emacs.send(vec![Message { kind: MessageKind::Eval, body: Some(lisp.to_string()) }])?;
        let messages = inf_emacs.recv_messages()?;
        let full_response = messages.iter().fold(String::new(), |mut full_response, message| {
            if let Some(ref body) = message.body {
                full_response.push_str(body);
            }
            full_response
        });
        Ok(full_response)
    })
}

#[derive(Debug, PartialEq)]
enum MessageKind {
    EmacsPid,
    Print,
    Error,
    Eval,
}

impl std::fmt::Display for MessageKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MessageKind::EmacsPid => write!(f, "emacs-pid"),
            MessageKind::Print => write!(f, "print"),
            MessageKind::Error => write!(f, "error"),
            MessageKind::Eval => write!(f, "eval"),
        }
    }
}

#[derive(Debug)]
struct Message {
    kind: MessageKind,
    body: Option<String>,
}

impl Message {
    pub fn from(s: &str) -> Self {
        let kind_re = Regex::new("^-([^ ]+) ").unwrap();
        let raw_kind = kind_re.captures(s).unwrap().unwrap().get(1).unwrap().as_str();
        let kind = match raw_kind {
            "emacs-pid" => MessageKind::EmacsPid,
            "print" | "print-nonl" => MessageKind::Print,
            "error" => MessageKind::Error,
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
        Message { kind, body: Some(body) }
    }
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "kind: {}, body: ", self.kind)?;
        if let Some(ref body) = self.body {
            write!(f, "{body}")
        } else {
            write!(f, "N/A")
        }
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
    #[cfg_attr(miri, ignore)]
    fn test_eval() {
        start_emacs().unwrap();
        let result = eval("(+ 40 2)").unwrap();
        assert_eq!("42", result);
        assert_elprop!["(make-vector 250 ?a)"];
    }
}
