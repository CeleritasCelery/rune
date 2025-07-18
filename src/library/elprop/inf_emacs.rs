use anyhow::{Context, Result, bail};
use fancy_regex::Regex;
use std::io::Read;
#[cfg(target_os = "windows")]
use std::net::TcpStream;
#[cfg(unix)]
use std::os::unix::net::UnixStream;
use std::{
    env,
    io::Write,
    process::{Child, Stdio},
    thread, time,
};

pub(crate) struct InferiorEmacs {
    pub socket_name: String,
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
        if self.maybe_stop_daemon().is_err() {
            eprintln!("Emacs daemon was already stopped");
        }
    }
}

impl InferiorEmacs {
    pub(crate) fn is_ready(&self) -> bool {
        self.stream.is_some()
    }

    pub(crate) fn start(&mut self) -> Result<()> {
        if self.is_ready() {
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

        let mut max_retries = 30;
        while !self.is_ready() {
            thread::sleep(time::Duration::from_millis(100));
            max_retries -= 1;
            let connect_result = self.connect_to_emacs();
            if connect_result.is_ok() {
                return Ok(());
            } else if max_retries == 0 {
                let mut stderr = String::new();
                let mut stdout = String::new();
                let mut daemon = self.maybe_stop_daemon()?;
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
                return connect_result.context(format!(
                    "Cannot start Emacs!\nEmacs daemon output:\n {stderr}\n{stdout}"
                ));
            }
        }

        unreachable!();
    }

    pub(crate) fn eval(&mut self, lisp: &str) -> Result<String> {
        if !self.is_ready() {
            bail!("Emacs daemon wasn't started");
        }

        self.connect_to_emacs()?;
        self.send(vec![Message { kind: MessageKind::Eval, body: Some(lisp.to_string()) }])?;
        let mut messages = self.receive()?.into_iter();
        let emacs_pid_msg = messages.next().context(format!("No response received for {lisp}"))?;
        if emacs_pid_msg.kind != MessageKind::EmacsPid {
            bail!(
                "Received an unexpected response. First message should be -emacs-pid, but was: {emacs_pid_msg}"
            );
        }

        let full_response = messages.fold(String::new(), |mut full_response, message| {
            if message.kind != MessageKind::Print {
                full_response.push_str(&format!("{}: ", message.kind));
            }
            if let Some(ref body) = message.body {
                full_response.push_str(body);
            }
            full_response
        });
        Ok(full_response)
    }

    fn send(&mut self, messages: Vec<Message>) -> Result<()> {
        let mut request_payload = vec![];

        #[cfg(target_os = "windows")]
        if let Some(ref auth_string) = self.auth_string {
            request_payload.extend_from_slice(format!("-auth {auth_string} ").as_bytes());
        } else {
            bail!("Cannot send message to Emacs because auth_string is not set");
        }

        request_payload.extend_from_slice("-current-frame ".as_bytes());

        for msg in messages {
            let mut msg_str = String::new();
            msg_str.push_str(&format!("-{}", msg.kind));
            if let Some(ref body) = msg.body {
                msg_str.push_str(&format!(" {}", Self::quote_argumet(body)));
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

    fn receive(&mut self) -> Result<Vec<Message>> {
        if let Some(ref mut stream) = self.stream {
            let mut raw_response = Vec::<u8>::new();
            stream.read_to_end(&mut raw_response)?;
            let raw_response = String::from_utf8_lossy(&raw_response);

            let mut messages = Vec::<Message>::new();
            let raw_messages = raw_response.split('\n').filter(|s| !s.is_empty());
            for m in raw_messages {
                let raw = Self::unquote_argument(m)?;
                let msg =
                    Message::from(&raw).context(format!("Cannot parse Emacs message: '{raw}'"))?;
                messages.push(msg);
            }

            Ok(messages)
        } else {
            bail!("Cannot receive messages because stream wasn't established");
        }
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
    fn unquote_argument(quoted_lisp: &str) -> Result<String> {
        let mut result = String::new();
        let mut iter = quoted_lisp.chars();
        while let Some(c) = iter.next() {
            if c == '&' {
                let c = iter
                    .next()
                    .with_context(|| panic!("Malformed message from Emacs: {}", &quoted_lisp))?;
                result.push(match c {
                    '_' => ' ',
                    'n' => '\n',
                    _ => c,
                })
            } else {
                result.push(c);
            }
        }
        Ok(result)
    }
}

#[cfg(target_os = "windows")]
impl InferiorEmacs {
    fn maybe_stop_daemon(&mut self) -> Result<Child> {
        let daemon = self.daemon.take().context("Emacs wasn't started")?;
        std::process::Command::new("taskkill")
            .args(["/F", "/T", "/PID", &daemon.id().to_string()])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .status()
            .context("Cannot run taskkill to kill Emacs daemon")?;
        self.stream.take();
        Ok(daemon)
    }

    // Content of the Emacs server file on Windows looks like this:
    // 127.0.0.1:51022 8120
    // <64 char long password>
    fn connect_to_emacs(&mut self) -> Result<()> {
        use std::fs;

        let server_file_path = format!("{}\\{}", self.socket_dir()?, self.socket_name);
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
        Ok(())
    }

    fn socket_dir(&self) -> Result<String> {
        let appdata = env::var("APPDATA").context("APPDATA is not defined")?;
        Ok(format!("{appdata}\\.emacs.d\\server"))
    }
}

#[cfg(unix)]
impl InferiorEmacs {
    fn maybe_stop_daemon(&mut self) -> Result<Child> {
        let mut daemon = self.daemon.take().context("Emacs wasn't started")?;
        daemon.kill().context("Emacs couldn't be killed")?;
        self.stream.take();
        Ok(daemon)
    }

    fn connect_to_emacs(&mut self) -> Result<()> {
        let socket_path = format!("{}/{}", self.socket_dir()?, self.socket_name);
        self.stream = Some(UnixStream::connect(&socket_path).context("Cannot connect to Emacs")?);
        Ok(())
    }

    #[cfg(target_os = "linux")]
    fn socket_dir(&self) -> Result<String> {
        let xdg_runtime_dir =
            env::var("XDG_RUNTIME_DIR").context("XDG_RUNTIME_DIR is not defined")?;
        Ok(format!("{xdg_runtime_dir}/emacs"))
    }

    #[cfg(target_os = "macos")]
    fn socket_dir(&self) -> Result<String> {
        let uid = users::get_current_uid();
        let tmpdir = env::var("TMPDIR").unwrap_or_else(|_| "/tmp".to_string());
        Ok(format!("{}/emacs{}", tmpdir, uid))
    }
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
    fn from(s: &str) -> Result<Self> {
        let err = || format!("Malformed message: Expected '-(kind) <body>' format, got: {s}");

        let kind_re = Regex::new("^-([^ ]+) ")?;
        let raw_kind = kind_re.captures(s)?.with_context(err)?.get(1).with_context(err)?.as_str();
        let kind = match raw_kind {
            "emacs-pid" => MessageKind::EmacsPid,
            "print" | "print-nonl" => MessageKind::Print,
            "error" => MessageKind::Error,
            _ => panic!("Unknown message kind: {raw_kind}"),
        };
        let body = kind_re
            .split(s)
            .last()
            .with_context(err)??
            .replace("\n   ", "")
            .trim_end_matches("\n")
            .to_string();

        Ok(Message { kind, body: Some(body) })
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
