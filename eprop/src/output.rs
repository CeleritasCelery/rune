use std::fs::File;
use std::io::Write;




pub(crate) enum Output {
    Stdout(String),
    File(File),
}

impl Output {
    pub(crate) fn new(path: Option<std::path::PathBuf>) -> Self {
        match path {
            Some(path) => {
                let file = File::create(path).unwrap();
                Output::File(file)
            }
            None => Output::Stdout(String::new()),
        }
    }
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Output::Stdout(string) => {
                let buf = std::str::from_utf8(buf).unwrap();
                string.push_str(buf);
                Ok(buf.len())
            }
            Output::File(file) => {
                file.write(buf)
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Output::Stdout(buf) => {
                print!("{}", buf);
                std::io::stdout().flush()?;
                Ok(())
            }
            Output::File(file) => {
                file.flush()
            }
        }
    }
}
