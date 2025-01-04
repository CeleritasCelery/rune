//! String and character case conversion.
use rune_macros::defun;

#[defun]
fn capitalize(s: &str) -> String {
    titlecase::titlecase(s)
}

#[defun]
fn upcase(s: &str) -> String {
    s.to_uppercase()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_upcase() {
        // Basic escape characters
        assert_eq!("\n", upcase("\n"));
        assert_eq!("\t", upcase("\t"));
        assert_eq!("\r", upcase("\r"));

        // Control characters
        assert_eq!("\u{0}", upcase("\u{0}"));
        assert_eq!("\u{1B}", upcase("\u{1B}"));
        assert_eq!("\u{7F}", upcase("\u{7F}"));

        // Non-ASCII characters
        assert_eq!("ΑΒΓ", upcase("αβγ"));
        assert_eq!("ÅÄÖ", upcase("åäö"));

        // Mixed content
        assert_eq!("HELLO\nWORLD", upcase("hello\nworld"));
        assert_eq!("FOO\tBAR", upcase("foo\tbar"));
        assert_eq!("PATH\\TO\\FILE\"NAME\"", upcase("path\\to\\file\"name\""));
    }
}
