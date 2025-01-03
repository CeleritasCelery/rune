//! String and character case conversion.
use rune_macros::defun;

#[defun]
fn capitalize(s: &str) -> String {
    titlecase::titlecase(s)
}

#[defun]
fn upcase(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '\u{10D40}'..='\u{10D8F}' => result.push(c), // ignores range not supported by Emacs yet
            _ => {
                for uc in c.to_uppercase() {
                    result.push(uc);
                }
            }
        }
    }
    result.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_characters_in_garay_unicode_block() {
        assert_eq!("𐵰", upcase("𐵰"));
        assert_eq!("𐵪", upcase("𐵪"));
    }
}
