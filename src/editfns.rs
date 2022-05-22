use fn_macros::defun;

use crate::core::object::GcObj;
use anyhow::{anyhow, ensure, Result};

#[defun]
fn message(format_string: &str, args: &[GcObj]) -> String {
    println!("MESSAGE: {format_string} -> {args:?}");
    format_string.to_owned()
}

#[defun]
fn format(string: &str, objects: &[GcObj]) -> Result<String> {
    let mut result = String::new();
    let mut last_end = 0;
    let mut iter = objects.iter();
    for (start, part) in string.match_indices("%s") {
        result.push_str(&string[last_end..start]);
        let val = iter
            .next()
            .ok_or_else(|| anyhow!("Not enough objects for format string"))?;
        result.push_str(&format!("{val}"));
        last_end = start + part.len();
    }
    ensure!(
        iter.next().is_none(),
        "Too many arguments for format string"
    );
    result.push_str(&string[last_end..string.len()]);
    Ok(result)
}

defsubr!(message, format);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_format() {
        assert_eq!(&format("%s", &[1.into()]).unwrap(), "1");
        assert_eq!(&format("foo-%s", &[2.into()]).unwrap(), "foo-2");
        assert_eq!(
            &format("foo-%s %s", &[3.into(), 4.into()]).unwrap(),
            "foo-3 4"
        );
        assert_eq!(
            &format("%s", &[(&crate::core::env::sym::FUNCTION).into()]).unwrap(),
            "function"
        );

        assert!(&format("%s", &[]).is_err());
        assert!(&format("%s", &[1.into(), 2.into()]).is_err());
    }
}
