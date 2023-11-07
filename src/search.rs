use crate::core::{
    env::Env,
    gc::{Context, Rt},
    object::{nil, Gc, GcObj, List},
};
use anyhow::{ensure, Result};
use fallible_iterator::FallibleIterator;
use fancy_regex::Regex;
use fn_macros::defun;

#[defun]
fn string_match<'ob>(
    regexp: &str,
    string: &str,
    start: Option<i64>,
    _inhibit_modify: Option<()>,
    env: &mut Rt<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    // TODO: implement inhibit-modify
    let re = Regex::new(&lisp_regex_to_rust(regexp))?;

    let start = start.unwrap_or(0) as usize;
    if let Some(matches) = re.captures_iter(&string[start..]).next() {
        let mut all: Vec<GcObj> = Vec::new();
        let all_matches = matches?;
        let mut groups = all_matches.iter();
        while let Some(Some(group)) = groups.next() {
            all.push(group.start().into());
            all.push(group.end().into());
        }
        let match_data = crate::fns::slice_into_list(&all, None, cx);
        env.match_data.set(match_data);
        Ok(match_data.as_cons().car())
    } else {
        Ok(nil())
    }
}

fn lisp_regex_to_rust(regexp: &str) -> String {
    let mut norm_regex = String::new();
    let mut chars = regexp.chars();
    while let Some(ch) = chars.next() {
        match ch {
            // Invert the escaping of parens. i.e. \( => ( and ( => \(
            c @ '('..=')' => {
                norm_regex.push('\\');
                norm_regex.push(c);
            }
            '\\' => match chars.next() {
                Some(c @ '('..=')') => norm_regex.push(c),
                Some('`') => norm_regex += "\\A",
                Some('\'') => norm_regex += "\\z",
                Some(c) => {
                    norm_regex.push('\\');
                    norm_regex.push(c);
                }
                None => norm_regex.push('\\'),
            },
            c => norm_regex.push(c),
        }
    }
    norm_regex
}
#[defun]
fn match_data<'ob>(
    integer: Option<()>,
    reuse: Option<()>,
    reseat: Option<()>,
    env: &Rt<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    ensure!(integer.is_none(), "match-data integer field is not implemented");
    ensure!(reuse.is_none(), "match-data reuse field is not implemented");
    ensure!(reseat.is_none(), "match-data reseat field is not implemented");
    Ok(env.match_data.bind(cx))
}

#[defun]
fn set_match_data<'ob>(list: Gc<List>, _reseat: Option<()>, env: &mut Rt<Env>) -> GcObj<'ob> {
    // TODO: add reseat when markers implemented
    let obj: GcObj = list.into();
    env.match_data.set(obj);
    nil()
}

#[defun]
fn match_beginning<'ob>(subexp: usize, env: &Rt<Env>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    let list = env.match_data.bind(cx).as_list()?;
    Ok(list.fallible().nth(subexp)?.unwrap_or_default())
}

#[defun]
fn match_end<'ob>(subexp: usize, env: &Rt<Env>, cx: &'ob Context) -> Result<GcObj<'ob>> {
    let list = env.match_data.bind(cx).as_list()?;
    Ok(list.fallible().nth(subexp + 1)?.unwrap_or_default())
}

#[defun]
fn string_equal(s1: &str, s2: &str) -> bool {
    s1 == s2
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lisp_regex() {
        assert_eq!(lisp_regex_to_rust("foo"), "foo");
        assert_eq!(lisp_regex_to_rust("\\foo"), "\\foo");
        assert_eq!(lisp_regex_to_rust("\\(foo\\)"), "(foo)");
        assert_eq!(lisp_regex_to_rust("(foo)"), "\\(foo\\)");
    }
}
