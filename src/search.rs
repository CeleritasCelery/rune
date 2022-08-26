use crate::core::{
    env::Env,
    gc::{Context, Root},
    object::{nil, GcObj},
};
use anyhow::{ensure, Result};
use fancy_regex::Regex;
use fn_macros::defun;

#[defun]
fn string_match<'ob>(
    regexp: &str,
    string: &str,
    start: Option<i64>,
    env: &mut Root<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    let start = start.unwrap_or(0) as usize;
    let re = Regex::new(regexp)?;

    if let Some(matches) = re.captures_iter(&string[start..]).next() {
        let mut all: Vec<GcObj> = Vec::new();
        let all_matches = matches?;
        let mut groups = all_matches.iter();
        while let Some(Some(group)) = groups.next() {
            all.push(group.start().into());
            all.push(group.end().into());
        }
        let match_data = crate::fns::slice_into_list(&all, None, cx);
        env.as_mut(cx).match_data.set(match_data);
        Ok(match_data.as_cons().car())
    } else {
        Ok(nil())
    }
}

#[defun]
fn match_data<'ob>(
    integer: Option<()>,
    reuse: Option<()>,
    reseat: Option<()>,
    env: &Root<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    ensure!(
        integer.is_none(),
        "match-data integer field is not implemented"
    );
    ensure!(reuse.is_none(), "match-data reuse field is not implemented");
    ensure!(
        reseat.is_none(),
        "match-data reseat field is not implemented"
    );
    Ok(env.match_data.bind(cx))
}

#[defun]
fn set_match_data<'ob>(
    list: GcObj,
    _reseat: Option<()>,
    env: &mut Root<Env>,
    cx: &'ob Context,
) -> GcObj<'ob> {
    // TODO: add reseat when markers implemented
    env.as_mut(cx).match_data.set(list);
    nil()
}

#[defun]
fn string_equal(s1: &str, s2: &str) -> bool {
    s1 == s2
}

define_symbols!(FUNCS => {string_match, match_data, set_match_data, string_equal});
