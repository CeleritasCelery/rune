///Return the length of a prefix of S that corresponds to the suffix
///defined by this extended regular expression in the C locale:
///  (\.[A-Za-z~][A-Za-z0-9~]*)*$
///Use the longest suffix matching this regular expression,
///except do not use all of S as a suffix if S is nonempty.
///If *LEN is -1, S is a string; set *LEN to S's length.
///Otherwise, *LEN should be nonnegative, S is a char array,
///and *LEN does not change.
fn prefix_len(s: &[u8]) -> (usize, usize) {
    let mut prefix_len = 0;

    let mut i = 0;
    loop {
        if i == s.len() {
            return (prefix_len, i);
        }

        i += 1;
        prefix_len = i;

        while i + 1 < s.len()
            && s[i] == b'.'
            && (s[i + 1].is_ascii_alphabetic() || s[i + 1] == b'~')
        {
            i += 2;
            while i < s.len() && (s[i].is_ascii_alphanumeric() || s[i] == b'~') {
                i += 1;
            }
        }
    }
}

///Return a version sort comparison value for S's byte at position POS.
///S has length LEN.  If POS == LEN, sort before all non-'~' bytes.
fn order(s: &[u8], pos: usize) -> i32 {
    if pos == s.len() {
        return -1;
    }

    let c = s[pos];

    if c.is_ascii_digit() {
        0
    } else if c.is_ascii_alphabetic() {
        c as i32
    } else if c == b'~' {
        -2
    } else {
        c as i32 + 256
    }
}

///slightly modified verrevcmp function from dpkg
///S1, S2 - compared char array
///S1_LEN, S2_LEN - length of arrays to be scanned
///
///This implements the algorithm for comparison of version strings
///specified by Debian and now widely adopted.  The detailed
///specification can be found in the Debian Policy Manual in the
///section on the 'Version' control field.  This version of the code
///implements that from s5.6.12 of Debian Policy v3.8.0.1
///https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version
fn verrevcmp(s1: &[u8], s2: &[u8]) -> i32 {
    let mut s1_pos = 0;
    let mut s2_pos = 0;

    while s1_pos < s1.len() && s2_pos < s2.len() {
        let mut first_diff: i32 = 0;

        while (s1_pos < s1.len() && !s1[s1_pos].is_ascii_digit())
            || (s2_pos < s2.len() && !s2[s2_pos].is_ascii_digit())
        {
            let s1_c = order(s1, s1_pos);
            let s2_c = order(s2, s2_pos);
            if s1_c != s2_c {
                return s1_c - s2_c;
            }
            s1_pos += 1;
            s2_pos += 1;
        }

        while s1_pos < s1.len() && s1[s1_pos] == b'0' {
            s1_pos += 1;
        }
        while s2_pos < s2.len() && s2[s2_pos] == b'0' {
            s2_pos += 1;
        }
        while s1_pos < s1.len()
            && s2_pos < s2.len()
            && s1[s1_pos].is_ascii_digit()
            && s2[s2_pos].is_ascii_digit()
        {
            if first_diff == 0 {
                first_diff = s1[s1_pos] as i32 - s2[s2_pos] as i32;
            }
            s1_pos += 1;
            s2_pos += 1;
        }
        if s1_pos < s1.len() && s1[s1_pos].is_ascii_digit() {
            return 1;
        }
        if s2_pos < s2.len() && s2[s2_pos].is_ascii_digit() {
            return -1;
        }
        if first_diff != 0 {
            return first_diff;
        }
    }
    0
}

///Compare strings A and B as file names containing version numbers,
///and return an integer that is negative, zero, or positive depending
///on whether A compares less than, equal to, or greater than B.
///
///Use the following version sort algorithm:
///
///  1. Compare the strings' maximal-length non-digit prefixes lexically.
///     If there is a difference return that difference.
///     Otherwise discard the prefixes and continue with the next step.
///
///  2. Compare the strings' maximal-length digit prefixes, using
///     numeric comparison of the numbers represented by each prefix.
///     (Treat an empty prefix as zero; this can happen only at string end.)
///     If there is a difference, return that difference.
///     Otherwise discard the prefixes and continue with the next step.
///
///  3. If both strings are empty, return 0.  Otherwise continue with step 1.
///
///In version sort, lexical comparison is left to right, byte by byte,
///using the byte's numeric value (0-255), except that:
///
///  1. ASCII letters sort before other bytes.
///  2. A tilde sorts before anything, even an empty string.
///
///In addition to the version sort rules, the following strings have
///special priority and sort before all other strings (listed in order):
///
///  1. The empty string.
///  2. ".".
///  3. "..".
///  4. Strings starting with "." sort before other strings.
///
///Before comparing two strings where both begin with non-".",
///or where both begin with "." but neither is "." or "..",
///suffixes matching the C-locale extended regular expression
///(\.[A-Za-z~][A-Za-z0-9~]*)*$ are removed and the strings compared
///without them, using version sort without special priority;
///if they do not compare equal, this comparison result is used and
///the suffixes are effectively ignored.  Otherwise, the entire
///strings are compared using version sort.  When removing a suffix
///from a nonempty string, remove the maximal-length suffix such that
///the remaining string is nonempty.
///
///This function is intended to be a replacement for strverscmp.
pub(crate) fn filevercmp(a: &[u8], b: &[u8]) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    if a.is_empty() && b.is_empty() {
        return Ordering::Equal;
    } else if a.is_empty() {
        return Ordering::Less;
    } else if b.is_empty() {
        return Ordering::Greater;
    }

    if a[0] == b'.' {
        if b[0] != b'.' {
            return Ordering::Less;
        }

        let a_dot = if a.len() == 1 {
            1
        } else if a[1] != 0 {
            0
        } else {
            1
        };
        let b_dot = if b.len() == 1 {
            1
        } else if b[1] != 0 {
            0
        } else {
            1
        };

        if a_dot != 0 {
            return if b_dot != 0 { Ordering::Equal } else { Ordering::Less };
        }
        if b_dot != 0 {
            return Ordering::Greater;
        }

        let a_dot_dot = if a[1] == b'.' {
            if a.len() == 2 {
                1
            } else {
                0
            }
        } else if a.len() > 2 {
            if a[2] != 0 {
                0
            } else {
                1
            }
        } else {
            0
        };

        let b_dot_dot = if b[1] == b'.' {
            if b.len() == 2 {
                1
            } else {
                0
            }
        } else if b.len() > 2 {
            if b[2] != 0 {
                0
            } else {
                1
            }
        } else {
            0
        };

        if a_dot_dot != 0 {
            return if b_dot_dot != 0 { Ordering::Equal } else { Ordering::Less };
        }
        if b_dot_dot != 0 {
            return Ordering::Greater;
        }
    } else if b[0] == b'.' {
        return Ordering::Greater;
    }

    let (a_prefix_len, a_len) = prefix_len(a);
    let (b_prefix_len, b_len) = prefix_len(b);

    let one_pass_only = a_prefix_len == a_len && b_prefix_len == b_len;

    let result = verrevcmp(&a[..a_prefix_len], &b[..b_prefix_len]);

    if result != 0 || one_pass_only {
        std::cmp::Ord::cmp(&result, &0)
    } else {
        let result = verrevcmp(&a[..a_len], &b[..b_len]);
        std::cmp::Ord::cmp(&result, &0)
    }
}
