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
            return first_diff as i32;
        }
    }
    0
}

pub(crate) fn filevercmp(a: &[u8], b: &[u8]) -> i32 {
    if a.is_empty() && b.is_empty() {
        return 0;
    } else if a.is_empty() {
        return -1;
    } else if b.is_empty() {
        return 1;
    }

    if a[0] == b'.' {
        if b[0] != b'.' {
            return -1;
        }

        if a.len() == 1 {
            return -1;
        } else if b.len() == 1 {
            return 1;
        }

        if a[1] == b'.' && a.len() == 2 {
            return -1;
        }
        if b[1] == b'.' && b.len() == 2 {
            return 1;
        }
    } else if b[0] == b'.' {
        return 1;
    }

    let (a_prefix_len, a_len) = prefix_len(a);
    let (b_prefix_len, b_len) = prefix_len(b);

    let one_pass_only = a_prefix_len == a_len && b_prefix_len == b_len;

    let result = verrevcmp(&a[..a_prefix_len], &b[..b_prefix_len]);

    if result != 0 || one_pass_only {
        result
    } else {
        verrevcmp(&a[..a_len], &b[..b_len])
    }
}
