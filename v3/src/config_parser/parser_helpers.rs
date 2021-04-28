use std::num::ParseIntError;

use super::{
    raw_ast::{ArgumentValue, Device},
    span::Spanned,
};

pub enum ArgOrDevice {
    Arg { name: Spanned<String>, arg: Spanned<ArgumentValue> },
    Device(Spanned<Device>),
}

pub fn parse_int_lit(s: &str) -> Result<i64, ParseIntError> {
    let (s, negative) = if &s[0..1] == "-" {
        (&s[1..], true)
    } else if &s[0..1] == "+" {
        (&s[1..], false)
    } else {
        (s, false)
    };

    let result = if s.len() <= 2 {
        i64::from_str_radix(s, 10)
    } else {
        match (&s[0..1], &s[1..2]) {
            ("0", "x") => i64::from_str_radix(&s[2..], 16),
            ("0", "b") => i64::from_str_radix(&s[2..], 2),
            ("0", "o") => i64::from_str_radix(&s[2..], 8),
            _ => i64::from_str_radix(s, 10),
        }
    };
    result.map(|v| if negative { -v } else { v })
}
