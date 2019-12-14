use num::{bigint::Sign, BigInt, Num};
use std::iter::FromIterator;

#[derive(Debug, PartialEq)]
pub struct ParseError(String);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { write!(f, "{}", self.0) }
}

impl std::error::Error for ParseError {}

impl<T: AsRef<str>> From<T> for ParseError {
    fn from(t: T) -> ParseError { ParseError(t.as_ref().to_string()) }
}

fn get_negative_radix_start(s: &[char]) -> Result<(bool, u32, usize), ParseError> {
    let mut s = s;
    let negative = match s.get(0) {
        Some('-') => {
            s = &s[1..];
            true
        }
        _ => false,
    };

    let offset = if negative { 1 } else { 0 };

    match (s.get(0), s.get(1)) {
        (Some('0'), Some('x')) => Ok((negative, 16, 2 + offset)),
        (Some('0'), Some('o')) => Ok((negative, 8, 2 + offset)),
        (Some('0'), Some('b')) => Ok((negative, 2, 2 + offset)),
        (Some('0'..='9'), _) => Ok((negative, 10, 0 + offset)),
        _ => Err("could not determine radix".into()),
    }
}

pub fn parse_num_mask<T: ToString>(s: T) -> Result<(Option<Vec<u8>>, Vec<u8>), ParseError> {
    let string: Vec<_> = s.to_string().chars().collect();
    let string = &string[..];

    if !string.contains(&'z') {
        let num = parse_num(s)?;

        return Ok((None, num))
    }

    let (negative, radix, start) = get_negative_radix_start(string)?;
    if radix.count_ones() != 1 {
        return Err("invald radix, expected radix of from 2^n for masked numbers".into())
    }

    let string = &string[start..];

    // TODO(robin): not really that nice at the moment, because it doesn't work for
    // radix > 256 (maybe that is ok?)
    let mask_element =
        match BigInt::new(Sign::Plus, vec![radix - 1]).to_str_radix(radix).chars().next() {
            Some(elem) => elem,
            None => return Err(format!("internal mask element error for radix {}", radix).into()),
        };

    let mask_string: Vec<_> =
        string.iter().cloned().map(|c| if c == 'z' { '0' } else { mask_element }).collect();
    let string: Vec<_> = string.iter().cloned().map(|c| if c == 'z' { '0' } else { c }).collect();

    for c in &string[..] {
        if !c.is_digit(radix) {
            return Err(format!("invalid digit {} for radix {}", c, radix).into())
        }
    }

    let mask = str_to_vec_radix_negative(&String::from_iter(mask_string), radix, negative)?;
    let value = str_to_vec_radix_negative(&String::from_iter(string), radix, negative)?;

    Ok((Some(mask), value))
}

pub fn parse_num_mask_padded<T: ToString + Clone>(
    s: T,
) -> Result<(Option<Vec<u8>>, Vec<u8>), ParseError> {
    let string: Vec<_> = s.to_string().chars().collect();
    let string = &string[..];

    if !string.contains(&'z') {
        let num = parse_num_padded(s)?;

        return Ok((None, num))
    }

    let (negative, radix, start) = get_negative_radix_start(string)?;
    if radix.count_ones() != 1 {
        return Err("invald radix, expected radix of from 2^n for masked numbers".into())
    }

    let string = &string[start..];

    // TODO(robin): not really that nice at the moment, because it doesn't work for
    // radix > 256 (maybe that is ok?)
    let mask_element =
        match BigInt::new(Sign::Plus, vec![radix - 1]).to_str_radix(radix).chars().next() {
            Some(elem) => elem,
            None => return Err(format!("internal mask element error for radix {}", radix).into()),
        };

    let mask_string: Vec<_> =
        string.iter().cloned().map(|c| if c == 'z' { '0' } else { mask_element }).collect();
    let string: Vec<_> = string.iter().cloned().map(|c| if c == 'z' { '0' } else { c }).collect();

    for c in &string[..] {
        if !c.is_digit(radix) {
            return Err(format!("invalid digit {} for radix {}", c, radix).into())
        }
    }

    let leading_zeros = string.iter().take_while(|c| **c == '0').count() as u32;
    let bits_per_digit = 0u32.leading_zeros() - (radix - 1).leading_zeros();
    let total_len = string.len() as u32;
    let total_bytes = if total_len > 0 { 1 + (total_len * bits_per_digit - 1) / 8 } else { 0 };

    let value_bytes = if total_len - leading_zeros > 0 {
        1 + ((total_len - leading_zeros) * bits_per_digit - 1) / 8
    } else {
        0
    };

    let padding_bytes = total_bytes - value_bytes;

    let mask = str_to_vec_radix_negative(&String::from_iter(mask_string), radix, negative).map(
        |mut v| {
            let mut padding = vec![0; padding_bytes as usize];
            padding.append(&mut v);
            padding
        },
    )?;

    let value =
        str_to_vec_radix_negative(&String::from_iter(string), radix, negative).map(|mut v| {
            let mut padding = vec![0; padding_bytes as usize];
            padding.append(&mut v);
            padding
        })?;

    Ok((Some(mask), value))
}

fn str_to_vec_radix_negative(s: &str, radix: u32, negative: bool) -> Result<Vec<u8>, ParseError> {
    BigInt::from_str_radix(s, radix)
        .map(|v| if negative { -v } else { v })
        .map(|v| {
            let bytes = v.to_signed_bytes_be();
            let len = bytes.len();
            if len > 1 {
                bytes.into_iter().skip_while(|v| *v == 0).collect()
            } else {
                bytes
            }
        })
        .map_err(|e| e.to_string().into())
}

pub fn parse_num<T: ToString>(string: T) -> Result<Vec<u8>, ParseError> {
    let string: Vec<_> = string.to_string().chars().collect();
    let string = &string[..];

    let (negative, radix, start) = get_negative_radix_start(string)?;
    let string = &string[start..];

    for c in string {
        if !c.is_digit(radix) {
            return Err(format!("invalid digit {} for radix {}", c, radix).into())
        }
    }

    str_to_vec_radix_negative(&String::from_iter(string), radix, negative)
}

// TODO(robin): this is not really well defined for non radix 2^n ∀ n ∈ ℕ
// numbers
pub fn parse_num_padded<T: ToString + Clone>(string: T) -> Result<Vec<u8>, ParseError> {
    let orig_string = string.clone();
    let string: Vec<_> = string.to_string().chars().collect();
    let string = &string[..];

    let (negative, radix, start) = get_negative_radix_start(string)?;
    let string = &string[start..];

    if radix.count_ones() != 1 {
        return parse_num(orig_string)
    }

    for c in string {
        if !c.is_digit(radix) {
            return Err(format!("invalid digit {} for radix {}", c, radix).into())
        }
    }

    let leading_zeros = string.iter().take_while(|c| **c == '0').count() as u32;
    let bits_per_digit = 0u32.leading_zeros() - (radix - 1).leading_zeros();
    let total_len = string.len() as u32;
    let total_bytes = if total_len > 0 { 1 + (total_len * bits_per_digit - 1) / 8 } else { 0 };
    let value_bytes = if total_len - leading_zeros > 0 {
        1 + ((total_len - leading_zeros) * bits_per_digit - 1) / 8
    } else {
        0
    };
    let mut padding_bytes = total_bytes - value_bytes;

    if value_bytes == 0 && padding_bytes > 0 {
        padding_bytes -= 1;
    }

    str_to_vec_radix_negative(&String::from_iter(string), radix, negative).map(|mut v| {
        let mut padding = vec![0; padding_bytes as usize];
        padding.append(&mut v);
        padding
    })
}

#[cfg(test)]
mod tests {
    use crate::{parse_num, parse_num_mask, parse_num_mask_padded, parse_num_padded};

    #[test]
    fn padding_test() {
        assert_eq!(parse_num_padded("0b0"), Ok(vec![0b0]));
        assert_eq!(parse_num_padded("0b1"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b01"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b001"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b0001"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b00001"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b000001"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b0000001"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b00000001"), Ok(vec![0b1]));
        assert_eq!(parse_num_padded("0b000000001"), Ok(vec![0, 0b1]));
        assert_eq!(parse_num_padded("0b10"), Ok(vec![0b10]));
        assert_eq!(parse_num_padded("0b010"), Ok(vec![0b10]));
        assert_eq!(parse_num_padded("0b0010"), Ok(vec![0b10]));
        assert_eq!(parse_num_padded("0b00010"), Ok(vec![0b10]));
        assert_eq!(parse_num_padded("0b000010"), Ok(vec![0b10]));
        assert_eq!(parse_num_padded("0b0000010"), Ok(vec![0b10]));
        assert_eq!(parse_num_padded("0b00000010"), Ok(vec![0b10]));
        assert_eq!(parse_num_padded("0b000000010"), Ok(vec![0, 0b10]));
        assert_eq!(parse_num_padded("0b0000000010"), Ok(vec![0, 0b10]));
        assert_eq!(parse_num_padded("0b100"), Ok(vec![0b100]));
        assert_eq!(parse_num_padded("0b0100"), Ok(vec![0b100]));
        assert_eq!(parse_num_padded("0b00100"), Ok(vec![0b100]));
        assert_eq!(parse_num_padded("0b000100"), Ok(vec![0b100]));
        assert_eq!(parse_num_padded("0b0000100"), Ok(vec![0b100]));
        assert_eq!(parse_num_padded("0b00000100"), Ok(vec![0b100]));
        assert_eq!(parse_num_padded("0b000000100"), Ok(vec![0, 0b100]));
        assert_eq!(parse_num_padded("0b0000000100"), Ok(vec![0, 0b100]));
        assert_eq!(parse_num_padded("0b00000000100"), Ok(vec![0, 0b100]));
        assert_eq!(parse_num_padded("0b1000"), Ok(vec![0b1000]));
        assert_eq!(parse_num_padded("0b01000"), Ok(vec![0b1000]));
        assert_eq!(parse_num_padded("0b001000"), Ok(vec![0b1000]));
        assert_eq!(parse_num_padded("0b0001000"), Ok(vec![0b1000]));
        assert_eq!(parse_num_padded("0b00001000"), Ok(vec![0b1000]));
        assert_eq!(parse_num_padded("0b000001000"), Ok(vec![0, 0b1000]));
        assert_eq!(parse_num_padded("0b0000001000"), Ok(vec![0, 0b1000]));
        assert_eq!(parse_num_padded("0b00000001000"), Ok(vec![0, 0b1000]));
        assert_eq!(parse_num_padded("0b000000001000"), Ok(vec![0, 0b1000]));
        assert_eq!(parse_num_padded("0b10000"), Ok(vec![0b10000]));
        assert_eq!(parse_num_padded("0b010000"), Ok(vec![0b10000]));
        assert_eq!(parse_num_padded("0b0010000"), Ok(vec![0b10000]));
        assert_eq!(parse_num_padded("0b00010000"), Ok(vec![0b10000]));
        assert_eq!(parse_num_padded("0b000010000"), Ok(vec![0, 0b10000]));
        assert_eq!(parse_num_padded("0b0000010000"), Ok(vec![0, 0b10000]));
        assert_eq!(parse_num_padded("0b00000010000"), Ok(vec![0, 0b10000]));
        assert_eq!(parse_num_padded("0b000000010000"), Ok(vec![0, 0b10000]));
        assert_eq!(parse_num_padded("0b0000000010000"), Ok(vec![0, 0b10000]));
        assert_eq!(parse_num_padded("0b100000"), Ok(vec![0b100000]));
        assert_eq!(parse_num_padded("0b0100000"), Ok(vec![0b100000]));
        assert_eq!(parse_num_padded("0b00100000"), Ok(vec![0b100000]));
        assert_eq!(parse_num_padded("0b000100000"), Ok(vec![0, 0b100000]));
        assert_eq!(parse_num_padded("0b0000100000"), Ok(vec![0, 0b100000]));
        assert_eq!(parse_num_padded("0b00000100000"), Ok(vec![0, 0b100000]));
        assert_eq!(parse_num_padded("0b000000100000"), Ok(vec![0, 0b100000]));
        assert_eq!(parse_num_padded("0b0000000100000"), Ok(vec![0, 0b100000]));
        assert_eq!(parse_num_padded("0b00000000100000"), Ok(vec![0, 0b100000]));
        assert_eq!(parse_num_padded("0b1000000"), Ok(vec![0b1000000]));
        assert_eq!(parse_num_padded("0b01000000"), Ok(vec![0b1000000]));
        assert_eq!(parse_num_padded("0b001000000"), Ok(vec![0, 0b1000000]));
        assert_eq!(parse_num_padded("0b0001000000"), Ok(vec![0, 0b1000000]));
        assert_eq!(parse_num_padded("0b00001000000"), Ok(vec![0, 0b1000000]));
        assert_eq!(parse_num_padded("0b000001000000"), Ok(vec![0, 0b1000000]));
        assert_eq!(parse_num_padded("0b0000001000000"), Ok(vec![0, 0b1000000]));
        assert_eq!(parse_num_padded("0b00000001000000"), Ok(vec![0, 0b1000000]));
        assert_eq!(parse_num_padded("0b000000001000000"), Ok(vec![0, 0b1000000]));
        assert_eq!(parse_num_padded("0b10000000"), Ok(vec![0b10000000]));
        assert_eq!(parse_num_padded("0b010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b0010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b00010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b0000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b00000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b000000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b0000000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b0100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b00100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b000100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b0000100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b00000100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b000000100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b0000000100000000"), Ok(vec![0b1, 0]));
        assert_eq!(parse_num_padded("0b00000000100000000"), Ok(vec![0, 0b1, 0]));
        assert_eq!(parse_num_padded("0b010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b0010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b00010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b0000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b00000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b000000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b0000000010000000"), Ok(vec![0, 0b10000000]));
        assert_eq!(parse_num_padded("0b00000000010000000"), Ok(vec![0, 0, 0b10000000]));
    }

    #[test]
    fn padding_mask_test() {
        assert_eq!(parse_num_mask_padded("0b1"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b01"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b0001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b00001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b000001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b0000001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b00000001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b000000001"), Ok((None, vec![0, 0b1])));
        assert_eq!(parse_num_mask_padded("0b10"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b010"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b0010"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b00010"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b000010"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b0000010"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b00000010"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b000000010"), Ok((None, vec![0, 0b10])));
        assert_eq!(parse_num_mask_padded("0b0000000010"), Ok((None, vec![0, 0b10])));
        assert_eq!(parse_num_mask_padded("0b100"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b0100"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b00100"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b000100"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b0000100"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b00000100"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b000000100"), Ok((None, vec![0, 0b100])));
        assert_eq!(parse_num_mask_padded("0b0000000100"), Ok((None, vec![0, 0b100])));
        assert_eq!(parse_num_mask_padded("0b00000000100"), Ok((None, vec![0, 0b100])));
        assert_eq!(parse_num_mask_padded("0b1000"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b01000"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b001000"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b0001000"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b00001000"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b000001000"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b0000001000"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b00000001000"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b000000001000"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b10000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b010000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b0010000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b00010000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b000010000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b0000010000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b00000010000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b000000010000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b0000000010000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b100000"), Ok((None, vec![0b100000])));
        assert_eq!(parse_num_mask_padded("0b0100000"), Ok((None, vec![0b100000])));
        assert_eq!(parse_num_mask_padded("0b00100000"), Ok((None, vec![0b100000])));
        assert_eq!(parse_num_mask_padded("0b000100000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b0000100000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b00000100000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b000000100000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b0000000100000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b00000000100000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b1000000"), Ok((None, vec![0b1000000])));
        assert_eq!(parse_num_mask_padded("0b01000000"), Ok((None, vec![0b1000000])));
        assert_eq!(parse_num_mask_padded("0b001000000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b0001000000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b00001000000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b000001000000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b0000001000000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b00000001000000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b000000001000000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b10000000"), Ok((None, vec![0b10000000])));
        assert_eq!(parse_num_mask_padded("0b010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0000000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b0100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b00100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b000100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b0000100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b00000100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b000000100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b0000000100000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b00000000100000000"), Ok((None, vec![0, 0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0000000010000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(
            parse_num_mask_padded("0b00000000010000000"),
            Ok((None, vec![0, 0, 0b10000000]))
        );
    }

    /*
    #[test]
    fn padding_mask_test_with_mask() {
        assert_eq!(parse_num_mask_padded("0b1"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b01"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b0001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b00001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b000001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b0000001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b00000001"), Ok((None, vec![0b1])));
        assert_eq!(parse_num_mask_padded("0b000000001"), Ok((None, vec![0, 0b1])));
        assert_eq!(parse_num_mask_padded("0b1z"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b01z"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b001z"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b0001z"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b00001z"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b000001z"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b0000001z"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask_padded("0b00000001z"), Ok((None, vec![0, 0b10])));
        assert_eq!(parse_num_mask_padded("0b000000001z"), Ok((None, vec![0, 0b10])));
        assert_eq!(parse_num_mask_padded("0b1z0"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b01z0"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b001z0"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b0001z0"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b00001z0"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b000001z0"), Ok((None, vec![0b100])));
        assert_eq!(parse_num_mask_padded("0b0000001z0"), Ok((None, vec![0, 0b100])));
        assert_eq!(parse_num_mask_padded("0b00000001z0"), Ok((None, vec![0, 0b100])));
        assert_eq!(parse_num_mask_padded("0b000000001z0"), Ok((None, vec![0, 0b100])));
        assert_eq!(parse_num_mask_padded("0b1z00"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b01z00"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b001z00"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b0001z00"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b00001z00"), Ok((None, vec![0b1000])));
        assert_eq!(parse_num_mask_padded("0b000001z00"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b0000001z00"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b00000001z00"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b000000001z00"), Ok((None, vec![0, 0b1000])));
        assert_eq!(parse_num_mask_padded("0b1z000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b01z000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b001z000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b0001z000"), Ok((None, vec![0b10000])));
        assert_eq!(parse_num_mask_padded("0b00001z000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b000001z000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b0000001z000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b00000001z000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b000000001z000"), Ok((None, vec![0, 0b10000])));
        assert_eq!(parse_num_mask_padded("0b1z0000"), Ok((None, vec![0b100000])));
        assert_eq!(parse_num_mask_padded("0b01z0000"), Ok((None, vec![0b100000])));
        assert_eq!(parse_num_mask_padded("0b001z0000"), Ok((None, vec![0b100000])));
        assert_eq!(parse_num_mask_padded("0b0001z0000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b00001z0000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b000001z0000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b0000001z0000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b00000001z0000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b000000001z0000"), Ok((None, vec![0, 0b100000])));
        assert_eq!(parse_num_mask_padded("0b1z00000"), Ok((None, vec![0b1000000])));
        assert_eq!(parse_num_mask_padded("0b01z00000"), Ok((None, vec![0b1000000])));
        assert_eq!(parse_num_mask_padded("0b001z00000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b0001z00000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b00001z00000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b000001z00000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b0000001z00000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b00000001z00000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b000000001z00000"), Ok((None, vec![0, 0b1000000])));
        assert_eq!(parse_num_mask_padded("0b1z000000"), Ok((None, vec![0b10000000])));
        assert_eq!(parse_num_mask_padded("0b01z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b1z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b01z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b001z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b0001z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b00001z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b000001z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b0000001z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b00000001z0000000"), Ok((None, vec![0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b000000001z0000000"), Ok((None, vec![0, 0b1, 0])));
        assert_eq!(parse_num_mask_padded("0b01z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b00000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b000000001z000000"), Ok((None, vec![0, 0b10000000])));
        assert_eq!(parse_num_mask_padded("0b0000000001z000000"), Ok((None, vec![0, 0, 0b10000000])));
    }
    */

    /*
    #[test]
    fn padding_test() {
        assert_eq!(parse_num("0x002"), Ok(vec![0, 0x2]));
        assert_eq!(parse_num("0b0000010"), Ok(vec![0b10]));
        assert_eq!(parse_num("0o002"), Ok(vec![0, 0o2]));
    }
    */

    #[test]
    fn basic_number_parsing() {
        assert_eq!(parse_num("2"), Ok(vec![2]));
        assert_eq!(parse_num("0x2"), Ok(vec![0x2]));
        assert_eq!(parse_num("0xBEEF"), Ok(vec![0xbe, 0xef]));
        assert_eq!(parse_num("0xBEEF00"), Ok(vec![0xbe, 0xef, 0x0]));
        assert_eq!(parse_num("0b10"), Ok(vec![0b10]));
        assert_eq!(parse_num("0o2"), Ok(vec![0o2]));
    }

    #[test]
    fn masked_number_parsing() {
        assert_eq!(
            parse_num_mask("2z"),
            Err("invald radix, expected radix of from 2^n for masked numbers".into())
        );
        assert_eq!(parse_num_mask("0x2"), Ok((None, vec![0x2])));
        assert_eq!(parse_num_mask("0b10"), Ok((None, vec![0b10])));
        assert_eq!(parse_num_mask("0o2"), Ok((None, vec![0o2])));
    }

    #[test]
    fn masked_number_parsing_with_masks() {
        assert_eq!(parse_num_mask("0xz2"), Ok((Some(vec![0b1111]), vec![0x2])));
        assert_eq!(parse_num_mask("0b1z0"), Ok((Some(vec![0b101]), vec![0b100])));
        assert_eq!(parse_num_mask("0o2z"), Ok((Some(vec![0b111000]), vec![0o20])));
    }
}
