use fuseable_derive::Fuseable;
use serde_derive::*;

#[derive(Debug, Serialize, Deserialize, Fuseable, Clone)]
#[serde(untagged)]
pub enum Range {
    MinMax { min: i64, max: i64 },
}

#[derive(Debug, Serialize, Deserialize, Fuseable, Clone)]
#[serde(untagged)]
pub enum Description {
    Simple(String),
    LongAndShort { long: String, short: String },
}

pub fn to_hex(v: &Vec<u8>) -> String {
    if !v.is_empty() {
        "0x".to_string() + &v.iter().map(|v| format!("{:02X}", v).to_string()).collect::<String>()
    } else {
        "".to_string()
    }
}

pub trait ToStringOrVecU8 {
    fn bytes(self) -> Vec<u8>;
}

impl<T: ToString> ToStringOrVecU8 for T {
    fn bytes(self) -> Vec<u8> { self.to_string().as_bytes().to_vec() }
}

// shitty hack because specialization is not stable
// TODO(robin): revisit when (if) specialization ever lands
// (tracking issue: https://github.com/rust-lang/rust/issues/31844)
pub struct Bytes(Vec<u8>);

impl ToStringOrVecU8 for Bytes {
    fn bytes(self) -> Vec<u8> { self.0 }
}
