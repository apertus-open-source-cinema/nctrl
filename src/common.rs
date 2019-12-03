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
