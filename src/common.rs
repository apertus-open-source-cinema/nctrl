use fuseable_derive::Fuseable;
use serde_derive::*;

use crate::bytes::{FromBytes, ToBytes};

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

pub fn to_hex_string(v: &Vec<u8>) -> fuseable::Result<Vec<u8>> {
    if !v.is_empty() {
        ("0x".to_string()
            + &v.iter().map(|v| format!("{:02X}", v).to_string()).collect::<String>()
            + "\n")
            .to_bytes()
    } else {
        "\n".to_bytes()
    }
}

pub fn string(value: Vec<u8>) -> fuseable::Result<String> { FromBytes::from_bytes(value) }

pub fn float(value: Vec<u8>) -> fuseable::Result<f64> { FromBytes::from_bytes(value) }
