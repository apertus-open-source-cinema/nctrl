use crate::common::{Description, Range};

use crate::{
    address::Address,
    communication_channel::{CommChannel, CommunicationChannel},
    value::Value,
};


use fuseable_derive::Fuseable;

use failure::format_err;

use serde::{de::Error, Deserialize, Deserializer};
use serde_derive::*;
use std::fmt::Debug;


#[derive(Debug, Serialize, Fuseable, Clone, PartialEq)]
pub struct RawRegister {
    #[fuseable(ro)]
    pub address: Address,
    #[fuseable(ro)]
    pub width: Option<u64>,
    #[fuseable(ro)]
    mask: Option<String>,
    #[fuseable(ro)]
    #[serde(flatten)]
    range: Option<Range>,
    #[fuseable(ro)]
    #[serde(default, deserialize_with = "by_string_option_num")]
    pub default: Option<Vec<u8>>,
    #[fuseable(ro)]
    description: Option<Description>,
}

impl<'de> Deserialize<'de> for RawRegister {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        pub struct RegisterStringAddr {
            pub address: String,
            pub width: Option<u64>,
            mask: Option<String>,
            #[serde(flatten)]
            range: Option<Range>,
            default: Option<String>,
            description: Option<Description>,
        }

        let reg = RegisterStringAddr::deserialize(deserializer)?;

        let address = Address::parse(&reg.address, reg.width)
            .map_err(|_| D::Error::custom("error parsing address"))?;

        // TODO(robin): error handling here!
        let width = reg.width; // rust closure capture is shit and cannot handle partial captures
        let default = reg.default.map(|x| {
            match width {
                Some(width) => parse_num::parse_num_padded_width(x, width as u64),
                None => parse_num::parse_num(x),
            }
            .unwrap()
            .1
        });

        Ok(RawRegister {
            address,
            width: reg.width,
            mask: reg.mask,
            range: reg.range,
            default,
            description: reg.description,
        })
    }
}

impl RawRegister {
    pub fn width(&self) -> fuseable::Result<u64> {
        self.width.map(|v| v as u64).or_else(|| self.address.bytes().ok()).ok_or_else(|| {
            format_err!(
                "tried to get width of {:?} but neither was width directly specified nor part of the address",
                self
            )
        })
    }

    pub fn read_value(&self, comm_channel: &CommunicationChannel) -> fuseable::Result<Value> {
        comm_channel.read_value(&self.address)
    }

    pub fn write_value(
        &self,
        value: Value,
        comm_channel: &CommunicationChannel,
    ) -> fuseable::Result<()> {
        comm_channel.write_value(&self.address, value)
    }
}
