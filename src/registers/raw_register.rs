use crate::common::{to_hex, Description, Range};

use crate::{
    address::Address, communication_channel::CommunicationChannel,
};

use failure::format_err;
use fuseable::{type_name, Either, FuseableError};
use fuseable_derive::Fuseable;
use itertools::izip;
use parse_num::parse_num_mask;
use serde::{de::Error, Deserialize, Deserializer};
use serde_derive::*;
use std::fmt::Debug;


#[derive(Debug, Serialize, Fuseable, Clone)]
pub struct RawRegister {
    #[fuseable(ro)]
    pub address: Address,
    #[fuseable(ro)]
    pub width: Option<u8>,
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
            pub width: Option<u8>,
            mask: Option<String>,
            #[serde(flatten)]
            range: Option<Range>,
            default: Option<String>,
            description: Option<Description>,
        }

        let reg = RegisterStringAddr::deserialize(deserializer)?;

        let address = Address::parse(&reg.address, reg.width.map(|v| v as usize))
            .map_err(|_| D::Error::custom("error parsing address"))?;

        // TODO: error handling here!
        let default = reg.default.map(|x| parse_num::parse_num(x).unwrap());

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
    pub fn read_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        comm_channel: &CommunicationChannel,
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => comm_channel.read_value(&self.address).map(|v| Either::Right(to_hex(&v))),
        }
    }

    pub fn write_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
        comm_channel: &CommunicationChannel,
    ) -> fuseable::Result<()> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => {
                if let Some(width) = self.width {
                    let (mask, mut value) = parse_num_mask(String::from_utf8_lossy(&value))?;

                    if value.len() > width as usize {
                        return Err(format_err!("value {:?} to write was longer ({}) than register {:?} with width of {}", value, value.len(), self, width));
                    }

                    while value.len() < width as usize {
                        value.insert(0, 0);
                    }

                    let value = match mask {
                        Some(mut mask) => {
                            // TODO(robin): this currently interprets a too short value, as if the
                            // missing part should not be assigned and the old value (that is
                            // already in the register) be kept
                            // it is unclear if this is the wanted / intuitive behaviour, or if the
                            // opposite is the case (note this applies only if a mask is specified,
                            // maybe we only want to allow masks, when their width matches the
                            while mask.len() < width as usize {
                                mask.insert(0, 0);
                            }

                            let current_value = comm_channel.read_value(&self.address)?;

                            izip!(mask, value, current_value)
                                .map(|(m, val, cur)| (val & m) | (cur & !m))
                                .collect()
                        }
                        None => value,
                    };

                    comm_channel.write_value(&self.address, value)
                } else {
                    Err(format_err!("the register written to {:?} did not specify a width, don't know what to do", self))
                }
            }
        }
    }
}
