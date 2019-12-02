use crate::{
    address::Address,
    common::Description,
    communication_channel::CommunicationChannel,
    registers::{ComputedRegister, CookedRegister, RawRegister},
    serde_util::{bool_true, by_path},
    valuemap::*,
};
use rlua::Lua;

use fuseable::{type_name, Either, Fuseable, FuseableError};

use itertools::Itertools;

use serde::{de::Error, Deserialize, Deserializer};
use serde_derive::*;
use std::{collections::HashMap, fmt::Debug};

use derivative::Derivative;
use failure::format_err;

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Device {
    pub channel: CommunicationChannel,
    raw: HashMap<String, RawRegister>,
    cooked: HashMap<String, CookedRegister>,
    computed: HashMap<String, ComputedRegister>,
    #[derivative(Debug = "ignore")]
    pub lua_vm: Lua,
}

// one lua vm for each device, preloaded with the right functions and
// maybe we even store the precompiled function for each computed
// register?

// this is kinda sad, because we also need a lua vm for the whole
// camera (for lua scripts), but it is hard to use that one for
// computed registers aswell, because computed registers already have
// the device locked for them and thus we can preload the lua vm with
// the functions needed to access the device, this is not the case for
// lua scripts, these need to first lock the devices and then can
// access them

// as we want computed registers to be fast, i see no way currently to
// just use one global lua vm


impl Device {
    pub fn read_raw(&self, name: &str) -> fuseable::Result<String> {
        self.raw
            .get(name)
            .ok_or_else(|| format_err!("tried to read non existant raw register {}", name))?
            .read_value(&mut std::iter::empty(), &self.channel)
            .map(|v| match v {
                Either::Right(s) => s,
                _ => panic!("got directory entries from a register"),
            })
    }

    pub fn write_raw<T: ToString>(&self, name: &str, value: T) -> fuseable::Result<()> {
        self.raw
            .get(name)
            .ok_or_else(|| format_err!("tried to write to non existant raw register {}", name))?
            .write_value(
                &mut std::iter::empty(),
                value.to_string().as_bytes().to_vec(),
                &self.channel,
            )
    }

    pub fn read_cooked(&self, name: &str) -> fuseable::Result<String> {
        self.cooked
            .get(name)
            .ok_or_else(|| format_err!("tried to read non existant cooked register {}", name))?
            .read_value(&mut std::iter::empty(), &self.channel)
            .map(|v| match v {
                Either::Right(s) => s,
                _ => panic!("got directory entries from a register"),
            })
    }

    pub fn write_cooked<T: ToString>(&self, name: &str, value: T) -> fuseable::Result<()> {
        self.cooked
            .get(name)
            .ok_or_else(|| format_err!("tried to write to non existant cooked register {}", name))?
            .write_value(
                &mut std::iter::empty(),
                value.to_string().as_bytes().to_vec(),
                &self.channel,
            )
    }

    pub fn read_computed(&self, name: &str) -> fuseable::Result<String> {
        self.computed
            .get(name)
            .ok_or_else(|| format_err!("tried to read non existant computed register {}", name))?
            .read_value(&mut std::iter::empty(), &self)
            .map(|v| match v {
                Either::Right(s) => s,
                _ => panic!("got directory entries from a register"),
            })
    }

    pub fn write_computed<T: ToString>(&self, name: &str, value: T) -> fuseable::Result<()> {
        self.computed
            .get(name)
            .ok_or_else(|| {
                format_err!("tried to write to non existant computed register {}", name)
            })?
            .write_value(&mut std::iter::empty(), value.to_string().as_bytes().to_vec(), &self)
    }
}

impl Fuseable for Device {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        match path.next() {
            Some("channel") => self.channel.is_dir(path),
            Some("raw") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.raw.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => self.raw.is_dir(&mut path),
                }
            }
            Some("cooked") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.cooked.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => self.cooked.is_dir(&mut path),
                }
            }
            Some("computed") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.computed.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => self.computed.is_dir(&mut path),
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(true),
        }
    }

    fn read(
        &self,
        path: &mut dyn Iterator<Item = &str>,
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some("channel") => self.channel.read(path),
            Some("raw") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(_), None) => self.raw.read(&mut path).map(|value| match value {
                        Either::Left(mut dir_entries) => {
                            dir_entries.push("value".to_owned());
                            Either::Left(dir_entries)
                        }
                        Either::Right(_) => {
                            panic!("tought I would get directory entires, but got file content")
                        }
                    }),
                    (Some(name), Some("value")) => self
                        .raw
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .read_value(&mut std::iter::empty(), &self.channel),
                    _ => self.raw.read(&mut path),
                }
            }
            Some("cooked") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(_), None) => self.cooked.read(&mut path).map(|value| match value {
                        Either::Left(mut dir_entries) => {
                            dir_entries.push("value".to_owned());
                            Either::Left(dir_entries)
                        }
                        Either::Right(_) => {
                            panic!("tought I would get directory entires, but got file content")
                        }
                    }),
                    (Some(name), Some("value")) => self
                        .cooked
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .read_value(&mut std::iter::empty(), &self.channel),
                    _ => self.cooked.read(&mut path),
                }
            }
            Some("computed") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(_), None) => self.computed.read(&mut path).map(|value| match value {
                        Either::Left(mut dir_entries) => {
                            dir_entries.push("value".to_owned());
                            Either::Left(dir_entries)
                        }
                        Either::Right(_) => {
                            panic!("tought I would get directory entires, but got file content")
                        }
                    }),
                    (Some(name), Some("value")) => self
                        .computed
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .read_value(&mut std::iter::empty(), &self),
                    _ => self.computed.read(&mut path),
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(Either::Left(vec![
                "channel".to_owned(),
                "raw".to_owned(),
                "cooked".to_owned(),
                "computed".to_owned(),
            ])),
        }
    }

    fn write(
        &mut self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
    ) -> fuseable::Result<()> {
        match path.next() {
            Some("channel") => Err(FuseableError::unsupported("write", type_name(&self.channel))),
            Some("raw") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => self
                        .raw
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .write_value(&mut std::iter::empty(), value, &self.channel),
                    _ => self.raw.write(&mut path, value),
                }
            }
            Some("cooked") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => self
                        .cooked
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .write_value(&mut std::iter::empty(), value, &self.channel),
                    _ => self.cooked.write(&mut path, value),
                }
            }
            Some("computed") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => self
                        .computed
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .write_value(&mut std::iter::empty(), value, &self),
                    _ => self.computed.write(&mut path, value),
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

impl<'de> Deserialize<'de> for Device {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        struct CookedRegisterStringAddr {
            address: String,
            description: Option<Description>,
            #[serde(default, deserialize_with = "deser_valuemap")]
            map: Option<ValueMap>,
            #[serde(default = "bool_true")]
            writable: bool,
            default: Option<u64>,
        }

        #[derive(Debug, Deserialize)]
        struct DeviceConfig {
            channel: CommunicationChannel,
            #[serde(deserialize_with = "by_path", default)]
            raw: HashMap<String, RawRegister>,
            #[serde(deserialize_with = "by_path", default)]
            cooked: HashMap<String, CookedRegisterStringAddr>,
            #[serde(deserialize_with = "by_path", default)]
            computed: HashMap<String, ComputedRegister>,
        }

        let settings = DeviceConfig::deserialize(deserializer)?;

        let DeviceConfig { channel, raw, cooked, computed } = settings;

        let cooked = cooked
            .into_iter()
            .map(|(name, cooked_reg)| {
                let address = Address::parse_named(&cooked_reg.address, &raw).map_err(|_| {
                    D::Error::custom(format!(
                        "could not parse the address of this cooked register ({})",
                        cooked_reg.address
                    ))
                })?;

                Ok((name.clone(), CookedRegister {
                    address,
                    description: cooked_reg.description,
                    map: cooked_reg.map,
                    default: cooked_reg.default,
                    writable: cooked_reg.writable,
                }))
            })
            .collect::<Result<HashMap<String, CookedRegister>, _>>()?;

        Ok(Device { channel, raw, cooked, computed, lua_vm: Lua::new() })
    }
}
