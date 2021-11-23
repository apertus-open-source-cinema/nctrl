// SPDX-FileCopyrightText: © 2019 Jaro Habiger <jarohabiger@googlemail.com>
// SPDX-FileCopyrightText: © 2019 Robin Ole Heinemann <robin.ole.heinemann@gmail.com>
// SPDX-License-Identifier: AGPL-3.0-only

use crate::{
    address::Address,
    common::Description,
    communication_channel::CommunicationChannel,
    registers::{ComputedRegister, CookedRegister, RawRegister, WidthOrRawRegister},
    serde_util::{bool_true, by_path},
    value::Value,
    valuemap::*,
};

use fuseable::{type_name, Either, Fuseable, FuseableError};

use itertools::Itertools;

use serde::{de::Error, Deserialize, Deserializer};
use serde_derive::*;
use std::{collections::HashMap, fmt::Debug, rc::Rc};

use derivative::Derivative;
use failure::{format_err, ResultExt};

pub trait DeviceLike {
    fn read_raw(&self, name: &str) -> fuseable::Result<Value>;
    fn write_raw(&self, name: &str, value: Value) -> fuseable::Result<()>;

    fn read_cooked(&self, name: &str) -> fuseable::Result<Value>;
    fn write_cooked(&self, name: &str, value: Value) -> fuseable::Result<()>;

    fn read_computed(&self, name: &str) -> fuseable::Result<Value>;
    fn write_computed(&self, name: &str, value: Value) -> fuseable::Result<()>;
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Device {
    pub channel: CommunicationChannel,
    pub raw: HashMap<String, Rc<RawRegister>>,
    pub cooked: HashMap<String, CookedRegister>,
    pub computed: HashMap<String, ComputedRegister>,
}

macro_rules! with_register_from_set {
    ($self:ident.$reg_set:ident, $reg_name:ident, $op:tt) => {{
        $self.$reg_set.get($reg_name).ok_or_else(|| {
            format_err!(
                concat!("tried to ", $op, " non existant ", stringify!($reg_set), " register {}"),
                $reg_name
            )
        })?
    }};
}

macro_rules! read_reg_from_set {
    ($self:ident.$reg_set:ident, $reg_name:ident, $extra:expr) => {
        with_register_from_set!($self.$reg_set, $reg_name, "read")
            .read_value($extra)
            .with_context(|e| {
                format!(
                    "error while reading register {}.{}: {}",
                    stringify!($reg_set),
                    $reg_name,
                    e
                )
            })
            .map_err(|e| e.into())
    };
}

macro_rules! write_reg_from_set {
    ($self:ident.$reg_set:ident, $reg_name:ident, $extra:expr, $value:ident) => {
        with_register_from_set!($self.$reg_set, $reg_name, "write to")
            .write_value($value, $extra)
            .with_context(|e| {
                format!(
                    "error while writing to register {}.{}: {}",
                    stringify!($reg_set),
                    $reg_name,
                    e
                )
            })
            .map_err(|e| e.into())
    };
}

impl DeviceLike for Device {
    fn read_raw(&self, name: &str) -> fuseable::Result<Value> {
        read_reg_from_set!(self.raw, name, &self.channel)
    }

    fn write_raw(&self, name: &str, value: Value) -> fuseable::Result<()> {
        write_reg_from_set!(self.raw, name, &self.channel, value)
    }

    fn read_cooked(&self, name: &str) -> fuseable::Result<Value> {
        read_reg_from_set!(self.cooked, name, &self.channel)
    }

    fn write_cooked(&self, name: &str, value: Value) -> fuseable::Result<()> {
        write_reg_from_set!(self.cooked, name, &self.channel, value)
    }

    fn read_computed(&self, name: &str) -> fuseable::Result<Value> {
        read_reg_from_set!(self.computed, name, &self)
    }

    fn write_computed(&self, name: &str, value: Value) -> fuseable::Result<()> {
        write_reg_from_set!(self.computed, name, &self, value)
    }
}

macro_rules! inject {
    ($path_iter:ident, $new_path_iter:ident, $($rules:pat => $code:expr),*) => {
        {
            let (mut peek, mut $new_path_iter) = $path_iter.tee();
            let reg_name = peek.next();
            let reg_field = peek.next();

            match (reg_name, reg_field) {
                $($rules => $code),*
            }
        }
    };
}

macro_rules! inject_read {
    ($actual_path:expr, $self:ident.$path:ident, $path_iter:ident) => {
        {
            inject!($path_iter, path,
                (Some(_), None) => $actual_path.read(&mut path).map(|value| match value {
                    Either::Left(mut dir_entries) => {
                        dir_entries.push("value".to_owned());
                        Either::Left(dir_entries)
                    }
                    Either::Right(_) => {
                        panic!("tought I would get directory entires, but got file content")
                    }
                }),
                (Some(name), Some("value")) => $self.$path(name).and_then(|v| v.display_representation().map(fuseable::Either::Right)),
                _ => $actual_path.read(&mut path)
            )
        }
    };
}

macro_rules! inject_write {
    ($actual_path:expr, $self:ident.$path:ident, $path_iter:ident, $value:ident) => {
        {
            inject!($path_iter, path,
                    (Some(name), Some("value")) => $self.$path(name, Value::String(String::from_utf8($value)?)),
                    _ => $actual_path.write(&mut path, $value)
            )
        }
    };
}


macro_rules! inject_is_dir {
    ($path:expr, $path_iter:ident, $injected_property:ident) => {
        {
            inject!($path_iter, path,
                (Some(name), Some(stringify!($injected_property))) => {
                    $path.is_dir(&mut std::iter::once(name)).map(|_| false)
                },
                _ => $path.is_dir(&mut path)
            )
        }
    };
}

impl Fuseable for Device {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        match path.next() {
            Some("channel") => self.channel.is_dir(path),
            Some("raw") => inject_is_dir!(self.raw, path, value),
            Some("cooked") => inject_is_dir!(self.cooked, path, value),
            Some("computed") => inject_is_dir!(self.computed, path, value),
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(true),
        }
    }

    fn read(
        &self,
        path: &mut dyn Iterator<Item = &str>,
    ) -> fuseable::Result<Either<Vec<String>, Vec<u8>>> {
        match path.next() {
            Some("channel") => self.channel.read(path),
            Some("raw") => inject_read!(self.raw, self.read_raw, path),
            Some("cooked") => inject_read!(self.cooked, self.read_cooked, path),
            Some("computed") => inject_read!(self.computed, self.read_computed, path),
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
            Some("raw") => inject_write!(self.raw, self.write_raw, path, value),
            Some("cooked") => inject_write!(self.cooked, self.write_cooked, path, value),
            Some("computed") => inject_write!(self.computed, self.write_computed, path, value),
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
            #[serde(default)]
            map: Option<ValueMapNonMatched>,
            #[serde(default = "bool_true")]
            writable: bool,
            default: Option<u64>,
            width: Option<u64>,
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
        let raw = raw.into_iter().map(|(k, v)| (k, Rc::new(v))).collect();

        let cooked = cooked
            .into_iter()
            .map(|(name, cooked_reg)| {
                let (address, slice, raw_register) = Address::parse_named(&cooked_reg.address, cooked_reg.width, &raw).map_err(|e| {
                    D::Error::custom(format!(
                        "could not parse the address of this cooked register ({}): {}",
                        cooked_reg.address, e
                    ))
                })?;

                let width_or_raw_register = raw_register.clone()
                    .map(WidthOrRawRegister::RawRegister)
                    .or_else(|| {
                        cooked_reg.width.map(WidthOrRawRegister::Width)
                    })
                    .ok_or_else(|| {
                        D::Error::custom(
                            format!("tried to parse cooked register {:?}, but neither was the base a raw register or a width specified, one of these is necessary", cooked_reg))})?;



                let reg_bytes = if let Some(slice) = slice.clone() {
                    slice.bytes()
                } else if let Some(width) = cooked_reg.width {
                    width
                } else if let Some(r) = raw_register {
                    if let Some(width) = r.width {
                        width
                    } else if let Some(width) = r.address.bytes().ok() {
                        width
                    } else {
                        return Err(D::Error::custom(format!("tried to parse cooked register {:?} but no width was specified", cooked_reg)));
                    }
                } else {
                    return Err(D::Error::custom(format!("tried to parse cooked register {:?} but no width was specified", cooked_reg)));
                };

                Ok((name.clone(), CookedRegister {
                    address,
                    width_or_raw_register,
                    slice,
                    description: cooked_reg.description,
                    map: cooked_reg.map.map(|vm| vm.into_valuemap(reg_bytes)).transpose().map_err(|e| D::Error::custom(format!("{}", e)))?,
                    default: cooked_reg.default,
                    writable: cooked_reg.writable,
                }))
            })
            .collect::<Result<HashMap<String, CookedRegister>, _>>()?;

        Ok(Device { channel, raw, cooked, computed })
    }
}
