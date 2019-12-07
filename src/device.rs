use crate::{
    address::Address,
    camera,
    common::Description,
    communication_channel::CommunicationChannel,
    lua_util::FailureCompat,
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
use failure::{format_err, ResultExt};

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

pub trait ToStringOrVecU8 {
    fn bytes(self) -> Vec<u8>;
}

impl<T: ToString> ToStringOrVecU8 for T {
    fn bytes(self) -> Vec<u8> { self.to_string().as_bytes().to_vec() }
}

// shitty hack because specialization is not stable
// TODO(robin): revisit when (if) specialization ever lands
// (tracking issue: https://github.com/rust-lang/rust/issues/31844)
struct Bytes(Vec<u8>);

impl ToStringOrVecU8 for Bytes {
    fn bytes(self) -> Vec<u8> { self.0 }
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
            .read_value(&mut std::iter::empty(), $extra)
            .with_context(|e| {
                format!(
                    "error while reading register {}.{}: {}",
                    stringify!($reg_set),
                    $reg_name,
                    e
                )
            })
            .map_err(|e| e.into())
            .map(|v| match v {
                Either::Right(s) => s,
                _ => panic!("got directory entries from a register"),
            })
    };
}

macro_rules! write_reg_from_set {
    ($self:ident.$reg_set:ident, $reg_name:ident, $extra:expr, $value:ident) => {
        with_register_from_set!($self.$reg_set, $reg_name, "write to")
            .write_value(&mut std::iter::empty(), $value.bytes(), $extra)
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

impl Device {
    pub fn read_raw(&self, name: &str) -> fuseable::Result<String> {
        read_reg_from_set!(self.raw, name, &self.channel)
    }

    pub fn write_raw<T: ToStringOrVecU8>(&self, name: &str, value: T) -> fuseable::Result<()> {
        write_reg_from_set!(self.raw, name, &self.channel, value)
    }

    pub fn read_cooked(&self, name: &str) -> fuseable::Result<String> {
        read_reg_from_set!(self.cooked, name, &self.channel)
    }

    pub fn write_cooked<T: ToStringOrVecU8>(&self, name: &str, value: T) -> fuseable::Result<()> {
        write_reg_from_set!(self.cooked, name, &self.channel, value)
    }

    pub fn read_computed(&self, name: &str) -> fuseable::Result<String> {
        read_reg_from_set!(self.computed, name, &self)
    }

    pub fn write_computed<T: ToStringOrVecU8>(&self, name: &str, value: T) -> fuseable::Result<()> {
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
                (Some(name), Some("value")) => $self.$path(name).map(fuseable::Either::Right),
                _ => $actual_path.read(&mut path)
            )
        }
    };
}

macro_rules! inject_write {
    ($actual_path:expr, $self:ident.$path:ident, $path_iter:ident, $value:ident) => {
        {
            inject!($path_iter, path,
                    (Some(name), Some("value")) => $self.$path(name, Bytes($value)),
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
    ) -> fuseable::Result<Either<Vec<String>, String>> {
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

        let lua_vm = Lua::new();

        // TODO(robin): this could be optimized by `load`ing them all once at startup
        // however that also comes with some problems like globals depending on other globals
        lua_vm.context(|ctx| {
            let globals = ctx
                .create_function(|_, name: String| {
                    camera::globals::<String>(&name).map_err(|e| FailureCompat::failure_to_lua(e))
                })
                .unwrap();

            let meta_table = ctx.create_table().unwrap();

            let global_index = ctx.load(r#" function (table, name) local func, err = load("return " .. globals(name)) return func() end"#).eval::<rlua::Function>().unwrap();

            meta_table.set("__index", global_index).unwrap();

            ctx.globals().set_metatable(Some(meta_table));

            ctx.globals().set("globals", globals).unwrap();
        });

        Ok(Device { channel, raw, cooked, computed, lua_vm })
    }
}
