use crate::{
    bytes::{FromBytes, ToBytes},
    camera::with_camera,
    device::DeviceLike,
    lua_util::FailureCompat,
    scripts::Script,
};

use failure::format_err;
use fuseable_derive::Fuseable;
use serde::{Deserialize, Deserializer};
use serde_derive::*;
use std::collections::HashMap;

use rlua::{Function, Lua, RegistryKey, ToLua, Value};

#[derive(Debug, Deserialize, Fuseable, Clone)]
enum ArgumentType {
    #[serde(rename = "float")]
    Float,
    #[serde(rename = "int")]
    Int,
    #[serde(rename = "string")]
    String,
    #[serde(rename = "binary")]
    Binary,
}

#[derive(Debug)]
struct Bytes(Vec<u8>);

impl fuseable::Fuseable for Bytes {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        match path.next() {
            None => Ok(false),
            Some(s) => Err(fuseable::FuseableError::not_a_directory(fuseable::type_name(&self), s)),
        }
    }

    fn read(
        &self,
        path: &mut dyn Iterator<Item = &str>,
    ) -> fuseable::Result<fuseable::Either<Vec<String>, Vec<u8>>> {
        match path.next() {
            None => Ok(fuseable::Either::Right(self.0.clone())),
            Some(s) => Err(fuseable::FuseableError::not_a_directory(fuseable::type_name(&self), s)),
        }
    }

    fn write(
        &mut self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
    ) -> fuseable::Result<()> {
        match path.next() {
            None => {
                self.0 = value;
                Ok(())
            }
            Some(s) => Err(fuseable::FuseableError::not_a_directory(fuseable::type_name(&self), s)),
        }
    }
}

#[derive(Debug, Fuseable)]
enum TypedArgument {
    #[fuseable(flatten)]
    Float(f64),
    #[fuseable(flatten)]
    Int(i64),
    #[fuseable(flatten)]
    String(String),
    #[fuseable(flatten)]
    Binary(Bytes),
}

impl TypedArgument {
    fn new(ty: &ArgumentType) -> Self {
        match ty {
            ArgumentType::Float => Self::Float(0.0),
            ArgumentType::Int => Self::Int(0),
            ArgumentType::String => Self::String(String::new()),
            ArgumentType::Binary => Self::Binary(Bytes(Vec::new())),
        }
    }
}

#[derive(Debug, Fuseable)]
pub struct LuaScript {
    #[fuseable(ro)]
    description: String,
    #[fuseable(ro)]
    script: String,
    #[fuseable(ro)]
    uses: Vec<String>,
    #[fuseable(ro)]
    arg_types: HashMap<String, ArgumentType>,

    args: HashMap<String, TypedArgument>,

    #[fuseable(skip)]
    lua_function: Option<RegistryKey>,
}

impl<'de> Deserialize<'de> for LuaScript {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        pub struct LuaScriptWithoutArgs {
            description: String,
            script: String,
            #[serde(default = "Vec::new")]
            uses: Vec<String>,
            #[serde(default = "HashMap::new")]
            args: HashMap<String, ArgumentType>,
        }

        let script = LuaScriptWithoutArgs::deserialize(deserializer)?;
        let LuaScriptWithoutArgs { description, script, uses, args: arg_types } = script;

        let args = arg_types
            .iter()
            .map(|(arg_name, arg_type)| (arg_name.clone(), TypedArgument::new(arg_type)))
            .collect();


        Ok(LuaScript { description, script, uses, arg_types, args, lua_function: None })
    }
}

impl LuaScript {
    pub fn with_no_args(description: String, script: String, uses: Vec<String>) -> Self {
        Self {
            description,
            script,
            uses,
            arg_types: HashMap::new(),
            args: HashMap::new(),
            lua_function: None,
        }
    }

    pub fn init(&mut self, lua_vm: &Lua) {
        let mut devices_unpack = String::new();
        for device_name in &self.uses {
            devices_unpack = format!("{0}local {1} = devices.{1}\n", devices_unpack, device_name);
        }

        let mut args_unpack = String::new();
        for arg_name in self.args.keys() {
            args_unpack = format!("{0}local {1} = args.{1}\n", args_unpack, arg_name);
        }

        lua_vm.context(|ctx| {
            let script = format!(
                "function (devices, args) {} {} {} end",
                devices_unpack, args_unpack, self.script
            );

            self.lua_function = Some(
                ctx.create_registry_value(ctx.load(&script).eval::<Function>().unwrap()).unwrap(),
            );
        })
    }
}

impl Script for LuaScript {
    fn run(
        &self,
        devices: HashMap<String, &dyn DeviceLike>,
        args: HashMap<String, Vec<u8>>,
    ) -> fuseable::Result<Vec<u8>> {
        with_camera(|cam| {
            cam.lua_vm.context(|ctx| {
                let devices_table = ctx.create_table()?;

                ctx.scope(|lua| {
                    for (device_name, device) in devices.iter() {
                        let device_table = ctx.create_table()?;

                        let (raw_tbl, cooked_tbl, computed_tbl) =
                            rw_tables_from_device!(ctx, lua, device);

                        device_table.set("raw", raw_tbl)?;
                        device_table.set("cooked", cooked_tbl)?;
                        device_table.set("computed", computed_tbl)?;

                        devices_table.set(device_name.as_str(), device_table)?;
                    }

                    let args_table = ctx.create_table()?;
                    for (arg_name, arg_type) in &self.arg_types {
                        let arg_value = (&args[arg_name]).clone();

                        let arg_value = match arg_type {
                            ArgumentType::Float => {
                                <f64 as FromBytes>::from_bytes(arg_value)?.to_lua(ctx)?
                            }
                            ArgumentType::Int => {
                                <i64 as FromBytes>::from_bytes(arg_value)?.to_lua(ctx)?
                            }
                            ArgumentType::String => {
                                <String as FromBytes>::from_bytes(arg_value)?.to_lua(ctx)?
                            }
                            ArgumentType::Binary => arg_value.to_lua(ctx)?,
                        };

                        args_table.set(arg_name.clone(), arg_value)?;
                    }

                    ctx.registry_value::<Function>(self.lua_function.as_ref().ok_or_else(
                        || format_err!("cannot read script {:#?} with no get script", self),
                    )?)?
                    .call::<_, Value>((devices_table, args_table))
                    .map_err(|e| e.into())
                    .and_then(ToBytes::to_bytes)
                })
            })
        })
    }

    fn devices(&self) -> Vec<String> { self.uses.clone() }
}
