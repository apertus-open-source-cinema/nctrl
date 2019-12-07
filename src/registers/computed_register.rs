use crate::{
    common::{Description, Range},
    device::Device,
    lua_util::FailureCompat,
    camera
};

use fuseable::{type_name, Either, FuseableError};
use fuseable_derive::Fuseable;

use failure::format_err;

use rlua::{Function, RegistryKey, ToLua};

use serde_derive::*;

#[derive(Debug, Deserialize, Fuseable, Clone)]
#[serde(tag = "type")]
enum ComputedRegisterType {
    #[serde(rename = "float")]
    Float,
    #[serde(rename = "int")]
    Int,
    #[serde(rename = "string")]
    String,
}

#[derive(Debug, Deserialize, Fuseable)]
pub struct ComputedRegister {
    #[fuseable(ro)]
    description: Description,
    #[fuseable(ro)]
    get: Option<String>,
    #[fuseable(ro)]
    set: Option<String>,
    #[fuseable(ro)]
    #[serde(flatten)]
    range: Option<Range>,
    #[fuseable(ro)]
    #[serde(flatten)]
    ty: ComputedRegisterType,

    #[fuseable(skip)]
    #[serde(skip)]
    read_function: std::cell::RefCell<Option<RegistryKey>>,
    #[fuseable(skip)]
    #[serde(skip)]
    write_function: std::cell::RefCell<Option<RegistryKey>>,
}

impl ComputedRegister {
    pub fn read_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        device: &Device,
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => camera::with_camera(|camera| camera.lua_vm.context(|lua_ctx| {
                    lua_ctx.scope(|lua| {
                        let raw_table = make_table!(lua_ctx, lua, |name| {
                            device.read_raw(&name).map_err(FailureCompat::failure_to_lua)
                        })?;
                        let cooked_table = make_table!(lua_ctx, lua, |name| {
                            device.read_cooked(&name).map_err(FailureCompat::failure_to_lua)
                        })?;
                        let computed_table = make_table!(lua_ctx, lua, |name| {
                            device.read_computed(&name).map_err(FailureCompat::failure_to_lua)
                        })?;

                        if self.read_function.borrow().is_none() {
                            let script = self.get.as_ref().ok_or_else(|| {
                                format_err!(
                                    "cannot not read computed value {:#?} with no get script",
                                    self
                                )
                            })?;

                            let script = format!("function (raw, cooked, computed) {} end", script);

                            *self.read_function.borrow_mut() =
                                Some(lua_ctx.create_registry_value(
                                    lua_ctx.load(&script).eval::<Function>()?,
                                )?);
                        }

                        lua_ctx
                            .registry_value::<Function>(
                                self.read_function.borrow().as_ref().unwrap(),
                            )?
                            .call::<_, String>((raw_table, cooked_table, computed_table))
                            .map_err(|e| e.into())
                    })
                })
                .map(Either::Right),
        )}
    }

    pub fn write_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
        device: &Device,
    ) -> fuseable::Result<()> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => camera::with_camera(|camera| camera.lua_vm.context(|lua_ctx| {
                lua_ctx.scope(|lua| {
                    let raw_table = make_table!(
                        lua_ctx,
                        lua,
                        |name| { device.read_raw(&name).map_err(FailureCompat::failure_to_lua) },
                        |name, value| {
                            device.write_raw(&name, value).map_err(FailureCompat::failure_to_lua)
                        }
                    )?;

                    let cooked_table = make_table!(
                        lua_ctx,
                        lua,
                        |name| { device.read_cooked(&name).map_err(FailureCompat::failure_to_lua) },
                        |name, value| {
                            device.write_cooked(&name, value).map_err(FailureCompat::failure_to_lua)
                        }
                    )?;

                    let computed_table = make_table!(
                        lua_ctx,
                        lua,
                        |name| {
                            device.read_computed(&name).map_err(FailureCompat::failure_to_lua)
                        },
                        |name, value| {
                            device
                                .write_computed(&name, value)
                                .map_err(FailureCompat::failure_to_lua)
                        }
                    )?;

                    let value = match self.ty {
                        ComputedRegisterType::Float => {
                            String::from_utf8(value)?.parse::<f64>()?.to_lua(lua_ctx)?
                        }
                        ComputedRegisterType::Int => {
                            String::from_utf8(value)?.parse::<i64>()?.to_lua(lua_ctx)?
                        }
                        ComputedRegisterType::String => {
                            String::from_utf8(value)?.to_lua(lua_ctx)?
                        }
                    };


                    if self.write_function.borrow().is_none() {
                        let script = self.set.as_ref().ok_or_else(|| {
                            format_err!(
                                "cannot not write computed value {:#?} with no set script",
                                self
                            )
                        })?;

                        let script =
                            format!("function (value, raw, cooked, computed) {} end", script);

                        *self.write_function.borrow_mut() = Some(
                            lua_ctx
                                .create_registry_value(lua_ctx.load(&script).eval::<Function>()?)?,
                        );
                    }

                    lua_ctx
                        .registry_value::<Function>(self.write_function.borrow().as_ref().unwrap())?
                        .call((value, raw_table, cooked_table, computed_table))
                        .map_err(|e| e.into())
                })
            }),
        )}
    }
}
