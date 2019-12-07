use crate::{
    common::{Description, Range},
    device::Device,
    lua_util::FailureCompat,
};

use fuseable::{type_name, Either, FuseableError};
use fuseable_derive::Fuseable;

use failure::format_err;

use rlua::{Function, RegistryKey, Table, ToLua};

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

// i can't figure out the lifetimes for a function that would do this, so do a
// macro
macro_rules! make_table {
    (@gen_read $scope:ident, $table:ident, $read_name:ident, $read:tt) => {
        let read =
            $scope.create_function(move |_, (_table, $read_name): (Table, String)| {
                $read
            })?;

        $table.set("__index", read)?;
    };
    (@gen_write $scope:ident, $table:ident, $write_name:ident, $write_val:ident, $write:tt) => {
        let write =
            $scope.create_function(move |_, (_table, $write_name, $write_val): (Table, String, String)| {
                $write
            })?;

        $table.set("__newindex", write)?;
    };
    ($context:ident, $scope:ident, |$read_name:ident| $read:tt) => {
        {
            let meta_table = $context.create_table()?;

            make_table!(@gen_read $scope, meta_table, $read_name, $read);

            let table = $context.create_table().map(|v| {
                v.set_metatable(Some(meta_table));
                v
            });

            let table: fuseable::Result<Table> = table.map_err(|e| e.into());
            table
        }
    };
    ($context:ident, $scope:ident, |$read_name:ident| $read:tt, |$write_name:ident, $write_val:ident| $write:tt) => {
        {
            let meta_table = $context.create_table()?;

            make_table!(@gen_read $scope, meta_table, $read_name, $read);
            make_table!(@gen_write $scope, meta_table, $write_name, $write_val, $write);

            let table = $context.create_table().map(|v| {
                v.set_metatable(Some(meta_table));
                v
            });

            let table: fuseable::Result<Table> = table.map_err(|e| e.into());
            table
        }

    };
}

impl ComputedRegister {
    pub fn read_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        device: &Device,
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => device
                .lua_vm
                .context(|lua_ctx| {
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
        }
    }

    pub fn write_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
        device: &Device,
    ) -> fuseable::Result<()> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => device.lua_vm.context(|lua_ctx| {
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
        }
    }
}
