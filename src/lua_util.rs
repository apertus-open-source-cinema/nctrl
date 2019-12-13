use core::fmt::{self, Display};
use rlua::{Error as LuaError};
use std::{fmt::Debug, sync::Arc, collections::HashMap};
use failure::format_err;

use crate::{camera, device::DeviceLike};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct FailureCompat<E>(E);

impl FailureCompat<failure::Error> {
    pub fn failure_to_lua(e: failure::Error) -> LuaError {
        LuaError::ExternalError(Arc::new(FailureCompat(e)))
    }
}

impl<E: Display> Display for FailureCompat<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lua error: ")?;
        Display::fmt(&self.0, f)
    }
}

impl<E: Display + Debug> std::error::Error for FailureCompat<E> {
    fn description(&self) -> &'static str { "An error has occurred." }
}

// i can't figure out the lifetimes for a function that would do this, so do a
// macro
#[macro_export(local_inner_macros)]
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
            use rlua::Table;

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
            use rlua::Table;

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

pub fn create_lua_vm() -> rlua::Lua {
    let lua_vm = rlua::Lua::new();

    // TODO(robin): this could be optimized by `load`ing them all once at startup
    // however that also comes with some problems like globals depending on other globals
    lua_vm.context(|ctx| {
        let globals = ctx
            .create_function(|_, name: String| {
                camera::globals::<String>(&name).map_err(|e| FailureCompat::failure_to_lua(e.into()))
            })
            .unwrap();

        let meta_table = ctx.create_table().unwrap();

        let global_index = ctx.load(r#" function (table, name) local func, err = load("return " .. globals(name)) return func() end"#).eval::<rlua::Function>().unwrap();

        meta_table.set("__index", global_index).unwrap();

        ctx.globals().set_metatable(Some(meta_table));

        ctx.globals().set("globals", globals).unwrap();

        let script_get = ctx
            .create_function(|ctx,  (_table, name): (rlua::Table, String)| {
                let return_table = ctx.create_table()?;
                let return_metatable = ctx.create_table()?;

                let call = ctx.create_function(move |_ctx, (_, devices): (rlua::Table, rlua::Table)| {
                    camera::with_camera(|cam| {
                        let script = cam.scripts.get(&name)
                            .ok_or_else(|| format_err!("tried to run non existant script {}", name))
                            .map_err(FailureCompat::failure_to_lua)?
                            .lock().unwrap();

                        let devices = devices
                            .pairs()
                            .map(|key_value| {
                                key_value
                                    .map(|(name, device_table)| {
                                        (name, DeviceLikeFromLua { device_table })
                                    })
                            }).collect::<Result<HashMap<String, DeviceLikeFromLua>, _>>()?;

                        script
                            .run(devices
                                 .iter()
                                 .map(|(name, device)| (name.clone(), device as &dyn DeviceLike))
                                 .collect())
                            .map_err(FailureCompat::failure_to_lua)
                    })
                })?;

                return_metatable.set("__call", call)?;
                return_table.set_metatable(Some(return_metatable));

                Ok(return_table)
            }).unwrap();

        let scripts_metatable = ctx.create_table().unwrap();
        scripts_metatable.set("__index", script_get).unwrap();

        let scripts = ctx.create_table().unwrap();
        scripts.set_metatable(Some(scripts_metatable));

        ctx.globals().set("scripts", scripts).unwrap();
    });

    lua_vm
}

pub struct DeviceLikeFromLua<'a> {
    device_table: rlua::Table<'a>,
}

impl<'a> DeviceLike for DeviceLikeFromLua<'a> {
    fn read_raw(&self, name: &str) -> fuseable::Result<String> {
        Ok(self.device_table.get::<_, rlua::Table>("raw")?.get(name)?)
    }

    // TODO(robin): don't cast to string!!
    fn write_raw(&self, name: &str, value: Vec<u8>) -> fuseable::Result<()> {
        let value = String::from_utf8(value)?;
        Ok(self.device_table.get::<_, rlua::Table>("raw")?.set(name, value)?)
    }

    fn read_cooked(&self, name: &str) -> fuseable::Result<String> {
        Ok(self.device_table.get::<_, rlua::Table>("cooked")?.get(name)?)
    }

    fn write_cooked(&self, name: &str, value: Vec<u8>) -> fuseable::Result<()> {
        let value = String::from_utf8(value)?;
        Ok(self.device_table.get::<_, rlua::Table>("cooked")?.set(name, value)?)
    }

    fn read_computed(&self, name: &str) -> fuseable::Result<String> {
        Ok(self.device_table.get::<_, rlua::Table>("computed")?.get(name)?)
    }

    fn write_computed(&self, name: &str, value: Vec<u8>) -> fuseable::Result<()> {
        let value = String::from_utf8(value)?;
        Ok(self.device_table.get::<_, rlua::Table>("computed")?.set(name, value)?)
    }
}
