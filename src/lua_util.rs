use core::fmt::{self, Display};
use failure::format_err;
use rlua::Error as LuaError;
use std::{collections::HashMap, fmt::Debug, sync::Arc};

use crate::{
    bytes::{FromBytes, ToBytes},
    camera,
    device::DeviceLike,
};

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

// i can't figure out the lifetimes for a function that would do this,
// so take a macro
#[macro_export(local_inner_macros)]
macro_rules! make_table {
    (@gen_read $scope:ident, $table:ident, $read_name:ident, $read:tt) => {
        use crate::bytes::FromBytes;

        let read =
            $scope.create_function(move |_, (_table, $read_name): (rlua::Table, String)| {
                $read.and_then(String::from_bytes).map_err(FailureCompat::failure_to_lua)
            })?;

        $table.set("__index", read)?;
    };
    (@gen_write $scope:ident, $table:ident, $write_name:ident, $write_val:ident, $write:tt) => {
        let write =
            $scope.create_function(move |_, (_table, $write_name, $write_val): (rlua::Table, String, rlua::Value)| {
                $write.map_err(FailureCompat::failure_to_lua)
            })?;

        $table.set("__newindex", write)?;
    };
    (@gen_table $context:ident, $scope:ident, $meta_table:ident, $read_name:ident, $read:block $body:block) => {
        {
            let $meta_table = $context.create_table()?;

            $body

            let table: fuseable::Result<rlua::Table> = Ok($context.create_table().and_then(|v| {
                let read_binary =
                    $scope.create_function(move |_, (_table, $read_name): (rlua::Table, String)| {
                        $read.map_err(FailureCompat::failure_to_lua)
                    })?;

                v.set("read_binary", read_binary)?;

                v.set_metatable(Some($meta_table));
                Ok(v)
            })?);

            table
        }
    };
    ($context:ident, $scope:ident, |$read_name:ident| $read:tt) => {
        make_table!(@gen_table $context, $scope, meta_table, $read_name, $read {
            make_table!(@gen_read $scope, meta_table, $read_name, $read);
        })
    };
    ($context:ident, $scope:ident, |$read_name:ident| $read:tt, |$write_name:ident, $write_val:ident| $write:tt) => {
        make_table!(@gen_table $context, $scope, meta_table, $read_name, $read {
            make_table!(@gen_read $scope, meta_table, $read_name, $read);
            make_table!(@gen_write $scope, meta_table, $write_name, $write_val, $write);
        })
    };
}


// TODO(robin): consider:
// make device_table a metatable (or even a better directly device using
// userdata) and override raw, cooked and computed to return a table with
// metatable that overrides __index and __newindex??


#[macro_export(local_inner_macros)]
macro_rules! rw_tables_from_device {
    ($ctx:ident, $scope:ident, $device:ident) => {{
        let raw_table =
            make_table!($ctx, $scope, |name| { $device.read_raw(&name) }, |name, value| {
                $device.write_raw(&name, value.to_bytes().map_err(FailureCompat::failure_to_lua)?)
            })?;

        let cooked_table =
            make_table!($ctx, $scope, |name| { $device.read_cooked(&name) }, |name, value| {
                $device
                    .write_cooked(&name, value.to_bytes().map_err(FailureCompat::failure_to_lua)?)
            })?;

        let computed_table =
            make_table!($ctx, $scope, |name| { $device.read_computed(&name) }, |name, value| {
                $device
                    .write_computed(&name, value.to_bytes().map_err(FailureCompat::failure_to_lua)?)
            })?;

        (raw_table, cooked_table, computed_table)
    }};
}

#[macro_export(local_inner_macros)]
macro_rules! ro_tables_from_device {
    ($ctx:ident, $scope:ident, $device:ident) => {{
        let raw_table = make_table!($ctx, $scope, |name| { $device.read_raw(&name) })?;
        let cooked_table = make_table!($ctx, $scope, |name| { $device.read_cooked(&name) })?;
        let computed_table = make_table!($ctx, $scope, |name| { $device.read_computed(&name) })?;

        (raw_table, cooked_table, computed_table)
    }};
}

pub fn create_lua_vm() -> rlua::Lua {
    let lua_vm = rlua::Lua::new();

    lua_vm.context(|ctx| {
        let globals = ctx
            .create_function(|_, name: String| {
                camera::globals::<String>(&name).map_err(|e| FailureCompat::failure_to_lua(e.into()))
            })
            .unwrap();

        let meta_table = ctx.create_table().unwrap();

        // TODO(robin): this could be optimized by `load`ing them all once at startup
        // however that also comes with some problems like globals depending on other globals
        let global_index = ctx.load(r#" function (table, name) local func, err = load("return " .. globals(name)) return func() end"#).eval::<rlua::Function>().unwrap();

        meta_table.set("__index", global_index).unwrap();

        ctx.globals().set_metatable(Some(meta_table));

        ctx.globals().set("globals", globals).unwrap();

        let script_get = ctx
            .create_function(|ctx,  (_table, name): (rlua::Table, String)| {
                let return_table = ctx.create_table()?;
                let return_metatable = ctx.create_table()?;

                let call = ctx.create_function(move |_ctx, (_, devices, args): (rlua::Table, rlua::Table, Option<rlua::Table>)| {
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

                        let args = match args {
                            Some(args) => args
                                .pairs::<_, rlua::Value>()
                                .map(|name_value| {
                                    name_value.and_then(|(name, value)| {
                                        value.to_bytes()
                                            .map(|value| (name, value))
                                            .map_err(FailureCompat::failure_to_lua)
                                    })
                                })
                                .collect::<Result<_, _>>()?,
                            None => HashMap::new()
                        };

                        script
                            .run(devices
                                 .iter()
                                 .map(|(name, device)| (name.clone(), device as &dyn DeviceLike))
                                 .collect(), args)
                        // TODO(robin): the most practical is to have a string as return value, as these
                        // are coerced to int / float as necessary by lua
                        // if we ever want binary return values for script, it has to go here
                            .and_then(String::from_bytes)
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
    fn read_raw(&self, name: &str) -> fuseable::Result<Vec<u8>> {
        self.device_table.get::<_, rlua::Table>("raw")?.get::<_, rlua::Value>(name)?.to_bytes()
    }

    fn write_raw(&self, name: &str, value: Vec<u8>) -> fuseable::Result<()> {
        Ok(self.device_table.get::<_, rlua::Table>("raw")?.set(name, value)?)
    }

    fn read_cooked(&self, name: &str) -> fuseable::Result<Vec<u8>> {
        self.device_table.get::<_, rlua::Table>("cooked")?.get::<_, rlua::Value>(name)?.to_bytes()
    }

    fn write_cooked(&self, name: &str, value: Vec<u8>) -> fuseable::Result<()> {
        Ok(self.device_table.get::<_, rlua::Table>("cooked")?.set(name, value)?)
    }

    fn read_computed(&self, name: &str) -> fuseable::Result<Vec<u8>> {
        self.device_table.get::<_, rlua::Table>("computed")?.get::<_, rlua::Value>(name)?.to_bytes()
    }

    fn write_computed(&self, name: &str, value: Vec<u8>) -> fuseable::Result<()> {
        Ok(self.device_table.get::<_, rlua::Table>("computed")?.set(name, value)?)
    }
}
