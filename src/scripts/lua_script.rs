use crate::{camera::with_camera, device::DeviceLike, lua_util::FailureCompat, scripts::Script};

use failure::format_err;
use fuseable_derive::Fuseable;
use serde_derive::*;
use std::collections::HashMap;

use rlua::{Function, RegistryKey};

#[derive(Debug, Fuseable, Deserialize)]
pub struct LuaScript {
    #[fuseable(ro)]
    description: String,
    #[fuseable(ro)]
    script: String,
    #[serde(default = "Vec::new")]
    #[fuseable(ro)]
    uses: Vec<String>,

    #[fuseable(skip)]
    #[serde(skip)]
    lua_function: Option<RegistryKey>,
}

impl LuaScript {
    pub fn init_functions(&mut self, lua_vm: &rlua::Lua) {
        let mut devices_unpack = String::new();
        for device_name in &self.uses {
            devices_unpack = format!("{0}local {1} = devices.{1}\n", devices_unpack, device_name);
        }

        lua_vm.context(|ctx| {
            let script = format!("function (devices) {} {} end", devices_unpack, self.script);

            self.lua_function = Some(
                ctx.create_registry_value(ctx.load(&script).eval::<Function>().unwrap()).unwrap(),
            );
        })
    }
}

impl Script for LuaScript {
    fn run(
        &self,
        devices: HashMap<String, &dyn DeviceLike>, /* , args: HashMap<String, Vec<u8>> */
    ) -> fuseable::Result<String> {
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

                    ctx.registry_value::<Function>(self.lua_function.as_ref().ok_or_else(
                        || format_err!("cannot read script {:#?} with no get script", self),
                    )?)?
                    .call(devices_table)
                    .map_err(|e| e.into())
                })
            })
        })
    }

    fn devices(&self) -> Vec<String> { self.uses.clone() }
}
