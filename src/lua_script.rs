use crate::{scripts::Script, camera::Camera, lua_util::FailureCompat};
use fuseable::Result;
use fuseable_derive::Fuseable;
use serde_derive::*;
use failure::format_err;

use rlua::{Function, RegistryKey};

#[derive(Debug, Fuseable, Deserialize)]
pub struct LuaScript {
    #[fuseable(ro)]
    description: String,
    #[fuseable(ro)]
    get: Option<String>,
    #[fuseable(ro)]
    set: Option<String>,
    #[serde(default = "Vec::new")]
    #[fuseable(ro)]
    uses: Vec<String>,

    #[fuseable(skip)]
    #[serde(skip)]
    read_function: Option<RegistryKey>,
    #[fuseable(skip)]
    #[serde(skip)]
    write_function: Option<RegistryKey>,
}

impl LuaScript {
    pub fn init_functions(&mut self, lua_vm: &rlua::Lua) {
        let mut devices_unpack = String::new();
        for device_name in &self.uses {
            devices_unpack = format!("{0}local {1} = devices.{1}\n", devices_unpack, device_name);
        }

        lua_vm.context(|ctx| {
            // closure capture is shit
            let read_function = &mut self.read_function;
            let write_function = &mut self.write_function;

            self.get.as_ref().map(|script| {
                let script = format!("function (devices) {} {} end", devices_unpack, script);

                *read_function =
                    Some(ctx.create_registry_value(
                        ctx.load(&script).eval::<Function>().unwrap(),
                    ).unwrap());
            });

            self.set.as_ref().map(|script| {
                let script = format!("function (devices, value) {} {} end", devices_unpack, script);

                *write_function =
                    Some(ctx.create_registry_value(
                        ctx.load(&script).eval::<Function>().unwrap(),
                    ).unwrap());
            });
        })
    }
}

macro_rules! with_device_table {
    ($self:expr, $cam:ident, |$ctx:ident, $devices_table:ident| $func:tt) => {
        $cam.lua_vm.context(|$ctx| {
            let mut devices = Vec::new();

            $ctx.scope(|lua| {
                for device_name in &$self.uses {
                    devices.push($cam.devices[device_name].lock().unwrap());
                }

                let $devices_table = $ctx.create_table()?;

                for (device, device_name) in devices.iter().zip(&$self.uses) {
                    let device_table = $ctx.create_table()?;

                    let raw_table = make_table!(
                        $ctx,
                        lua,
                        |name| { device.read_raw(&name).map_err(FailureCompat::failure_to_lua) },
                        |name, value| {
                            device.write_raw(&name, value).map_err(FailureCompat::failure_to_lua)
                        }
                    )?;

                    let cooked_table = make_table!(
                        $ctx,
                        lua,
                        |name| { device.read_cooked(&name).map_err(FailureCompat::failure_to_lua) },
                        |name, value| {
                            device.write_cooked(&name, value).map_err(FailureCompat::failure_to_lua)
                        }
                    )?;

                    let computed_table = make_table!(
                        $ctx,
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

                    device_table.set("raw", raw_table)?;
                    device_table.set("cooked", cooked_table)?;
                    device_table.set("computed", computed_table)?;

                    $devices_table.set(device_name.as_str(), device_table)?;
                }

                $func
            })
        })
    };
}

impl Script for LuaScript {
    fn read_key(&self) -> Option<&RegistryKey> {
        self.read_function.as_ref()
    }

    fn write_key(&self) -> Option<&RegistryKey> {
        self.write_function.as_ref()
    }

    fn read(&self, cam: &Camera) -> Result<String> {
        with_device_table!(&self, cam, |ctx, devices_table| {
            ctx
            .registry_value::<Function>(
                self.read_function.as_ref()
                    .ok_or_else(|| format_err!("cannot read script {:#?} with no get script", self))?
            )?
            .call(devices_table)
                .map_err(|e| e.into())
        })
    }

    fn write(&self, cam: &Camera, value: Vec<u8>) -> Result<()> {
        with_device_table!(&self, cam, |ctx, devices_table| {
            ctx
            .registry_value::<Function>(
                self.write_function.as_ref()
                    .ok_or_else(|| format_err!("cannot write to script {:#?} with no set script", self))?
            )?
            .call((devices_table, String::from_utf8(value)?)) // TODO(robin): implement argument types for registers
                .map_err(|e| e.into())
        })
    }
}
