use crate::{scripts::Script, camera::Camera, lua_util::FailureCompat};
use fuseable::Result;
use fuseable_derive::Fuseable;
use serde_derive::*;
use std::iter::FromIterator;
use failure::format_err;

use rlua::{Lua, Function, RegistryKey};

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
    read_function: std::cell::RefCell<Option<RegistryKey>>,
    #[fuseable(skip)]
    #[serde(skip)]
    write_function: std::cell::RefCell<Option<RegistryKey>>,
}

impl Script for LuaScript {
    fn read(&self, cam: &Camera) -> Result<String> {
        cam.lua_vm.context(|ctx| {
            let mut devices = Vec::new();
            let mut devices_arg = String::new();
            let mut first = true;

            ctx.scope(|lua| {
                for device_name in self.uses.clone() {
                    devices.push(cam.devices[&device_name].lock().unwrap());

                    if first {
                        devices_arg = format!("{}", device_name);
                    } else {
                        devices_arg = format!("{}, {}", devices_arg, device_name);
                    }

                    first = false;
                }

                let devices: Result<Vec<_>> = devices.iter().map(|device| {
                    let device_table = ctx.create_table()?;

                    let raw_table = make_table!(
                        ctx,
                        lua,
                        |name| { device.read_raw(&name).map_err(FailureCompat::failure_to_lua) },
                        |name, value| {
                            device.write_raw(&name, value).map_err(FailureCompat::failure_to_lua)
                        }
                    )?;

                    let cooked_table = make_table!(
                        ctx,
                        lua,
                        |name| { device.read_cooked(&name).map_err(FailureCompat::failure_to_lua) },
                        |name, value| {
                            device.write_cooked(&name, value).map_err(FailureCompat::failure_to_lua)
                        }
                    )?;

                    let computed_table = make_table!(
                        ctx,
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

                    Ok(device_table)
                }).collect();

                if self.read_function.borrow().is_none() {
                    let script = self.get.as_ref().ok_or_else(|| {
                        format_err!(
                            "cannot not read script {:#?} with no get script",
                            self
                        )
                    })?;

                    let script = format!("function ({}) {} end", devices_arg, script);

                    *self.read_function.borrow_mut() =
                        Some(ctx.create_registry_value(
                            ctx.load(&script).eval::<Function>()?,
                        )?);
                }

                ctx
                    .registry_value::<Function>(
                        self.read_function.borrow().as_ref().unwrap(),
                    )?
                    .call::<_, String>(rlua::Variadic::from_iter(devices?.into_iter()))
                    .map_err(|e| e.into())

            })
        })
    }

    fn write(&self, cam: &Camera, value: Vec<u8>) -> Result<()> {
        unimplemented!()
    }
}
