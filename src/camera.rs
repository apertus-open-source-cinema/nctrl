// SPDX-FileCopyrightText: © 2019 Jaro Habiger <jarohabiger@googlemail.com>
// SPDX-FileCopyrightText: © 2019 Robin Ole Heinemann <robin.ole.heinemann@gmail.com>
// SPDX-License-Identifier: AGPL-3.0-only

use derivative::Derivative;
use failure::format_err;
use itertools::Itertools;
use serde::{Deserialize, Deserializer};
use serde_derive::*;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Display},
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    communication_channel::mock_memory::MockMemory,
    device::{Device, DeviceLike},
    lua_util,
    scripts::{scripts_from_model, LuaScript, Script},
    serde_util::empty_map,
    value::Value,
};
use fuseable::{type_name, Either, Fuseable, FuseableError};
use fuseable_derive::Fuseable;
use log::trace;

static mut CAMERA: Option<Arc<RwLock<Camera>>> = None;

pub fn camera() -> Arc<RwLock<Camera>> {
    unsafe {
        match CAMERA {
            Some(ref cam) => cam.clone(),
            None => panic!("tried to use camera, but we had none yet"),
        }
    }
}

pub fn with_camera<F: FnOnce(&Camera) -> T, T>(func: F) -> T { func(&camera().read().unwrap()) }

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct GlobalsError<E>(E);


impl From<GlobalsError<failure::Error>> for rlua::Error {
    fn from(err: GlobalsError<failure::Error>) -> rlua::Error {
        rlua::Error::ExternalError(Arc::new(err))
    }
}

impl<E: Display> Display for GlobalsError<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error while reading global variable: ")?;
        Display::fmt(&self.0, f)
    }
}

impl<E: Display + Debug> std::error::Error for GlobalsError<E> {
    fn description(&self) -> &'static str { "An error has occurred." }
}

#[macro_export]
macro_rules! run_script {
    ($name:expr, $devices:ident) => {
        run_script!($name, $devices, {})
    };
    ($name:expr, $devices:ident, {$($arg_name:ident:$arg_val:expr),*}) => {
        crate::camera::with_camera(|cam| {
            #[allow(unused_mut)]
            let mut args: HashMap<String, Value> = HashMap::new();
            $(args.insert(stringify!($arg_name).to_owned(), $arg_val.to_value()?);)*

            cam.scripts
                .get($name)
                .ok_or_else(|| failure::format_err!("tried to run non existant script {}", $name))?
                .lock()
                .unwrap()
                .run($devices.clone(), args)
        })
    }
}

pub fn globals(name: &str) -> Result<Value, GlobalsError<failure::Error>> {
    (*camera().read().unwrap())
        .globals
        .get(name)
        .ok_or_else(|| format_err!("tried to get non existant global {}", name))
        .map(|s| Value::String(s.to_owned()))
        .map_err(GlobalsError)
}

pub fn set_camera(cam: Camera) { unsafe { CAMERA = Some(Arc::new(RwLock::new(cam))) } }

#[derive(Fuseable, Derivative)]
#[derivative(Debug)]
pub struct Camera {
    pub camera_model: String,
    pub devices: HashMap<String, Mutex<Device>>,
    pub scripts: HashMap<String, Mutex<Box<dyn Script>>>,
    globals: HashMap<String, String>,
    init_script: Option<String>,
    #[fuseable(skip)]
    #[derivative(Debug = "ignore")]
    pub lua_vm: rlua::Lua,
}

pub struct SharedCamera {
    pub camera: Arc<RwLock<Camera>>,
}

impl SharedCamera {
    fn camera<'a>(&'a self) -> impl Deref<Target = Camera> + 'a { self.camera.read().unwrap() }
}

impl Fuseable for SharedCamera {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        let cam = self.camera();
        let (mut peek, mut path) = path.tee();
        let field = peek.next();

        match field {
            Some("scripts") => {
                path.next();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(name), Some("value")) => {
                        cam.scripts.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => cam.scripts.is_dir(&mut path),
                }
            }
            _ => cam.is_dir(&mut path),
        }
    }

    fn read(
        &self,
        path: &mut dyn Iterator<Item = &str>,
    ) -> fuseable::Result<Either<Vec<String>, Vec<u8>>> {
        let cam = self.camera();
        let (mut peek, mut path) = path.tee();
        let field = peek.next();

        match field {
            Some("scripts") => {
                path.next();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(_), None) => cam.scripts.read(&mut path).map(|value| match value {
                        Either::Left(mut dir_entries) => {
                            dir_entries.push("value".to_owned());
                            Either::Left(dir_entries)
                        }
                        Either::Right(_) => panic!("tought I would get directory entires, but got file content"),
                    }),
                    (Some(name), Some("value")) => {
                        let script = &**cam.scripts // wat???
                            .get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .lock().unwrap();

                        let device_names = script.devices();

                        let device_handles: Vec<_> =
                            device_names.iter().map(|device_name| cam.devices[device_name].lock().unwrap()).collect();

                        let mut devices: HashMap<String, &dyn DeviceLike> = HashMap::new();

                        for (handle, name) in device_handles.iter().zip(device_names) {
                            devices.insert(name, &**handle as &dyn DeviceLike);
                        }

                        // get the script arguments from fuse
                        let args = match script.read(&mut std::iter::once("args")) {
                            Ok(Either::Left(args)) => args
                                .into_iter()
                                .map(|name| {
                                    let arg_value = match script
                                        .read(&mut std::iter::once("args").chain(std::iter::once(name.as_str())))
                                    {
                                        Ok(Either::Right(val)) => val,
                                        something_else => panic!(
                                            "tried to read argument {} of script {:?}, but got {:?}",
                                            name, script, something_else
                                        ),
                                    };

                                    let arg_value = String::from_utf8(arg_value.clone()).unwrap_or_else(|e| {
                                        panic!(
                                            "tried to convert arg {} (value: {:?}) to string, but a error occured: {}",
                                            name, arg_value, e
                                        )
                                    });

                                    (name.to_string(), Value::String(arg_value))
                                })
                                .collect(),
                            _ => HashMap::new(),
                        };
                        trace!("running script {} with args {:?}", name, args);

                        script.run(devices, args).and_then(|v| v.display_representation())
                    }
                    .map(Either::Right),
                    _ => cam.scripts.read(&mut path),
                }
            }
            _ => cam.read(&mut path),
        }
    }

    fn write(
        &mut self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
    ) -> fuseable::Result<()> {
        let cam = self.camera();

        match path.next() {
            Some("camera_model") => Err(FuseableError::unsupported("write", "Camera.camera_model")),
            Some("init_script") => Err(FuseableError::unsupported("write", "Camera.init_script")),
            Some("devices") => match path.next() {
                Some(name) => match cam.devices.get(name) {
                    Some(device) => device.lock().unwrap().write(path, value),
                    None => Err(FuseableError::not_found(format!("camera.devices.{}", name))),
                },
                None => Err(FuseableError::unsupported("write", "camera.devices")),
            },
            Some("scripts") => match path.next() {
                Some(name) => match cam.scripts.get(name) {
                    Some(script) => script.lock().unwrap().write(path, value),
                    None => Err(FuseableError::not_found(format!("camera.scripts.{}", name))),
                },
                None => Err(FuseableError::unsupported("write", "camera.scripts")),
            },
            Some(name) => Err(FuseableError::not_found(name)),
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

impl<'de> Deserialize<'de> for Camera {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        pub struct CameraWithoutScripts {
            camera_model: String,
            devices: HashMap<String, Mutex<Device>>,
            #[serde(default = "empty_map")]
            globals: HashMap<String, String>,
            #[serde(default = "empty_map")]
            scripts: HashMap<String, LuaScript>,
            #[serde(rename = "init")]
            init_script: Option<String>,
        }

        let CameraWithoutScripts { camera_model, devices, globals, scripts, init_script } =
            CameraWithoutScripts::deserialize(deserializer)?;

        let lua_vm = lua_util::create_lua_vm();

        // doesn't work without the type annotation :(
        let mut scripts: HashMap<String, Mutex<Box<dyn Script>>> = scripts
            .into_iter()
            .map(|(k, mut v)| {
                v.init(&lua_vm);

                (k, Mutex::new(Box::new(v) as Box<dyn Script>))
            })
            .collect();

        // doesn't work without the type annotation :(
        let rust_scripts: HashMap<String, Mutex<Box<dyn Script>>> =
            scripts_from_model(&camera_model)
                .into_iter()
                .map(|(k, v)| (k, Mutex::new(v)))
                .collect();

        scripts.extend(rust_scripts);

        Ok(Camera { scripts, camera_model, devices, globals, lua_vm, init_script })
    }
}

impl Camera {
    pub fn mocked(&mut self, mock: bool) {
        for device in self.devices.values() {
            let mut device = device.lock().unwrap();

            if mock {
                let mock_memory = MockMemory::filled_with_device_defaults(&device);

                device.channel.mock_memory = mock_memory;
            }

            device.channel.mocked = mock;
        }
    }

    pub fn init(&self) -> fuseable::Result<()> {
        self.init_script
            .as_ref()
            .map(|script| {
                let device_names = self.devices.keys().cloned().collect::<Vec<_>>();
                let device_handles: Vec<_> = device_names
                    .iter()
                    .map(|device_name| self.devices[device_name].lock().unwrap())
                    .collect();

                let mut devices: HashMap<String, &dyn DeviceLike> = HashMap::new();

                for (handle, name) in device_handles.iter().zip(&device_names) {
                    devices.insert(name.to_owned(), &**handle as &dyn DeviceLike);
                }

                let mut script = crate::scripts::LuaScript::with_no_args(
                    "".to_owned(),
                    script.to_owned(),
                    device_names,
                );

                script.init(&self.lua_vm);
                script.run(devices, HashMap::new())?;

                Ok(())
            })
            .unwrap_or(Ok(()))
    }
}
