use failure::format_err;
use itertools::Itertools;
use serde::{Deserialize, Deserializer};
use serde_derive::*;
use derivative::Derivative;
use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    device::Device,
    scripts::{scripts_from_model, Script},
    serde_util::empty_map,
    lua_script::LuaScript,
    lua_util
};
use fuseable::{type_name, Either, Fuseable, FuseableError};
use fuseable_derive::Fuseable;

static mut CAMERA: Option<Arc<RwLock<Camera>>> = None;

pub fn camera() -> Arc<RwLock<Camera>> {
    unsafe {
        match CAMERA {
            Some(ref cam) => cam.clone(),
            None => panic!("tried to use camera, but we had none yet"),
        }
    }
}

pub fn with_camera<F: FnOnce(&Camera) -> T, T>(func: F) -> T {
    func(&camera().read().unwrap())
}

pub fn globals<T: std::str::FromStr>(name: &str) -> fuseable::Result<T>
where
    <T as std::str::FromStr>::Err: std::error::Error + Sync + Send + 'static,
{
    return (*camera().read().unwrap())
        .globals
        .get(name)
        .ok_or_else(|| format_err!("tried to get non existant global {}", name))
        .and_then(|v| v.parse().map_err(|e: <T as std::str::FromStr>::Err| e.into()))
}

pub fn set_camera(cam: Camera) { unsafe { CAMERA = Some(Arc::new(RwLock::new(cam))) } }

#[derive(Fuseable, Derivative)]
#[derivative(Debug)]
pub struct Camera {
    camera_model: String,
    pub devices: HashMap<String, Mutex<Device>>,
    pub scripts: HashMap<String, Mutex<Box<dyn Script>>>,
    globals: HashMap<String, String>,
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
    ) -> fuseable::Result<Either<Vec<String>, String>> {
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
                        Either::Right(_) => {
                            panic!("tought I would get directory entires, but got file content")
                        }
                    }),
                    (Some(name), Some("value")) => Script::read(
                        &**cam.scripts // wat???
                            .get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .lock().unwrap(),
                        &cam,
                    )
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
            Some("devices") => match path.next() {
                Some(name) => match cam.devices.get(name) {
                    Some(device) => device.lock().unwrap().write(path, value),
                    None => Err(FuseableError::not_found(format!("camera.devices.{}", name))),
                },
                None => Err(FuseableError::unsupported("write", "camera.devices")),
            },
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let path = &mut path;
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(name), Some("value")) => Script::write(
                        &**cam.scripts // wat??
                            .get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .lock().unwrap(),
                        &cam,
                        value,
                    ),
                    (Some(name), _) => match cam.scripts.get(name) {
                        Some(script) => script.lock().unwrap().write(path, value),
                        None => Err(FuseableError::not_found(format!("camera.scripts.{}", name))),
                    },
                    (None, _) => Err(FuseableError::unsupported("write", "camera.devices")),
                }
            }
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
            scripts: HashMap<String, LuaScript>
        }

        let CameraWithoutScripts { camera_model, devices, globals, scripts } =
            CameraWithoutScripts::deserialize(deserializer)?;

        let lua_vm = lua_util::create_lua_vm();

        // doesn't work without the type annotation :(
        let mut scripts: HashMap<String, Mutex<Box<dyn Script>>> = scripts
            .into_iter()
            .map(|(k, mut v)| {
                // init the functions
                v.init_functions(&lua_vm);

                (k, Mutex::new(Box::new(v) as Box<dyn Script>))
            })
            .collect();

        // doesn't work without the type annotation :(
        let rust_scripts: HashMap<String, Mutex<Box<dyn Script>>> = scripts_from_model(&camera_model)
            .into_iter()
            .map(|(k, v)| (k, Mutex::new(v)))
            .collect();

        scripts.extend(rust_scripts);

        Ok(Camera { scripts, camera_model, devices, globals, lua_vm })
    }
}

impl Camera {
    pub fn mocked(&mut self, mock: bool) {
        for rs in self.devices.values_mut() {
            rs.lock().unwrap().channel.mock_mode(mock);
        }
    }
}
