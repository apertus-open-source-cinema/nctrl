use itertools::Itertools;
use serde::{Deserialize, Deserializer};
use serde_derive::*;
use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, Mutex, RwLock},
};

use crate::{
    device::Device,
    scripts::{scripts_from_model, Script},
};
use fuseable::{type_name, Either, Fuseable, FuseableError};

static mut CAMERA: Option<Arc<RwLock<Camera>>> = None;

pub fn camera() -> Arc<RwLock<Camera>> {
    unsafe {
        match CAMERA {
            Some(ref cam) => cam.clone(),
            None => panic!("tried to use camera, but we had none yet"),
        }
    }
}

pub fn set_camera(cam: Camera) { unsafe { CAMERA = Some(Arc::new(RwLock::new(cam))) } }

#[derive(Debug)]
pub struct Camera {
    camera_model: String,
    pub devices: HashMap<String, Mutex<Device>>,
    scripts: HashMap<String, Mutex<Box<dyn Script>>>,
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

        match path.next() {
            Some("camera_model") => cam.camera_model.is_dir(path),
            Some("devices") => cam.devices.is_dir(path),
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(name), Some("value")) => {
                        cam.scripts.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => cam.scripts.is_dir(&mut path),
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(true),
        }
    }

    fn read(
        &self,
        path: &mut dyn Iterator<Item = &str>,
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        let cam = self.camera();

        match path.next() {
            Some("camera_model") => cam.camera_model.read(path),
            Some("devices") => cam.devices.read(path),
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
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
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(Either::Left(vec![
                "camera_model".to_owned(),
                "devices".to_owned(),
                "scripts".to_owned(),
            ])),
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
        }

        let CameraWithoutScripts { camera_model, devices } =
            CameraWithoutScripts::deserialize(deserializer)?;

        let scripts = scripts_from_model(&camera_model)
            .into_iter()
            .map(|(k, v)| (k, Mutex::new(v)))
            .collect();

        Ok(Camera { scripts, camera_model, devices })
    }
}

impl Camera {
    pub fn mocked(&mut self, mock: bool) {
        for rs in self.devices.values_mut() {
            rs.lock().unwrap().channel.mock_mode(mock);
        }
    }
}
