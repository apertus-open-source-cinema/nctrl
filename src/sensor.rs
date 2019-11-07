use crate::{
    address::Address,
    communication_channel::CommunicationChannel,
    serde_util::{bool_false, by_path, by_string_option_num},
    valuemap::*,
};
use failure::format_err;
use fuseable::{type_name, Either, FuseableError, Fuseable};
use fuseable_derive::Fuseable;
use itertools::{izip, Itertools};
use parse_num::parse_num_mask;
use serde::{de::Error, Deserialize, Deserializer};
use serde_derive::*;
use derivative::Derivative;
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    fmt::Debug,
    ops::Deref
};

#[derive(Debug, Serialize, Deserialize, Fuseable, Clone)]
#[serde(untagged)]
enum Range {
    MinMax { min: i64, max: i64 },
}

#[derive(Debug, Serialize, Deserialize, Fuseable, Clone)]
#[serde(untagged)]
enum Description {
    Simple(String),
    LongAndShort { long: String, short: String },
}

#[derive(Debug, Serialize, Fuseable, Clone)]
pub struct Register {
    #[fuseable(ro)]
    pub address: Address,
    #[fuseable(ro)]
    pub width: Option<u8>,
    #[fuseable(ro)]
    mask: Option<String>,
    #[fuseable(ro)]
    #[serde(flatten)]
    range: Option<Range>,
    #[fuseable(ro)]
    #[serde(default, deserialize_with = "by_string_option_num")]
    default: Option<u64>,
    #[fuseable(ro)]
    description: Option<Description>,
}

impl<'de> Deserialize<'de> for Register {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        pub struct RegisterStringAddr {
            pub address: String,
            pub width: Option<u8>,
            mask: Option<String>,
            #[serde(flatten)]
            range: Option<Range>,
            #[serde(default, deserialize_with = "by_string_option_num")]
            default: Option<u64>,
            description: Option<Description>,
        }

        let reg = RegisterStringAddr::deserialize(deserializer)?;

        let address = Address::parse(&reg.address, reg.width.map(|v| v as usize))
            .map_err(|_| D::Error::custom("error parsing address"))?;

        Ok(Register {
            address,
            width: reg.width,
            mask: reg.mask,
            range: reg.range,
            default: reg.default,
            description: reg.description,
        })
    }
}

fn to_hex(v: Vec<u8>) -> String {
    if !v.is_empty() {
        "0x".to_string() + &v.iter().map(|v| format!("{:02X}", v).to_string()).collect::<String>()
    } else {
        "".to_string()
    }
}

impl Register {
    fn read_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        comm_channel: &CommunicationChannel
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => {
                comm_channel.read_value(&self.address).map(|v| Either::Right(to_hex(v)))
            }
        }
    }

    fn write_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
        comm_channel: &CommunicationChannel
    ) -> fuseable::Result<()> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => {
                println!("writing");

                if let Some(width) = self.width {
                    let (mask, mut value) = parse_num_mask(String::from_utf8_lossy(&value))?;

                    if value.len() > width as usize {
                        return Err(format_err!("value {:?} to write was longer ({}) than register {:?} with width of {}", value, value.len(), self, width));
                    }

                    while value.len() < width as usize {
                        value.insert(0, 0); // TODO(robin): which way around?, really efficient this way around (vs value.push(0))
                    }

                    let value = match mask {
                        Some(mut mask) => {
                            // TODO(robin): this currently interprets a too short value, as if the
                            // missing part should not be assigned and the old value (that is
                            // already in the register) be kept
                            // it is unclear if this is the wanted / intuitive behaviour, or if the
                            // opposite is the case (note this applies only if a mask is specified,
                            // maybe we only want to allow masks, when their width matches the
                            // expected width

                            // TODO(robin): this also needs to account for little endian vs big
                            // endian for value 0x12345678 at 0x0,
                            // little endian has 0x78 is stored at 0x0, 0x56 is stored at 0x1 and so
                            // on big endian has 0x12 stored at 0x0,
                            // 0x34 stored at 0x1 and so on
                            // need to define internal byte order =>
                            // little endian -- not so intuitive
                            // big endian -- would be more efficient and more intuitive
                            while mask.len() < width as usize {
                                mask.insert(0, 0); // TODO(robin): which way around?, really efficient this way around
                            }

                            let current_value = comm_channel.read_value(&self.address)?;

                            izip!(mask, value, current_value)
                                .map(|(m, val, cur)| (val & m) | (cur & !m))
                                .collect()
                        }
                        None => value,
                    };

                    comm_channel.write_value(&self.address, value)
                } else {
                    Err(format_err!("the register written to {:?} did not specify a width, don't know what to do", self))
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct RegisterSetting {
    channel: CommunicationChannel,
    map: HashMap<String, Register>,
    functions: HashMap<String, Function>,
}

impl RegisterSetting {
    pub fn read_register(&self, name: &str) -> fuseable::Result<String> {
        self.map[name].read_value(&mut std::iter::empty(), &self.channel).map(|v| {
            match v {
                Either::Right(s) => s,
                _ => panic!("got directory entries from a register")
            }
        })
    }

    pub fn write_register<T: ToString>(&self, name: &str, value: T) -> fuseable::Result<()> {
        self.map[name].write_value(&mut std::iter::empty(), value.to_string().as_bytes().to_vec(), &self.channel)
    }

    pub fn read_function(&self, name: &str) -> fuseable::Result<String>  {
        self.functions[name].read_value(&mut std::iter::empty(), &self.channel).map(|v| {
            match v {
                Either::Right(s) => s,
                _ => panic!("got directory entries from a register")
            }
        })
    }

    pub fn write_function<T: ToString>(&self, name: &str, value: T) -> fuseable::Result<()>  {
        self.functions[name].write_value(&mut std::iter::empty(), value.to_string().as_bytes().to_vec(), &self.channel)
    }
}

impl Fuseable for RegisterSetting {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        match path.next() {
            Some("channel") => self.channel.is_dir(path),
            Some("map") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.map.is_dir(&mut std::iter::once(name)).map(|_| false)
                    },
                    _ => self.map.is_dir(&mut path)
                }
            }
            Some("functions") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.functions.is_dir(&mut std::iter::once(name)).map(|_| false)
                    },
                    _ => self.functions.is_dir(&mut path)
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(true),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some("channel") => self.channel.read(path),
            Some("map") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(_), None) => {
                        self.map.read(&mut path).map(|value| {
                            match value {
                                Either::Left(mut dir_entries) => {
                                    dir_entries.push("value".to_owned());
                                    Either::Left(dir_entries)
                                },
                                Either::Right(_) => {
                                    panic!("tought I would get directory entires, but got file content")
                                }
                            }
                        })
                    }
                    (Some(name), Some("value")) => {
                        self.map.get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .read_value(&mut std::iter::empty(), &self.channel)
                    },
                    _ => self.map.read(&mut path)
                }
            }
            Some("functions") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(_), None) => {
                        self.functions.read(&mut path).map(|value| {
                            match value {
                                Either::Left(mut dir_entries) => {
                                    dir_entries.push("value".to_owned());
                                    Either::Left(dir_entries)
                                },
                                Either::Right(_) => {
                                    panic!("tought I would get directory entires, but got file content")
                                }
                            }
                        })
                    }
                    (Some(name), Some("value")) => {
                        self.functions.get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .read_value(&mut std::iter::empty(), &self.channel)
                    },
                    _ => self.functions.read(&mut path)
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(Either::Left(vec!["channel".to_owned(), "map".to_owned(), "functions".to_owned()]))
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> fuseable::Result<()> {
        match path.next() {
            Some("channel") => {
                Err(FuseableError::unsupported("write", type_name(&self.channel)))
            }
            Some("map") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.map.get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .write_value(&mut std::iter::empty(), value, &self.channel)
                    },
                    _ => self.map.write(&mut path, value)
                }
            }
            Some("functions") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.functions.get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .write_value(&mut std::iter::empty(), value, &self.channel)
                    },
                    _ => self.functions.write(&mut path, value)
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

impl<'de> Deserialize<'de> for RegisterSetting {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        struct FunctionStringAddr {
            addr: String,
            desc: Option<Description>,
            #[serde(default, deserialize_with = "deser_valuemap")]
            map: Option<ValueMap>,
            #[serde(default = "bool_false")]
            writable: bool,
            default: Option<u64>,
        }

        #[derive(Debug, Deserialize)]
        struct RegisterSettingConfig {
            channel: CommunicationChannel,
            #[serde(deserialize_with = "by_path")]
            map: HashMap<String, Register>,
            #[serde(deserialize_with = "by_path")]
            functions: HashMap<String, FunctionStringAddr>,
        }

        let settings = RegisterSettingConfig::deserialize(deserializer)?;

        let RegisterSettingConfig { channel, map, functions } = settings;

        let functions = functions
            .into_iter()
            .map(|(name, func)| {
                let addr = Address::parse_named(&func.addr, &map).map_err(|_| {
                    D::Error::custom(format!(
                        "could not parse the address of this function ({})",
                        func.addr
                    ))
                })?;

                Ok((
                    name.clone(),
                    Function {
                        addr,
                        desc: func.desc,
                        map: func.map,
                        default: func.default,
                        writable: func.writable,
                    },
                ))
            })
            .collect::<Result<HashMap<String, Function>, _>>()?;

        Ok(RegisterSetting { channel, map, functions })
    }
}

#[derive(Debug, Serialize, Fuseable)]
pub struct Function {
    #[fuseable(ro)]
    addr: Address,
    #[fuseable(ro)]
    desc: Option<Description>,
    // #[fuseable(skip)]
    #[serde(default, deserialize_with = "deser_valuemap")]
    map: Option<ValueMap>,
    #[serde(default = "bool_false")]
    #[fuseable(ro)]
    writable: bool,
    #[fuseable(ro)]
    default: Option<u64>,
}

impl Function {
    fn read_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        comm_channel: &CommunicationChannel
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => {
                let value = comm_channel.read_value(&self.addr)?;

                match &self.map {
                    Some(map) => map.lookup(value).map(Either::Right),
                    None => Ok(Either::Right(to_hex(value))),
                }
            }
        }
    }

    fn write_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
        comm_channel: &CommunicationChannel
    ) -> fuseable::Result<()> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => {
                let value = match &self.map {
                    Some(map) => map.encode(String::from_utf8(value)?)?,
                    None =>  {
                        if let Some(width) = self.addr.bytes() {
                            let (mask, mut value) = parse_num_mask(String::from_utf8_lossy(&value))?;

                            if value.len() > width as usize {
                                return Err(format_err!("value {:?} to write was longer ({}) than function {:?} with width of {}", value, value.len(), self, width));
                            }

                            while value.len() < width as usize {
                                value.insert(0, 0);
                            }

                            match mask {
                                Some(mut mask) => {
                                    while mask.len() < width as usize {
                                        mask.insert(0, 0);
                                    }

                                    let current_value = comm_channel.read_value(&self.addr)?;

                                    izip!(mask, value, current_value)
                                        .map(|(m, val, cur)| (val & m) | (cur & !m))
                                        .collect()
                                }
                                None => value,
                            }
                        } else {
                            panic!("the function written to {:?} did not specify a width, don't know what to do", self)
                        }
                    }
                };

                println!("encoded value: {:?}", value);

                comm_channel.write_value(&self.addr, value)
            }
        }
    }
}

pub trait Script: Debug + Fuseable {
    fn read(&self, cam: &Camera) -> fuseable::Result<String>;
    fn write(&self, cam: &Camera, value: Vec<u8>) -> fuseable::Result<()>;
}

macro_rules! script {
    { $desc:tt $struct_name:ident {$($elem:ident:$elem_typ:ty),*}  => {
        read => ($self_read:ident $(,$regs_read:ident)*) $body_read:block
        write [$value_name:ident] => ($self_write:ident $(,$regs_write:ident)*) $body_write:block
    } } => {
            #[derive(Debug, Fuseable)]
            struct $struct_name {
                description: String,
                $($elem: $elem_typ,)*
            }

            impl Default for $struct_name {
                fn default() -> $struct_name {
                    #[derive(Default)]
                    struct ForDefault {
                        $($elem: $elem_typ,)*
                    }

                    let for_default = ForDefault::default();

                    $struct_name {
                        description: $desc.to_string(),
                        $($elem: for_default.$elem),*
                    }
                }
            }


            impl Script for $struct_name {
                fn read(&$self_read, cam: &Camera) -> fuseable::Result<String> {
                    $(let $regs_read = cam.registers[stringify!($regs_read)].lock().unwrap();)*

                    $body_read
                }
                fn write(&$self_write, cam: &Camera, $value_name: Vec<u8>) -> fuseable::Result<()> {
                    $(let $regs_write = cam.registers[stringify!($regs_write)].lock().unwrap();)*

                    $body_write
                }
            }
    };
}

script! {
    "hard resets the sensor and brings it into standby\n"
    Reset { test: u8 } => {
        read => (self) {
            Err(FuseableError::unsupported("read", fuseable::type_name(&self)))
        }
        write [value] => (self, sensor, sensor_io) {
            println!("writing {:?}", value);

            sensor_io.write_register("reset", 1)?;

            std::thread::sleep(std::time::Duration::from_millis(10));

            sensor_io.write_register("reset", 0)?;
            sensor.write_function("software_reset", 0)?;
            sensor.write_function("stream", 1)?;

            Ok(())
        }
    }
}

script! {
    "hard resets the ar0331 and brings it into standby\n"
    ResetAR0331 { test: u8 } => {
        read => (self) {
            Err(FuseableError::unsupported("read", fuseable::type_name(&self)))
        }
        write [value] => (self, sensor, sensor_io) {
            println!("writing {:?}", value);

            sensor_io.write_register("reset", 1)?;

            std::thread::sleep(std::time::Duration::from_millis(10));

            sensor_io.write_register("reset", 0)?;
            sensor.write_function("software_reset", 0)?;
            sensor.write_function("stream", 1)?;

            Ok(())
        }
    }
}

macro_rules! script_set {
    { $set_name:ident => { $($name:tt:$script:ident),* } } => {
        struct $set_name {}

        impl $set_name {
            fn new() -> HashMap<String, Box<dyn Script>> {
                let mut map = HashMap::new();

                $(map.insert($name.to_owned(), Box::new($script::default()) as Box<dyn Script>);)*

                map
            }
        }
    };
}

script_set! {
    AR0330Scripts => {
        "reset": Reset
    }
}

script_set! {
    AR0331Scripts => {
        "reset": ResetAR0331
    }
}

macro_rules! script_config {
    ( $($script:ident => $tag:tt),* ) => {
        fn scripts_from_model(model: &str) -> HashMap<String, Box<dyn Script>> {
            match model {
                $(
                    $tag => $script::new(),
                )*
                _ => {
                        panic!("unsupported model {}", model);
                }
            }
        }
    }
}

script_config!(AR0330Scripts => "ar0330", AR0331Scripts => "ar0331");

#[derive(Debug)]
pub struct Camera {
    model: String,
    pub registers: HashMap<String, Arc<Mutex<RegisterSetting>>>,
    scripts: HashMap<String, Box<dyn Script>>
}

impl Fuseable for Camera {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        match path.next() {
            Some("model") => self.model.is_dir(path),
            Some("registers") => self.registers.is_dir(path),
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(name), Some("value")) => {
                        self.scripts.is_dir(&mut std::iter::once(name)).map(|_| false)
                    },
                    _ => self.scripts.is_dir(&mut path)
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(true),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some("model") => self.model.read(path),
            Some("registers") => self.registers.read(path),
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(_), None) => {
                        self.scripts.read(&mut path).map(|value| {
                            match value {
                                Either::Left(mut dir_entries) => {
                                    dir_entries.push("value".to_owned());
                                    Either::Left(dir_entries)
                                },
                                Either::Right(_) => {
                                    panic!("tought I would get directory entires, but got file content")
                                }
                            }
                        })
                    }
                    (Some(name), Some("value")) => {
                        Script::read(self.scripts.get(name)
                                     .ok_or_else(|| FuseableError::not_found(name))?.deref(), &self)
                            .map(|v| Either::Right(v))
                    },
                    _ => self.scripts.read(&mut path)
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(Either::Left(vec!["model".to_owned(), "registers".to_owned(), "scripts".to_owned()]))
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> fuseable::Result<()> {
        match path.next() {
            Some("model") => {
                Err(FuseableError::unsupported("write", "Camera.model"))
            }
            Some("registers") => {
                self.registers.write(path, value)
            }
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(name), Some("value")) => {
                        Script::write(self.scripts.get(name)
                                     .ok_or_else(|| FuseableError::not_found(name))?.deref(), &self, value)
                    },
                    _ => self.scripts.write(&mut path, value)
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
            model: String,
            registers: HashMap<String, Arc<Mutex<RegisterSetting>>>,
        }

        let CameraWithoutScripts { model, registers } = CameraWithoutScripts::deserialize(deserializer)?;

        let scripts = scripts_from_model(&model);

        Ok(Camera { scripts, model, registers })
    }
}

impl Camera {
    pub fn mocked(&mut self, mock: bool) {
        for rs in self.registers.values_mut() {
            rs.lock().unwrap().channel.mock_mode(mock);
        }
    }
}
