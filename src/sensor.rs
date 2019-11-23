use crate::{
    address::Address,
    communication_channel::CommunicationChannel,
    serde_util::{bool_true, by_path, by_string_option_num},
    valuemap::*,
};
use failure::format_err;
use fuseable::{type_name, Either, Fuseable, FuseableError};
use fuseable_derive::Fuseable;
use itertools::{izip, Itertools};
use parse_num::parse_num_mask;
use serde::{de::Error, Deserialize, Deserializer};
use serde_derive::*;
use std::{
    collections::HashMap,
    fmt::Debug,
    ops::Deref,
    sync::{Arc, Mutex},
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
pub struct RawRegister {
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

impl<'de> Deserialize<'de> for RawRegister {
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

        Ok(RawRegister {
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

impl RawRegister {
    fn read_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        comm_channel: &CommunicationChannel,
    ) -> fuseable::Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => comm_channel.read_value(&self.address).map(|v| Either::Right(to_hex(v))),
        }
    }

    fn write_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
        comm_channel: &CommunicationChannel,
    ) -> fuseable::Result<()> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => {
                if let Some(width) = self.width {
                    let (mask, mut value) = parse_num_mask(String::from_utf8_lossy(&value))?;

                    if value.len() > width as usize {
                        return Err(format_err!("value {:?} to write was longer ({}) than register {:?} with width of {}", value, value.len(), self, width));
                    }

                    while value.len() < width as usize {
                        value.insert(0, 0); // TODO(robin): which way around?,
                                            // really efficient the other way
                                            // (value.push(0))
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
                                mask.insert(0, 0); // TODO(robin): which way
                                                   // around?, really efficient
                                                   // this way around
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
pub struct Device {
    channel: CommunicationChannel,
    raw: HashMap<String, RawRegister>,
    cooked: HashMap<String, CookedRegister>,
}

impl Device {
    pub fn read_raw(&self, name: &str) -> fuseable::Result<String> {
        println!("reading {}", name);
        self.raw[name].read_value(&mut std::iter::empty(), &self.channel).map(|v| match v {
            Either::Right(s) => s,
            _ => panic!("got directory entries from a register"),
        })
    }

    pub fn write_raw<T: ToString>(&self, name: &str, value: T) -> fuseable::Result<()> {
        println!("writing to {}", name);
        self.raw[name].write_value(
            &mut std::iter::empty(),
            value.to_string().as_bytes().to_vec(),
            &self.channel,
        )
    }

    pub fn read_cooked(&self, name: &str) -> fuseable::Result<String> {
        self.cooked[name].read_value(&mut std::iter::empty(), &self.channel).map(|v| match v {
            Either::Right(s) => s,
            _ => panic!("got directory entries from a register"),
        })
    }

    pub fn write_cooked<T: ToString>(&self, name: &str, value: T) -> fuseable::Result<()> {
        println!("reading {}", name);
        self.cooked[name].write_value(
            &mut std::iter::empty(),
            value.to_string().as_bytes().to_vec(),
            &self.channel,
        )
    }
}

impl Fuseable for Device {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        match path.next() {
            Some("channel") => self.channel.is_dir(path),
            Some("raw") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.raw.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => self.raw.is_dir(&mut path),
                }
            }
            Some("cooked") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => {
                        self.cooked.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => self.cooked.is_dir(&mut path),
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
        match path.next() {
            Some("channel") => self.channel.read(path),
            Some("raw") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(_), None) => self.raw.read(&mut path).map(|value| match value {
                        Either::Left(mut dir_entries) => {
                            dir_entries.push("value".to_owned());
                            Either::Left(dir_entries)
                        }
                        Either::Right(_) => {
                            panic!("tought I would get directory entires, but got file content")
                        }
                    }),
                    (Some(name), Some("value")) => self
                        .raw
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .read_value(&mut std::iter::empty(), &self.channel),
                    _ => self.raw.read(&mut path),
                }
            }
            Some("cooked") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(_), None) => self.cooked.read(&mut path).map(|value| match value {
                        Either::Left(mut dir_entries) => {
                            dir_entries.push("value".to_owned());
                            Either::Left(dir_entries)
                        }
                        Either::Right(_) => {
                            panic!("tought I would get directory entires, but got file content")
                        }
                    }),
                    (Some(name), Some("value")) => self
                        .cooked
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .read_value(&mut std::iter::empty(), &self.channel),
                    _ => self.cooked.read(&mut path),
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Ok(Either::Left(vec![
                "channel".to_owned(),
                "raw".to_owned(),
                "cooked".to_owned(),
            ])),
        }
    }

    fn write(
        &mut self,
        path: &mut dyn Iterator<Item = &str>,
        value: Vec<u8>,
    ) -> fuseable::Result<()> {
        match path.next() {
            Some("channel") => Err(FuseableError::unsupported("write", type_name(&self.channel))),
            Some("raw") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => self
                        .raw
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .write_value(&mut std::iter::empty(), value, &self.channel),
                    _ => self.raw.write(&mut path, value),
                }
            }
            Some("cooked") => {
                let (mut peek, mut path) = path.tee();
                let reg_name = peek.next();
                let reg_field = peek.next();

                match (reg_name, reg_field) {
                    (Some(name), Some("value")) => self
                        .cooked
                        .get(name)
                        .ok_or_else(|| FuseableError::not_found(name))?
                        .write_value(&mut std::iter::empty(), value, &self.channel),
                    _ => self.cooked.write(&mut path, value),
                }
            }
            Some(name) => Err(FuseableError::not_found(name)),
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

impl<'de> Deserialize<'de> for Device {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        struct CookedRegisterStringAddr {
            addr: String,
            desc: Option<Description>,
            #[serde(default, deserialize_with = "deser_valuemap")]
            map: Option<ValueMap>,
            #[serde(default = "bool_true")]
            writable: bool,
            default: Option<u64>,
        }

        #[derive(Debug, Deserialize)]
        struct DeviceConfig {
            channel: CommunicationChannel,
            #[serde(deserialize_with = "by_path", default)]
            raw: HashMap<String, RawRegister>,
            #[serde(deserialize_with = "by_path", default)]
            cooked: HashMap<String, CookedRegisterStringAddr>,
        }

        let settings = DeviceConfig::deserialize(deserializer)?;

        let DeviceConfig { channel, raw, cooked } = settings;

        let cooked = cooked
            .into_iter()
            .map(|(name, cooked_reg)| {
                let addr = Address::parse_named(&cooked_reg.addr, &raw).map_err(|_| {
                    D::Error::custom(format!(
                        "could not parse the address of this cooked register ({})",
                        cooked_reg.addr
                    ))
                })?;

                Ok((name.clone(), CookedRegister {
                    addr,
                    desc: cooked_reg.desc,
                    map: cooked_reg.map,
                    default: cooked_reg.default,
                    writable: cooked_reg.writable,
                }))
            })
            .collect::<Result<HashMap<String, CookedRegister>, _>>()?;

        Ok(Device { channel, raw, cooked })
    }
}

#[derive(Debug, Serialize, Fuseable)]
pub struct CookedRegister {
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

impl CookedRegister  {
    fn read_value(
        &self,
        path: &mut dyn Iterator<Item = &str>,
        comm_channel: &CommunicationChannel,
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
        comm_channel: &CommunicationChannel,
    ) -> fuseable::Result<()> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => {
                let value = match &self.map {
                    Some(map) => map.encode(String::from_utf8(value)?)?,
                    None => {
                        if let Some(width) = self.addr.bytes() {
                            let (mask, mut value) =
                                parse_num_mask(String::from_utf8_lossy(&value))?;

                            if value.len() > width as usize {
                                return Err(format_err!("value {:?} to write was longer ({}) than cooked register {:?} with width of {}", value, value.len(), self, width));
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
                            panic!("the cooked register written to {:?} did not specify a width, don't know what to do", self)
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

                    #[allow(unused_variables)]
                    let for_default = ForDefault::default();

                    $struct_name {
                        description: $desc.to_string(),
                        $($elem: for_default.$elem),*
                    }
                }
            }


            impl Script for $struct_name {
                #[allow(unused_variables)]
                fn read(&$self_read, cam: &Camera) -> fuseable::Result<String> {
                    $(let $regs_read = cam.devices[stringify!($regs_read)].lock().unwrap();)*

                    $body_read
                }
                #[allow(unused_variables)]
                fn write(&$self_write, cam: &Camera, $value_name: Vec<u8>) -> fuseable::Result<()> {
                    $(let $regs_write = cam.devices[stringify!($regs_write)].lock().unwrap();)*

                    $body_write
                }
            }
    };
}

macro_rules! script_config {
    ( $($script:ident => $tag:tt),* ) => {
        fn scripts_from_model(model: &str) -> HashMap<String, Box<dyn Script>> {
            match model {
                $(
                    $tag => $script::get_scripts(),
                )*
                _ => {
                        panic!("unsupported model {}", model);
                }
            }
        }
    }
}

script! {
    "hard resets the sensor and brings it into standby\n"
    Reset { test: u8 } => {
        read => (self) {
            Err(FuseableError::unsupported("read", fuseable::type_name(&self)))
        }
        write [value] => (self, sensor, sensor_io) {
            println!("writing {:?}", value);

            sensor_io.write_raw("reset", 1)?;

            std::thread::sleep(std::time::Duration::from_millis(10));

            sensor_io.write_raw("reset", 0)?;
            sensor.write_cooked("software_reset", 0)?;
            sensor.write_cooked("stream", 1)?;

            Ok(())
        }
    }
}

macro_rules! script_set {
    { $set_name:ident => { $($name:tt:$script:ident),* } } => {
        struct $set_name {}

        impl $set_name {
            fn get_scripts() -> HashMap<String, Box<dyn Script>> {
                let mut map = HashMap::new();

                $(map.insert($name.to_owned(), Box::new($script::default()) as Box<dyn Script>);)*

                map
            }
        }
    };
}

script! {
    "start up the sensor in default settings"
    Kick {} => {
        read => (self) {
            Err(FuseableError::unsupported("read", fuseable::type_name(&self)))
        }
        write [value] => (self, sensor, sensor_io) {
            let extclk = 24000000;
            // init
            // toggle reset (active low)
            sensor_io.write_raw("reset", 0x7)?;
            std::thread::sleep(std::time::Duration::from_millis(1));
            sensor_io.write_raw("reset", 0x0)?;
            std::thread::sleep(std::time::Duration::from_millis(1));
            sensor_io.write_raw("reset", 0x7)?;

            // magic init
            sensor.write_raw("magic_init_config", 0xa114)?;
            sensor.write_raw("magic_init_start", 0x0070)?;

            std::thread::sleep(std::time::Duration::from_millis(1));

            // check chip_version
            let chip_version = sensor.read_raw("chip_version_reg")?;
            // assert(chip_version == "0x2304");

            println!("chip_version {}", chip_version);
            println!("reserved_chiprev {}", sensor.read_raw("reserved_chiprev")?);
            println!("version {}", sensor.read_raw("test_data_red")?);

            /*
            write("magic_patch1", 0x0146);
            write("magic_patch2", 0x88bc);
            write("magic_patch3", 0xaa63);
            write("magic_patch4", 0x00a0);
            */

            fn gcd(mut a: usize, mut b: usize) -> usize {
                while b != 0 {
                    let old_b = b;
                    b = a % b;
                    a = old_b;
                }

                a
            }

            fn optimal_pll_config(extclk: usize, vco_target: usize) -> (usize, usize) {
                let vco_maximum = 768000000; // chip max
                // assert(vco_target < vco_maximum)
                let vco_max = vco_target;
                let vco_min = 384000000;

                let div_min = 1;
                let div_max = 64;

                let mul_min = 32;
                let mul_max = 384;

                let mut div = 0;
                let mut mul = 0;

                for vco in (vco_min..=vco_max).rev() {
                    let i = gcd(extclk, vco);
                    div = extclk / i;
                    mul = vco / i;

                    if (mul <= mul_max) && (mul >= mul_min) && (div <= div_max) {
                        break
                    }

                }

                (div, mul)
            }

            // pll config for 12bit, 4 lane hispi
            let vco_hispi_4lanes_12bit_clk = 588000000; // 588 MHz
            let (pre_pll_clk_div, pll_multiplier)
                = optimal_pll_config(extclk, vco_hispi_4lanes_12bit_clk);

            // taken from table in datasheet, no idea how to calculate on our own
            let vt_sys_clk_div =  2;
            let vt_pix_clk_div =  6;
            let op_sys_clk_div =  2;
            let op_pix_clk_div = 12;

            sensor.write_raw("vt_pix_clk_div", vt_pix_clk_div)?;
            sensor.write_raw("vt_sys_clk_div", vt_sys_clk_div)?;
            sensor.write_raw("pre_pll_clk_div", pre_pll_clk_div)?;
            sensor.write_raw("pll_multiplier", pll_multiplier)?;
            sensor.write_raw("op_pix_clk_div", op_pix_clk_div)?;
            sensor.write_raw("op_sys_clk_div", op_sys_clk_div)?;

            // pll lock time
            std::thread::sleep(std::time::Duration::from_millis(1));

            // data format setting
            // 0xc0c - 12bit raw uncompressed
            sensor.write_raw("data_format_bits", 0x0c0c)?;
            // serial output format
            // select hivcm (1V8)
            sensor.write_raw("datapath_select", 1 << 9)?;


            // hispi enable, test pattern all ones
            // write("hispi_control_status", int("0000 0011 1101 0100".replace(' ', ''), 2))
            // !!!! IMPORTANT  !!!! the 0x0400 bit toggles streaming s -> packetized sp
            sensor.write_raw("hispi_control_status", 0b1000_0100_0000_0000)?;
            sensor.write_raw("mipi_config_status", 0xc)?;

            // 0x0202 - 2 lane mipi
            // 0x0304 - 4 lane hispi
            sensor.write_raw("serial_format", 0x0304)?;

            // test pattern mode
            // 0   - no test pattern
            // 1   - solid color
            // 2   - solid color bars
            // 3   - fade to gray color bars
            // 256 - walking 1s
            sensor.write_raw("test_pattern_mode", 0)?;

            // unlock write to data_pedestal
            sensor.write_raw("reset_register", 0b10000)?;
            sensor.write_raw("test_raw_mode", 2)?;
            sensor.write_raw("data_pedestal", 0)?;

            // dubious, we have duplicate addresses for this one
            // sensor.write_register("dark_control", 0)?;

            sensor.write_raw("analog_gain", 0x0010)?;
            sensor.write_raw("global_gain", 0b0000000010000000)?;
            sensor.write_raw("coarse_integration_time", 1200)?;
            sensor.write_raw("fine_integration_time", 0)?;

            // reset hispi_timing
            sensor.write_raw("hispi_timing", 0b1_000_000_000_000_000)?;
            // streaming enable
            sensor.write_raw("mode_select", 1)?;

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

            sensor_io.write_raw("reset", 1)?;

            std::thread::sleep(std::time::Duration::from_millis(10));

            sensor_io.write_raw("reset", 0)?;
            sensor.write_cooked("software_reset", 0)?;
            sensor.write_cooked("stream", 1)?;

            Ok(())
        }
    }
}

script_set! {
    MicroR2Scripts => {
        "reset": Reset,
        "kick": Kick
    }
}

script_set! {
    BetaScripts => {}
}

script_config!(MicroR2Scripts => "micro-r2", BetaScripts => "beta");

#[derive(Debug)]
pub struct Camera {
    camera_model: String,
    pub devices: HashMap<String, Arc<Mutex<Device>>>,
    scripts: HashMap<String, Box<dyn Script>>,
}

impl Fuseable for Camera {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> fuseable::Result<bool> {
        match path.next() {
            Some("camera_model") => self.camera_model.is_dir(path),
            Some("devices") => self.devices.is_dir(path),
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(name), Some("value")) => {
                        self.scripts.is_dir(&mut std::iter::once(name)).map(|_| false)
                    }
                    _ => self.scripts.is_dir(&mut path),
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
        match path.next() {
            Some("camera_model") => self.camera_model.read(path),
            Some("devices") => self.devices.read(path),
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(_), None) => self.scripts.read(&mut path).map(|value| match value {
                        Either::Left(mut dir_entries) => {
                            dir_entries.push("value".to_owned());
                            Either::Left(dir_entries)
                        }
                        Either::Right(_) => {
                            panic!("tought I would get directory entires, but got file content")
                        }
                    }),
                    (Some(name), Some("value")) => Script::read(
                        self.scripts
                            .get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .deref(),
                        &self,
                    )
                    .map(Either::Right),
                    _ => self.scripts.read(&mut path),
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
        match path.next() {
            Some("camera_model") => Err(FuseableError::unsupported("write", "Camera.camera_model")),
            Some("devices") => self.devices.write(path, value),
            Some("scripts") => {
                let (mut peek, mut path) = path.tee();
                let script_name = peek.next();
                let script_field = peek.next();

                match (script_name, script_field) {
                    (Some(name), Some("value")) => Script::write(
                        self.scripts
                            .get(name)
                            .ok_or_else(|| FuseableError::not_found(name))?
                            .deref(),
                        &self,
                        value,
                    ),
                    _ => self.scripts.write(&mut path, value),
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
            devices: HashMap<String, Arc<Mutex<Device>>>,
        }

        let CameraWithoutScripts { camera_model, devices } =
            CameraWithoutScripts::deserialize(deserializer)?;

        let scripts = scripts_from_model(&camera_model);

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
