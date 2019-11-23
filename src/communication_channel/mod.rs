use crate::address::{Address, Slice};
use core::fmt::Debug;
use derivative::*;
use failure::format_err;
use fuseable::{Either, Fuseable, Result};
use fuseable_derive::*;
use i2cdev::{core::I2CDevice, linux::LinuxI2CDevice};
use memmap::{MmapMut, MmapOptions};
use paste;
use serde::*;
use serde_derive::{Deserialize, Serialize};
use std::{fs::OpenOptions, sync::RwLock};

use crate::bit_slice::{slice, slice_write};

pub type CommunicationChannel = Box<dyn CommChannel>;

pub trait CommChannel: Debug + Fuseable {
    // these are assumed to be bytewise
    fn read_value_real(&self, address: &Address) -> Result<Vec<u8>>;
    fn write_value_real(&self, address: &Address, value: Vec<u8>) -> Result<()>;

    fn read_value_mock(&self, address: &Address) -> Result<Vec<u8>> {
        println!("MOCK READ {:?} bytes at {:?} by {:?}", address.bytes(), address, self);
        Ok(vec![0; address.bytes().unwrap_or(0)])
    }

    fn write_value_mock(&self, address: &Address, value: Vec<u8>) -> Result<()> {
        println!("MOCK WRITE {:?} to {:?} by {:?}", value, address, self);
        Ok(())
    }

    fn mock_mode(&mut self, mock: bool);
    fn get_mock_mode(&self) -> bool;

    fn read_value(&self, address: &Address) -> Result<Vec<u8>> {
        let v = if self.get_mock_mode() {
            self.read_value_mock(&address)
        } else {
            self.read_value_real(&address)
        };

        v.map(|v| slice(v, address))
    }

    fn write_value(&self, address: &Address, value: Vec<u8>) -> Result<()> {
        let new_value = if !address.unbounded() {
            let mut old_value = if self.get_mock_mode() {
                self.read_value_mock(address)?
            } else {
                self.read_value_real(address)?
            };

            slice_write(&mut old_value, value, address);

            old_value
        } /* else if !address.unbounded() { TODO(robin): we could optimize the case where the slice starts and end at a byte boundary, as we just need to adjust the address base, but that carries the assumption, that addreses are always byte granularity, so maybe we actually cant do that? (for example the CMVSPIBridge has addresses that are 4 bytes per unit increment)
            // the slice starts and ends at a byte boundary and is not unbounded
            let Slice { start, end } = address.slice.as_ref().unwrap();
            value[(start >> 3) as usize..(end >> 3) as usize].to_vec()
        }*/ else {
            value
        };

        if self.get_mock_mode() {
            self.write_value_mock(&address, new_value)
        } else {
            self.write_value_real(&address, new_value)
        }
    }
}

#[derive(Derivative, Serialize, Deserialize, Fuseable)]
#[derivative(Debug, PartialEq)]
struct I2CCdev {
    bus: u8,
    address: u8,
    #[fuseable(skip)]
    #[serde(skip)]
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    dev: RwLock<Option<LinuxI2CDevice>>,
    #[fuseable(ro)]
    #[serde(skip)]
    mock: bool,
}

#[derive(Derivative, Serialize, Deserialize, Fuseable)]
#[derivative(Debug, PartialEq)]
struct MemoryMap {
    base: u64,
    len: u64,
    #[fuseable(skip)]
    #[serde(skip)]
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    dev: RwLock<Option<MmapMut>>,
    #[fuseable(ro)]
    #[serde(skip)]
    mock: bool,
}

#[derive(Derivative, Serialize, Fuseable)]
#[derivative(Debug, PartialEq)]
struct CMVSPIBridge {
    base: u64,
    len: u64,
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    channel: MemoryMap
}

impl<'de> Deserialize<'de> for CMVSPIBridge {
    fn deserialize<D>(deserializer: D) -> core::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        pub struct CMVSPIBridgeConfig {
            base: u64,
            len: u64,
        }

        let CMVSPIBridgeConfig { base, len } =
            CMVSPIBridgeConfig::deserialize(deserializer)?;

        let channel = MemoryMap { base, len, dev: RwLock::new(None), mock: false };

        Ok(CMVSPIBridge { base, len, channel })
    }
}

impl I2CCdev {
    fn init(&self) -> Result<LinuxI2CDevice> {
        LinuxI2CDevice::new(format!("/dev/i2c-{}", self.bus), u16::from(self.address))
            .map_err(|e| e.into())
    }
}

impl MemoryMap {
    fn init(&self) -> Result<MmapMut> {
        unsafe {
            let file = OpenOptions::new().read(true).write(true).create(true).open("/dev/mem")?;

            MmapOptions::new()
                .len(self.len as usize)
                .offset(self.base)
                .map_mut(&file)
                .map_err(|e| e.into())
        }
    }
}

impl CommChannel for I2CCdev {
    fn read_value_real(&self, address: &Address) -> Result<Vec<u8>> {
        with_dev(
            &self.dev,
            |i2c_dev| {
                i2c_dev.write(&address.base)?;
                let mut ret = vec![
                    0;
                    address.bytes().ok_or_else(|| format_err!(
                        "I2CCdev doesn't support unbounded read"
                    ))?
                ];
                i2c_dev.read(&mut ret)?;
                Ok(ret)
            },
            || self.init(),
        )
    }

    fn write_value_real(&self, address: &Address, value: Vec<u8>) -> Result<()> {
        let mut tmp = Vec::new();
        tmp.extend(&address.base);
        tmp.extend(value);

        with_dev(&self.dev, |i2c_dev| i2c_dev.write(&tmp).map_err(|e| e.into()), || self.init())
    }

    fn mock_mode(&mut self, mock: bool) { self.mock = mock; }

    fn get_mock_mode(&self) -> bool { self.mock }
}

impl CommChannel for MemoryMap {
    fn read_value_real(&self, address: &Address) -> Result<Vec<u8>> {
        let offset = address.as_u64() as usize;

        with_dev(
            &self.dev,
            |mmap_dev| {
                let bytes = address
                    .bytes()
                    .ok_or_else(|| format_err!("MemoryMap doesn't support unbounded read"))?;

                mmap_dev.get(offset..(offset + bytes)).map(|v| {
                    let mut v = v.to_vec();
                    v.reverse();
                    v
                }).ok_or_else(|| {
                    format_err!(
                        "could not read region {} to {} at {} of /dev/mem",
                        offset,
                        offset + bytes,
                        self.base
                    )
                })
            },
            || self.init(),
        )
    }

    fn write_value_real(&self, address: &Address, value: Vec<u8>) -> Result<()> {
        let offset = address.as_u64() as usize;

        with_dev(
            &self.dev,
            |mmap_dev| {
                for (i, byte) in value.iter().rev().enumerate() {
                    mmap_dev[offset + i] = *byte;
                }
                Ok(())
            },
            || self.init(),
        )
    }

    fn mock_mode(&mut self, mock: bool) { self.mock = mock; }

    fn get_mock_mode(&self) -> bool { self.mock }
}

fn with_dev<D, F, I, T>(dev: &RwLock<Option<D>>, func: F, init: I) -> Result<T>
where
    F: FnOnce(&mut D) -> Result<T>,
    I: FnOnce() -> Result<D>,
{
    let mut dev = dev.write().unwrap();

    if dev.is_none() {
        *dev = Some(init()?);

        println!("opened device");
    } else {
        println!("had cached device");
    }

    match *dev {
        None => {
            unreachable!();
        }
        Some(ref mut d) => func(d),
    }
}

impl CMVSPIBridge  {
    fn addr_to_mmap_addr(address: &Address) -> Address {
        let spi_reg = address.as_u64();
        let base = 4 * spi_reg;
        let mut address = address.clone();
        address.set_base_from_u64(base);
        address
    }
}

impl CommChannel for CMVSPIBridge {
    fn read_value_real(&self, address: &Address) -> Result<Vec<u8>> {
        self.channel.read_value_real(&Self::addr_to_mmap_addr(address))
    }

    fn write_value_real(&self, address: &Address, value: Vec<u8>) -> Result<()> {
        self.channel.write_value_real(&Self::addr_to_mmap_addr(address), value)
    }

    fn mock_mode(&mut self, mock: bool) { self.channel.mock_mode(mock); }

    fn get_mock_mode(&self) -> bool { self.channel.get_mock_mode() }
}

macro_rules! comm_channel_config {
    ( $($struct:ident => $tag:tt),* ) => {
        paste::item!{
            #[derive(Debug, PartialEq, Serialize, Deserialize, Fuseable)]
            #[serde(tag = "mode")]
            enum CommChannelConfig {
                $(
                    #[serde(rename = $tag)]
                    [<$struct Channel___>] {
                        #[serde(flatten)]
                        channel: $struct,
                    },
                )*
            }

            impl CommChannelConfig {
                fn convert_to_comm_channel(self) -> Box<dyn CommChannel> {
                    match self {
                        $(
                            CommChannelConfig::[<$struct Channel___>] { channel } => { Box::new(channel) },
                        )*
                    }
                }
            }
        }
    }
}

comm_channel_config!(I2CCdev => "i2c-cdev", MemoryMap => "memory-map", CMVSPIBridge => "cmv-spi-bridge");

impl<'de> Deserialize<'de> for Box<dyn CommChannel> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let config = CommChannelConfig::deserialize(deserializer)?;
        Ok(config.convert_to_comm_channel())
    }
}
