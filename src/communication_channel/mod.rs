use crate::{address::Address, serde_util::u64_one, value::Value};
use core::fmt::Debug;
use derivative::*;
use failure::format_err;
use fuseable::{Fuseable, Result};
use fuseable_derive::*;
use i2cdev::{core::I2CDevice, linux::LinuxI2CDevice};

use log::{debug, warn};
use memmap::{MmapMut, MmapOptions};
use paste;
use serde::*;
use serde_derive::{Deserialize, Serialize};
use std::{fs::OpenOptions, sync::RwLock};

use crate::communication_channel::mock_memory::MockMemory;


pub mod mock_memory;

pub type CommunicationChannel = MockableCommChannel;

pub trait CommChannel: Debug + Fuseable {
    fn read_value(&self, address: &Address) -> Result<Value>;
    fn write_value(&self, address: &Address, value: Value) -> Result<()>;
}

#[derive(Derivative, Fuseable)]
#[derivative(Debug)]
pub struct MockableCommChannel {
    comm_channel: Box<dyn CommChannel>,
    #[fuseable(skip)]
    #[derivative(Debug = "ignore")]
    pub mock_memory: MockMemory,
    pub mocked: bool,
}

impl MockableCommChannel {
    fn from(comm_channel: Box<dyn CommChannel>) -> MockableCommChannel {
        MockableCommChannel {
            comm_channel,
            mock_memory: MockMemory::all_zeros(), /* TODO(robin): figure out how to get the
                                                   * device defaults back in on creation? */
            mocked: false,
        }
    }
}

impl CommChannel for MockableCommChannel {
    fn read_value(&self, address: &Address) -> Result<Value> {
        if self.mocked {
            let value = self.mock_memory.read_value(address);
            debug!("mock_read: {:?} at {:?} by {:?}", &value.as_ref().unwrap(), address, self);
            value
        } else {
            self.comm_channel.read_value(address)
        }
    }

    fn write_value(&self, address: &Address, value: Value) -> Result<()> {
        if self.mocked {
            debug!("mock_write: {:?} to {:?} by {:?}", value, address, self);
            self.mock_memory.write_value(address, value)
        } else {
            self.comm_channel.write_value(address, value)
        }
    }
}

#[derive(Derivative, Serialize, Deserialize, Fuseable)]
#[derivative(Debug, PartialEq)]
struct I2CCdev {
    bus: u8,
    address: u8,
    // max 8 supported for now
    address_bytes: u8,
    #[fuseable(skip)]
    #[serde(skip)]
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    dev: RwLock<Option<LinuxI2CDevice>>,
}

#[derive(Derivative, Serialize, Deserialize, Fuseable)]
#[derivative(Debug, PartialEq)]
struct MemoryMap {
    base: u64,
    len: u64,
    #[serde(default = "u64_one")]
    bytes_per_word: u64,
    #[fuseable(skip)]
    #[serde(skip)]
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    dev: RwLock<Option<MmapMut>>,
}

#[derive(Derivative, Serialize, Fuseable)]
#[derivative(Debug, PartialEq)]
struct CMVSPIBridge {
    base: u64,
    len: u64,
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    channel: MemoryMap,
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

        let CMVSPIBridgeConfig { base, len } = CMVSPIBridgeConfig::deserialize(deserializer)?;

        let channel = MemoryMap { base, len, bytes_per_word: 1, dev: RwLock::new(None) };

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

// TODO(robin): replace with value.byte_representation(Some(max_len))
fn to_bytes_be(val: u64, max_len: u64) -> Result<Vec<u8>> {
    let value = val.to_be_bytes();
    let num_zeros = value.len() - max_len as usize;
    let mut all_zeros = true;

    for v in &value[..num_zeros] {
        all_zeros = (*v == 0) && all_zeros;
    }

    if !all_zeros {
        Err(format_err!(
            "tried to convert {} into a array of {} bytes, but it doesn't fit",
            val,
            max_len
        ))
    } else {
        Ok(value[num_zeros..].to_vec())
    }
}

impl CommChannel for I2CCdev {
    fn read_value(&self, address: &Address) -> Result<Value> {
        let (offset, bytes) = address.get_numeric()?;
        let offset = to_bytes_be(offset, self.address_bytes as u64)?;
        let bytes = bytes.ok_or_else(|| format_err!("I2CCdev doesn't support unbounded read"))?;

        with_dev(
            &self.dev,
            |i2c_dev| {
                i2c_dev.write(&offset)?;
                let mut ret = vec![0; bytes as usize];
                i2c_dev.read(&mut ret)?;
                Ok(Value::Bytes(ret))
            },
            || self.init(),
        )
    }

    fn write_value(&self, address: &Address, value: Value) -> Result<()> {
        let (offset, bytes) = address.get_numeric()?;
        let mut offset = to_bytes_be(offset, self.address_bytes as u64)?;
        let value = value.string_to_uint()?;
        let value_bytes = pad_bytes_to_address_bytes(value, bytes, self, address)?;
        offset.extend(value_bytes);

        with_dev(&self.dev, |i2c_dev| i2c_dev.write(&offset).map_err(|e| e.into()), || self.init())
    }
}

impl CommChannel for MemoryMap {
    fn read_value(&self, address: &Address) -> Result<Value> {
        let (offset, bytes) = address.get_numeric()?;
        let offset = (offset * self.bytes_per_word) as usize;
        let bytes =
            bytes.ok_or_else(|| format_err!("MemoryMap doesn't support unbounded read"))? as usize;

        with_dev(
            &self.dev,
            |mmap_dev| {
                mmap_dev
                    .get(offset..(offset + bytes))
                    .map(|v| {
                        let mut v = v.to_vec();
                        v.reverse(); // convert little to big endian
                        Value::Bytes(v)
                    })
                    .ok_or_else(|| {
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

    fn write_value(&self, address: &Address, value: Value) -> Result<()> {
        let (offset, bytes) = address.get_numeric()?;
        let offset = (offset * self.bytes_per_word) as usize;
        let value = value.string_to_uint()?;
        let value_bytes = pad_bytes_to_address_bytes(value, bytes, self, address)?;

        with_dev(
            &self.dev,
            |mmap_dev| {
                // rev to convert from big to little endian
                for (i, byte) in value_bytes.iter().rev().enumerate() {
                    mmap_dev[offset + i] = *byte;
                }
                Ok(())
            },
            || self.init(),
        )
    }
}

// TODO(robin): replace with value.byte_representation(Some(max_len))
fn pad_bytes_to_address_bytes<C: Debug, A: Debug>(
    value: Value,
    bytes: Option<u64>,
    comm_channel: &C,
    address: &A,
) -> Result<Vec<u8>> {
    if bytes.is_none() {
        warn!(
            "Writing value {:?} to address {:?} on comm channel {:?}, but don't know the width of this register",
            value, address, comm_channel
        )
    }

    value.clone().byte_representation(bytes.map(|v| v as usize))
}

fn with_dev<D, F, I, T>(dev: &RwLock<Option<D>>, func: F, init: I) -> Result<T>
where
    F: FnOnce(&mut D) -> Result<T>,
    I: FnOnce() -> Result<D>,
{
    let mut dev = dev.write().unwrap();

    if dev.is_none() {
        *dev = Some(init()?);

        debug!("opened device");
    } else {
        debug!("had cached device");
    }

    match *dev {
        None => {
            unreachable!();
        }
        Some(ref mut d) => func(d),
    }
}

impl CMVSPIBridge {
    fn addr_to_mmap_addr(address: &Address) -> Result<Address> {
        let (spi_reg, bytes) = address.get_numeric()?;
        let base = 4 * spi_reg;

        Ok(Address::Numeric { base, bytes })
    }
}

impl CommChannel for CMVSPIBridge {
    fn read_value(&self, address: &Address) -> Result<Value> {
        self.channel.read_value(&Self::addr_to_mmap_addr(address)?)
    }

    fn write_value(&self, address: &Address, value: Value) -> Result<()> {
        self.channel.write_value(&Self::addr_to_mmap_addr(address)?, value)
    }
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

impl<'de> Deserialize<'de> for MockableCommChannel {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let config = CommChannelConfig::deserialize(deserializer)?;
        Ok(MockableCommChannel::from(config.convert_to_comm_channel()))
    }
}
