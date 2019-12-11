use crate::address::{Address, Slice};
use core::fmt::Debug;
use derivative::*;
use failure::format_err;
use fuseable::{Fuseable, Result};
use fuseable_derive::*;
use i2cdev::{core::I2CDevice, linux::LinuxI2CDevice};
use log::debug;
use memmap::{MmapMut, MmapOptions};
use paste;
use serde::*;
use serde_derive::{Deserialize, Serialize};
use std::{fs::OpenOptions, sync::RwLock};
use lazy_static::{lazy_static};
use std::sync::Mutex;

use crate::bit_slice::{slice, slice_write};
use std::collections::HashMap;
use crate::communication_channel::mock_memory::MockMemory;

pub mod mock_memory;

pub type CommunicationChannel = Box<dyn CommChannel>;

lazy_static! {
    static ref MOCK_MEMORIES: Mutex<HashMap<String, MockMemory>> = Mutex::new(HashMap::new());
}


pub trait CommChannel: Debug + Fuseable {
    // these are assumed to be bytewise
    fn read_value_real(&self, address: &Address) -> Result<Vec<u8>>;
    fn write_value_real(&self, address: &Address, value: Vec<u8>) -> Result<()>;

    fn read_value_mock(&self, address: &Address) -> Result<Vec<u8>> {
        let mock_memories = MOCK_MEMORIES.lock().unwrap();
        let mock_memory = mock_memories.get(&format!("{:?}", self)).unwrap();
        let value = mock_memory.read(address).map(|x| x.clone());
        debug!("mock_read: {:?} at {:?} by {:?}", &value.as_ref().unwrap(), address.as_u64(), self);
        value.map(|x| x.clone())
    }

    fn write_value_mock(&self, address: &Address, value: Vec<u8>) -> Result<()> {
        let mut mock_memories = MOCK_MEMORIES.lock().unwrap();
        let mock_memory = mock_memories.get_mut(&format!("{:?}", self)).unwrap();
        mock_memory.write(address, value.clone());
        debug!("mock_write: {:?} to {:?} by {:?}", value.clone(), address.as_u64(), self);
        Ok(())
    }

    fn set_mock(&mut self, mock_memory: MockMemory) {
        MOCK_MEMORIES.lock().unwrap().insert(format!("{:?}", self), mock_memory);
    }
    fn unset_mock(&mut self) {
        MOCK_MEMORIES.lock().unwrap().remove(&format!("{:?}", self));
    }

    // TODO(anuejn): this is probably very slow
    fn get_mock_mode(&self) -> bool {
        MOCK_MEMORIES.lock().unwrap().contains_key(&format!("{:?}", self))
    }

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
        }
        /* else if !address.unbounded() { TODO(robin): we could optimize the case where the slice starts and end at a byte boundary, as we just need to adjust the address base, but that carries the assumption, that addreses are always byte granularity, so maybe we actually cant do that? (for example the CMVSPIBridge has addresses that are 4 bytes per unit increment)
            // the slice starts and ends at a byte boundary and is not unbounded
            let Slice { start, end } = address.slice.as_ref().unwrap();
            value[(start >> 3) as usize..(end >> 3) as usize].to_vec()
        }*/
        else {
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

        let channel = MemoryMap { base, len, dev: RwLock::new(None) };

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

                mmap_dev
                    .get(offset..(offset + bytes))
                    .map(|v| {
                        let mut v = v.to_vec();
                        v.reverse();
                        v
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
    fn addr_to_mmap_addr(address: &Address) -> Address {
        let spi_reg = address.as_u64();
        let base = 4 * spi_reg;
        let mut address = address.clone();
        // clear slice to avoid double handling of it in mock mode
        address.slice = Some(Slice { start: 0, end: (address.bytes().unwrap() * 8) as u8 });
        address.set_base_from_u64(base);
        address
    }
}

impl CommChannel for CMVSPIBridge {
    fn read_value_real(&self, address: &Address) -> Result<Vec<u8>> {
        self.channel.read_value(&Self::addr_to_mmap_addr(address))
    }

    fn write_value_real(&self, address: &Address, value: Vec<u8>) -> Result<()> {
        self.channel.write_value(&Self::addr_to_mmap_addr(address), value)
    }

    fn set_mock(&mut self, mock_memory: MockMemory) {
        self.channel.set_mock(mock_memory)
    }

    fn unset_mock(&mut self) {
        self.channel.unset_mock()
    }

    fn get_mock_mode(&self) -> bool {
        false
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

impl<'de> Deserialize<'de> for Box<dyn CommChannel> {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let config = CommChannelConfig::deserialize(deserializer)?;
        Ok(config.convert_to_comm_channel())
    }
}
