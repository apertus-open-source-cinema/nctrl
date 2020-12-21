use crate::{
    address::{Address, Slice},
    bit_slice::{slice, slice_write},
    common::Description,
    communication_channel::{CommChannel, CommunicationChannel},
    registers::RawRegister,
    value::{Bytes, FromValue, Value},
    valuemap::*,
};


use fuseable_derive::Fuseable;

use log::debug;

use serde_derive::*;

use std::{fmt::Debug, rc::Rc};


#[derive(Debug, Serialize, Fuseable)]
pub struct CookedRegister {
    #[fuseable(ro)]
    pub address: Address,
    #[fuseable(ro)]
    pub description: Option<Description>,
    #[serde(default, deserialize_with = "deser_valuemap")]
    pub map: Option<ValueMap>,
    #[serde(default = "bool_false")]
    #[fuseable(ro)]
    pub writable: bool,
    #[fuseable(ro)]
    pub default: Option<u64>,
    #[fuseable(ro)]
    pub slice: Option<Slice>,
    #[fuseable(skip)]
    pub width_or_raw_register: WidthOrRawRegister,
}

#[derive(Debug, Serialize)]
pub enum WidthOrRawRegister {
    Width(u64),
    RawRegister(Rc<RawRegister>),
}

impl CookedRegister {
    fn value_bytes(&self) -> fuseable::Result<u64> {
        Ok(self.slice.as_ref().map(|v| v.bytes()).unwrap_or(self.width()?))
    }

    fn width(&self) -> fuseable::Result<u64> {
        match &self.width_or_raw_register {
            WidthOrRawRegister::Width(width) => Ok(*width),
            WidthOrRawRegister::RawRegister(register) => register.width(),
        }
    }

    fn read_raw_value(&self, comm_channel: &CommunicationChannel) -> fuseable::Result<Value> {
        match &self.width_or_raw_register {
            WidthOrRawRegister::Width(_width) => comm_channel.read_value(&self.address),
            WidthOrRawRegister::RawRegister(register) => register.read_value(comm_channel),
        }
    }

    fn write_raw_value(
        &self,
        comm_channel: &CommunicationChannel,
        value: Value,
    ) -> fuseable::Result<()> {
        match &self.width_or_raw_register {
            WidthOrRawRegister::Width(_width) => comm_channel.write_value(&self.address, value),
            WidthOrRawRegister::RawRegister(register) => register.write_value(value, comm_channel),
        }
    }

    pub fn read_value(&self, comm_channel: &CommunicationChannel) -> fuseable::Result<Value> {
        let value = self.read_raw_value(comm_channel)?;
        let value = Value::Bytes(slice(value.byte_representation(None)?, &self.slice));

        let ret = match &self.map {
            Some(map) => map.lookup(Bytes(
                value.clone().byte_representation(Some(self.value_bytes()? as usize))?,
            )),
            None => Ok(value.clone()),
        };

        debug!("{:?} looked up to {:?}", value, ret);

        ret
    }

    pub fn write_value(
        &self,
        value: Value,
        comm_channel: &CommunicationChannel,
    ) -> fuseable::Result<()> {
        let encoded_value = match &self.map {
            Some(map) => Value::Bytes(map.encode(FromValue::from_value(value.clone())?)?),
            None => value.clone().string_to_uint()?,
        };

        debug!("{:?} encoded to {:?}", value, encoded_value);

        let encoded_value = if self.slice.is_some() {
            let mut old_bytes =
                self.read_raw_value(comm_channel).and_then(|v| v.byte_representation(None))?;

            slice_write(&mut old_bytes, encoded_value.byte_representation(None)?, &self.slice)
                .unwrap();

            Value::Bytes(old_bytes.to_vec())
        } else {
            encoded_value
        };

        self.write_raw_value(comm_channel, encoded_value)
    }
}
