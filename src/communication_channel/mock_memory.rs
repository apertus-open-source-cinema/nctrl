use crate::{communication_channel::Address, device::Device, value::Value};

use fuseable::Result;

use std::{cell::RefCell, collections::HashMap, sync::MutexGuard};

#[derive(Debug)]
pub struct MockMemory {
    hash_map: RefCell<HashMap<Address, Value>>,
}

impl MockMemory {
    pub fn all_zeros() -> MockMemory { MockMemory { hash_map: RefCell::new(HashMap::new()) } }

    pub fn filled_with_device_defaults(device: &MutexGuard<Device>) -> MockMemory {
        let memory = MockMemory::all_zeros();
        for raw_register in device.raw.values() {
            match &raw_register.default {
                Some(default) => {
                    let address: Address = raw_register.address.clone();

                    match address.bytes() {
                        Ok(bytes) => {
                            assert!(
                                bytes as usize == default.len(),
                                "default value of {:?} wasn't wide enough (has {}, wanted {})",
                                raw_register,
                                default.len(),
                                bytes
                            );
                        }
                        _ => {}
                    }

                    memory.write_value(&address, Value::Bytes(default.to_vec())).unwrap();
                }
                None => {}
            }
        }
        memory
    }

    pub fn write_value(&self, address: &Address, value: Value) -> Result<()> {
        // let value = value.try_determine();
        self.hash_map.borrow_mut().insert(address.clone(), value);
        Ok(())
    }

    pub fn read_value(&self, address: &Address) -> Result<Value> {
        match self.hash_map.borrow().get(&address) {
            Some(v) => Ok(v.clone()),
            None => Ok(Value::Bytes(vec![0; address.bytes()? as usize])),
        }
    }
}
