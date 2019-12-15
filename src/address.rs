use crate::registers::RawRegister;
use failure::Error;
use fuseable_derive::Fuseable;
use lazy_static::lazy_static;
use parse_num::{parse_num, parse_num_padded};
use regex::Regex;
use serde_derive::Serialize;
use std::{collections::HashMap, string::String};

#[derive(Debug, PartialEq, Serialize, Fuseable, Clone)]
pub struct Slice {
    pub start: u8,
    pub end: u8,
}

// TODO(robin): consider either
//              making address dependent on the communication channel
//              or making it a enum of u64 and Vec<u8> (or maybe String?)

// base contains the base address (in big endian bytes)
// if slice is Some it specifies the start and stop bit of this Address
// if slice is None, reads shall read from bit 0 until EOF
// and writes shall write starting from bit 0 and write the whole value
#[derive(Debug, PartialEq, Serialize, Fuseable, Clone)]
pub struct Address {
    pub base: Vec<u8>,
    pub slice: Option<Slice>,
}

impl Address {
    fn parse_internal(
        str: &str,
        register_set: Option<&HashMap<String, RawRegister>>,
        width: Option<u8>,
    ) -> Result<Address, Error> {
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r#"^([^\[\]]+)(\[(?:([^\[\]]+)?:([^\[\]]+)?|([^:\[\]]+))\])?$"#)
                    .unwrap();
        }

        match RE.captures(str) {
            Some(captures) => {
                // capture 0 is the whole string
                // capture 1 is the base
                let (base, base_reg) = match captures.get(1) {
                    Some(base) => {
                        let base_str = base.as_str();
                        match parse_num_padded(base_str) {
                            // if the base is a number, use that number as address
                            Ok(v) => (v, None),
                            Err(_) => {
                                // try to lookup the address in the register_set (for named
                                // addresses / computed registers)
                                let base_reg = register_set.and_then(|set| set.get(base_str));

                                let base = match base_reg {
                                    Some(reg) => reg.address.base.clone(),
                                    // if we can't find the address in the register set and it is
                                    // not a number, use the bytes directly
                                    None => base_str.bytes().collect::<Vec<u8>>(),
                                };

                                (base, base_reg)
                            }
                        }
                    }
                    None => {
                        panic!("no base found, lol?");
                    }
                };

                fn parse_slice_num(v: Vec<u8>) -> u8 {
                    if v.len() == 1 {
                        v[0]
                    } else if v.is_empty() {
                        0
                    } else {
                        panic!("sorry slices longer than one u8 not supported (got {:?})", v);
                    }
                }

                // capture 5 is the potential single bit slice
                let (slice_start, slice_end) = match captures.get(5) {
                    Some(single_bit_slice) => {
                        let bit = parse_num(single_bit_slice.as_str()).map(parse_slice_num)?;
                        (Some(bit), Some(bit + 1))
                    }
                    // if there is not single bit slice, we either have no slice, or a slice with
                    // start and end
                    None => {
                        // capture 2 is the potential slice
                        // capture 3 is the potential slice start
                        let slice_start = match captures.get(3) {
                            Some(m) => Some(parse_num(m.as_str()).map(parse_slice_num)?),
                            // no start was specified, so if we are a named register use the start
                            // of that one
                            None => {
                                match base_reg {
                                    Some(r) => r.address.slice.as_ref().map(|s| s.start),
                                    // we are not a named register, so just use zero
                                    None => Some(0),
                                }
                            }
                        };

                        // capture 4 is the potential slice end
                        let slice_end = match captures.get(4) {
                            Some(m) => Some(parse_num(m.as_str()).map(parse_slice_num)?),
                            None => {
                                // again same as start
                                match base_reg {
                                    Some(r) => r.address.slice.as_ref().map(|s| s.end),
                                    // however to get a sensible end, we need the width, as
                                    // otherwise we have no clue how big the register actually
                                    // is, and thus produce a unbounded address
                                    None => width.map(|w| w * 8),
                                }
                            }
                        };

                        (slice_start, slice_end)
                    }
                };

                let slice = slice_end.map(|end| {
                    let start = match slice_start {
                        Some(s) => s,
                        None => 0,
                    };

                    if start >= end {
                        panic!("slice of address {} had a start bigger than or equal to the end bit: {} >= {}", str, start, end)
                    } else {
                        Slice { start, end }
                    }

                });

                Ok(Address { base, slice })
            }

            None => {
                panic!("could not parse address {}", str);
            }
        }
    }

    pub fn parse_named(
        address: &str,
        regs: &HashMap<String, RawRegister>,
    ) -> Result<Address, Error> {
        Address::parse_internal(address, Some(regs), None)
    }

    pub fn parse(address: &str, amount: Option<usize>) -> Result<Address, Error> {
        Address::parse_internal(address, None, amount.map(|v| v as u8))
    }

    pub fn set_base_from_u64(&mut self, mut new_base: u64) {
        let mut base = [0u8; 8];

        for b in base.iter_mut().take(8) {
            *b = (new_base & 0xff) as u8;
            new_base >>= 8;
        }

        self.base = base.to_vec();
    }

    // TODO(robin): this could suffer from endianess fuckup
    // TODO(robin): to fix this use byteorder crate and specify the byteorder of
    // base byteorder of base should be big endian to match all the other stuff
    pub fn as_u64(&self) -> u64 {
        assert!(
            self.base.len() < 9,
            "base should be no longer than 8 bytes, but it is {:?}",
            self.base
        );

        let mut base: u64 = 0;

        for byte in self.base.iter().rev() {
            base <<= 8;
            base |= u64::from(*byte);
        }

        base
    }

    // bytes from base to the end
    pub fn bytes(&self) -> Option<usize> {
        self.slice.as_ref().map(|s| {
            let bits = s.end;
            let extra_byte = if bits % 8 > 0 { 1 } else { 0 };

            (extra_byte + (bits >> 3)) as usize
        })
    }

    // the slice is nontrivial if it doesn't start or end at a byte boundary
    pub fn nontrivial_slice(&self) -> bool {
        match self.slice {
            Some(Slice { start, end }) => ((start % 8) != 0) || ((end % 8) != 0),
            None => true,
        }
    }

    pub fn unbounded(&self) -> bool { self.slice.is_none() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_address_test() {
        assert_eq!(
            Address::parse(&"0x1234[1]".to_string(), Some(2)).map_err(|_| ()),
            Ok(Address { base: vec![0x12, 0x34], slice: Some(Slice { start: 1, end: 2 }) })
        );
        assert_eq!(
            Address::parse(&"0x1234[:1]".to_string(), Some(2)).map_err(|_| ()),
            Ok(Address { base: vec![0x12, 0x34], slice: Some(Slice { start: 0, end: 1 }) })
        );
        assert_eq!(
            Address::parse(&"0x1234[1:]".to_string(), Some(2)).map_err(|_| ()),
            Ok(Address { base: vec![0x12, 0x34], slice: Some(Slice { start: 1, end: 16 }) })
        );
        assert_eq!(
            Address::parse(&"0x1234[1:3]".to_string(), Some(2)).map_err(|_| ()),
            Ok(Address { base: vec![0x12, 0x34], slice: Some(Slice { start: 1, end: 3 }) })
        );
        assert_eq!(
            Address::parse(&"0x1234[0x1:0xa]".to_string(), Some(2)).map_err(|_| ()),
            Ok(Address { base: vec![0x12, 0x34], slice: Some(Slice { start: 1, end: 10 }) })
        );
        assert_eq!(
            Address::parse(&"0x1234".to_string(), Some(2)).map_err(|_| ()),
            Ok(Address { base: vec![0x12, 0x34], slice: Some(Slice { start: 0, end: 16 }) })
        );
        assert_eq!(
            Address::parse(&"0x1234".to_string(), None).map_err(|_| ()),
            Ok(Address { base: vec![0x12, 0x34], slice: None })
        );

        let s = "/sys/class/fpga/fpga_manager0/firmware".to_string();
        assert_eq!(
            Address::parse(&s, Some(2)).map_err(|_| ()),
            Ok(Address { base: s.bytes().collect(), slice: Some(Slice { start: 0, end: 16 }) })
        );

        let s_slice = "/sys/class/fpga/fpga_manager0/firmware[:217]".to_string();
        assert_eq!(
            Address::parse(&s_slice, Some(2)).map_err(|_| ()),
            Ok(Address { base: s.bytes().collect(), slice: Some(Slice { start: 0, end: 217 }) })
        );

        let s = "/sys/class/fpga/fpga_manager0/firmware".to_string();
        assert_eq!(
            Address::parse(&s, None).map_err(|_| ()),
            Ok(Address { base: s.bytes().collect(), slice: None })
        );
    }
}
