// SPDX-FileCopyrightText: © 2019 Robin Ole Heinemann <robin.ole.heinemann@gmail.com>
// SPDX-License-Identifier: AGPL-3.0-only

use crate::registers::RawRegister;
use failure::{format_err, Error};
use fuseable_derive::Fuseable;
use lazy_static::lazy_static;
use parse_num::{parse_num, parse_num_padded};
use regex::Regex;
use serde_derive::Serialize;
use std::{collections::HashMap, rc::Rc, string::String};

#[derive(Debug, PartialEq, Serialize, Fuseable, Clone)]
pub struct Slice {
    pub start: u64,
    pub end: u64,
}

impl Slice {
    pub fn bytes(&self) -> u64 { (self.end - self.start + 7) >> 3 }
}

// TODO(robin): reworking Address
// enum for different types (u64, String, long bytes)
// seperate slice off it, only cooked has a slice

// TODO(robin): consider either
//              making address dependent on the communication channel
//              or making it a enum of u64 and Vec<u8> (or maybe String?)

// base contains the base address (in big endian bytes)
// if slice is Some it specifies the start and stop bit of this Address
// if slice is None, reads shall read from bit 0 until EOF
// and writes shall write starting from bit 0 and write the whole value


// NOTE(robin): keep in lockstep with AddressBase
#[derive(Debug, Clone, PartialEq, Fuseable, Eq, Hash, Serialize)]
pub enum Address {
    Numeric { base: u64, bytes: Option<u64> },
    String { base: String, bytes: Option<u64> },
}

impl Address {
    fn parse_internal(
        str: &str,
        register_set: Option<&HashMap<String, Rc<RawRegister>>>,
        width: Option<u64>,
    ) -> Result<(Address, Option<Slice>, Option<Rc<RawRegister>>), Error> {
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
                            Ok(v) => (
                                Address::Numeric { base: Address::to_u64(&v.1), bytes: width },
                                None,
                            ),
                            Err(_) => {
                                // try to lookup the address in the register_set (for named
                                // addresses / computed registers)
                                let base_reg = register_set.and_then(|set| set.get(base_str));

                                let base = match base_reg {
                                    Some(reg) => reg.address.clone(),

                                    // if we can't find the address in the register set and it is
                                    // not a number, it must be a String
                                    None => {
                                        Address::String { base: base_str.to_owned(), bytes: width }
                                    }
                                };

                                (base, base_reg)
                            }
                        }
                    }
                    None => {
                        panic!("no base found, lol?");
                    }
                };

                fn parse_slice_num(v: (bool, Vec<u8>)) -> u64 {
                    let v = v.1;
                    if v.len() == 1 {
                        v[0] as u64
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
                            None => Some(0),
                        };

                        // capture 4 is the potential slice end
                        let slice_end = match captures.get(4) {
                            Some(m) => Some(parse_num(m.as_str()).map(parse_slice_num)?),
                            None => width.map(|w| w * 8),
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
                        panic!(
                            "slice of address {} had a start bigger than or equal to the end bit: {} >= {}",
                            str, start, end
                        )
                    } else {
                        Slice { start, end }
                    }
                });

                Ok((base, slice, base_reg.cloned()))
            }

            None => {
                panic!("could not parse address {}", str);
            }
        }
    }

    pub fn parse_named(
        address: &str,
        width: Option<u64>,
        regs: &HashMap<String, Rc<RawRegister>>,
    ) -> Result<(Address, Option<Slice>, Option<Rc<RawRegister>>), Error> {
        Address::parse_internal(address, Some(regs), width)
    }

    pub fn parse(address: &str, width: Option<u64>) -> Result<Address, Error> {
        Address::parse_internal(address, None, width).map(|v| v.0)
    }

    /*
    pub fn base(&self) -> AddressBase {
        match self {
            Address::Numeric { base, bytes: _bytes } => AddressBase::Numeric { base: base.clone() },
            Address::String { base, bytes: _bytes } => AddressBase::String { base: base.clone() },
        }
    }
    */

    pub fn bytes(&self) -> Result<u64, Error> {
        match self {
            Address::Numeric { base: _, bytes } | Address::String { base: _, bytes } => bytes
                .ok_or_else(|| {
                    format_err!("tried to get bytes of {:?} but it has no bytes specified", self)
                }),
            _ => Err(format_err!("tried to get bytes of address {:?}", self)),
        }
    }

    pub fn get_numeric(&self) -> Result<(u64, Option<u64>), Error> {
        match self {
            Address::Numeric { base, bytes } => Ok((*base, *bytes)),
            _ => Err(format_err!("tried to cast address {:?} to Numeric", self)),
        }
    }

    /*
    pub fn from_base_and_width(base: Vec<u8>, width: u64) -> Self {
        Address { base, slice: Some(Slice { start: 0, end: (width * 8) as u8 }) }
    }

    pub fn set_base_from_u64(&mut self, mut new_base: u64) {
        let mut base = [0u8; 8];

        for b in base.iter_mut().take(8) {
            *b = (new_base & 0xff) as u8;
            new_base >>= 8;
        }

        self.base = base.to_vec();
    }
    */

    // TODO(robin): this could suffer from endianess fuckup
    // TODO(robin): to fix this use byteorder crate and specify the byteorder of
    // base byteorder of base should be big endian to match all the other stuff
    fn to_u64(v: &Vec<u8>) -> u64 {
        assert!(v.len() < 9, "trying to convert {:?} to u64 but it is longer than 8 bytes", v);

        let mut base: u64 = 0;

        for byte in v.iter()
        /* .rev() ????? */
        {
            base <<= 8;
            base |= u64::from(*byte);
        }

        base
    }

    /*
    // bytes from base to the end
    pub fn bytes(&self) -> Option<usize> {
        self.slice.as_ref().map(|s| {
            let bits = s.end;
            let bits_rounded_up = bits + 7;
            (bits_rounded_up >> 3) as usize
        })
    }

    pub fn has_slice(&self) -> bool { self.slice.is_some() }

    // the slice is nontrivial if it doesn't start at 0 and doesn't end at a byte
    // boundary
    pub fn nontrivial_slice(&self) -> bool {
        match self.slice {
            Some(Slice { start, end }) => (start != 0) || ((end % 8) != 0),
            None => true,
        }
    }

    pub fn unbounded(&self) -> bool { self.slice.is_none() }
    */
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_address_test() {
        let regs = HashMap::new();

        assert_eq!(
            Address::parse_named(&"0x1234[1]".to_string(), Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::Numeric { base: 0x1234, bytes: Some(2) },
                Some(Slice { start: 1, end: 2 }),
                None
            ))
        );
        assert_eq!(
            Address::parse_named(&"0x1234[:1]".to_string(), Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::Numeric { base: 0x1234, bytes: Some(2) },
                Some(Slice { start: 0, end: 1 }),
                None
            ))
        );
        assert_eq!(
            Address::parse_named(&"0x1234[1:]".to_string(), Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::Numeric { base: 0x1234, bytes: Some(2) },
                Some(Slice { start: 1, end: 16 }),
                None
            ))
        );
        assert_eq!(
            Address::parse_named(&"0x1234[1:3]".to_string(), Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::Numeric { base: 0x1234, bytes: Some(2) },
                Some(Slice { start: 1, end: 3 }),
                None
            ))
        );
        assert_eq!(
            Address::parse_named(&"0x1234[0x1:0xa]".to_string(), Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::Numeric { base: 0x1234, bytes: Some(2) },
                Some(Slice { start: 1, end: 10 }),
                None
            ))
        );
        assert_eq!(
            Address::parse_named(&"0x1234".to_string(), Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::Numeric { base: 0x1234, bytes: Some(2) },
                Some(Slice { start: 0, end: 16 }),
                None
            ))
        );
        assert_eq!(
            Address::parse_named(&"0x1234".to_string(), None, &regs).map_err(|_| ()),
            Ok((Address::Numeric { base: 0x1234, bytes: None }, None, None))
        );

        let s = "/sys/class/fpga/fpga_manager0/firmware".to_string();
        assert_eq!(
            Address::parse_named(&s, Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::String { base: s.clone(), bytes: Some(2) },
                Some(Slice { start: 0, end: 16 }),
                None
            ))
        );

        let s_slice = "/sys/class/fpga/fpga_manager0/firmware[:217]".to_string();
        assert_eq!(
            Address::parse_named(&s_slice, Some(2), &regs).map_err(|_| ()),
            Ok((
                Address::String { base: s, bytes: Some(2) },
                Some(Slice { start: 0, end: 217 }),
                None
            ))
        );

        let s = "/sys/class/fpga/fpga_manager0/firmware".to_string();
        assert_eq!(
            Address::parse_named(&s, None, &regs).map_err(|_| ()),
            Ok((Address::String { base: s, bytes: None }, None, None))
        );
    }
}
