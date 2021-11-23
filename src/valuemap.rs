// SPDX-FileCopyrightText: © 2019 Robin Ole Heinemann <robin.ole.heinemann@gmail.com>
// SPDX-License-Identifier: AGPL-3.0-only

use crate::value::{Bytes, FromValue, ToValue, Value};
use ::log::trace;
use failure::format_err;
use fuseable::Result;
use fuseable_derive::Fuseable;
use isomorphism::BiMap;
use parse_num::{parse_num_padded, ParseError};
use serde_derive::*;
use std::{cmp::Ordering, collections::HashMap, str::FromStr};

// TODO(robin): think about how to change this to use Value

#[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq, Clone)]
pub enum ValueOrAny {
    Value(Vec<u8>),
    Any,
}

impl core::fmt::Display for ValueOrAny {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueOrAny::Value(v) => write!(f, "{}", ToValue::to_value(Bytes(v.to_vec())).unwrap()),
            ValueOrAny::Any => write!(f, "Any"),
        }
    }
}

impl FromStr for ValueOrAny {
    type Err = ParseError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "Any" | "any" => Ok(ValueOrAny::Any),
            _ => {
                trace!("got {}", s);
                let v = parse_num_padded(s).map(|v| ValueOrAny::Value(v.1));
                trace!("parsed {:?}", v);
                v
            }
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Fuseable)]
pub enum ValueMap {
    Keywords(BiMap<ValueOrAny, String>),
    Floating(HashMap<ValueOrAny, f64>),
    Fixed(HashMap<ValueOrAny, u64>),
}

impl ValueMap {
    pub fn lookup(&self, v: Bytes) -> Result<Value> {
        let v = v.0;

        match self {
            ValueMap::Keywords(map) => match map.get_left(&ValueOrAny::Value(v.clone())) {
                Some(v) => v.clone().to_value(),
                None => match map.get_left(&ValueOrAny::Any) {
                    Some(v) => v.clone().to_value(),
                    None => Err(format_err!("could not find {:?} in valuemap {:?}", v, self)),
                },
            },
            ValueMap::Floating(map) => match map.get(&ValueOrAny::Value(v.clone())) {
                Some(v) => v.to_value(),
                None => match map.get(&ValueOrAny::Any) {
                    Some(v) => v.to_value(),
                    None => Err(format_err!("could not find {:?} in valuemap {:?}", v, self)),
                },
            },
            ValueMap::Fixed(map) => match map.get(&ValueOrAny::Value(v.clone())) {
                Some(v) => v.to_value(),
                None => match map.get(&ValueOrAny::Any) {
                    Some(v) => v.to_value(),
                    None => Err(format_err!("could not find {:?} in valuemap {:?}", v, self)),
                },
            },
        }
    }

    pub fn encode(&self, s: Value) -> Result<Vec<u8>> {
        fn find_usable_any_value<F: Fn(&ValueOrAny) -> Option<T>, T>(map_getter: F) -> Vec<u8> {
            let mut potential_value = vec![0u8];
            loop {
                match potential_value.last().unwrap() {
                    255 => {
                        potential_value.push(1);
                        let last_pos = potential_value.len() - 2;
                        potential_value[last_pos] = 0;
                    }
                    _ => {
                        let last_pos = potential_value.len() - 1;
                        potential_value[last_pos] = potential_value.last().unwrap() + 1;
                    }
                }

                if map_getter(&ValueOrAny::Value(potential_value.clone())).is_none() {
                    break
                }
            }

            potential_value
        }

        match self {
            ValueMap::Keywords(map) => {
                let v = map
                    .get_right(&<String as FromValue>::from_value(s.clone())?)
                    .ok_or_else(|| format_err!("could not find {:?} in valuemap {:?}", s, self))?;
                match v {
                    ValueOrAny::Value(v) => Ok(v.clone()),
                    _ => Ok(find_usable_any_value(|v| map.get_left(v))),
                }
            }
            ValueMap::Floating(map) => {
                let wanted_value: f64 = FromValue::from_value(s.clone())?;

                let (v, _) = map
                    .iter()
                    .min_by(|(_, v1), (_, v2)| {
                        if (wanted_value - **v1).abs() < (wanted_value - **v2).abs() {
                            Ordering::Less
                        } else {
                            Ordering::Greater
                        }
                    })
                    .ok_or_else(|| format_err!("could not find {:?} in valuemap {:?}", s, self))?;

                match v {
                    ValueOrAny::Value(v) => Ok(v.clone()),
                    _ => Ok(find_usable_any_value(|v| map.get(v))),
                }
            }
            ValueMap::Fixed(map) => {
                let wanted_value: u64 = FromValue::from_value(s)?;

                let (v, _) = map.iter().find(|(_, v)| **v == wanted_value).ok_or_else(|| {
                    format_err!("could not find {} in valuemap {:?}", wanted_value, self)
                })?;

                match v {
                    ValueOrAny::Value(v) => Ok(v.clone()),
                    _ => Ok(find_usable_any_value(|v| map.get(v))),
                }
            }
        }
    }
}


#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
enum StringOru64Orf64 {
    String(String),
    U64(u64),
    F64(f64),
}

impl StringOru64Orf64 {
    fn to_string(&self) -> String {
        match self {
            StringOru64Orf64::String(s) => s.clone(),
            StringOru64Orf64::U64(i) => i.to_string(),
            StringOru64Orf64::F64(f) => f.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ValueMapNonMatched {
    #[serde(flatten)]
    map: HashMap<String, StringOru64Orf64>,
}

impl ValueMapNonMatched {
    pub fn into_valuemap(self, reg_bytes: u64) -> fuseable::Result<ValueMap> {
        let map: HashMap<String, String> =
            self.map.into_iter().map(|(k, v)| (k, v.to_string())).collect();

        // At first the map is split into it's key's and and it's values
        // then first try if the keys are a number (base 2, 8, 10  or 16), if this is
        // the case the number gets parsed and converted to bytes if the key cannot
        // be parsed as a number the string is converted to bytes A special role has
        // the _ character, it is a valid number and represents any number not yet used
        let (keys, values): (Vec<_>, Vec<_>) = map.into_iter().unzip();

        let keys_as_numbers: std::result::Result<Vec<ValueOrAny>, failure::Error> = keys
            .iter()
            .map(|k| {
                if k.trim() == "_" {
                    Ok(ValueOrAny::Any)
                } else {
                    Ok(ValueOrAny::Value(
                        Value::String(k.to_owned())
                            .string_to_uint()?
                            .byte_representation(Some(reg_bytes as usize))?,
                    ))
                    // parse_num_padded(k).map(|v| ValueOrAny::Value(v.1))
                }
            })
            .collect();

        let converted_keys = if let Ok(keys) = keys_as_numbers {
            keys
        } else {
            keys.into_iter().map(|k| ValueOrAny::Value(String::into_bytes(k))).collect()
        };

        // now to the values
        // first try u64, as they are the most specific (numbers without point)
        // then try f64, if nothing matches use String
        let values_as_int: std::result::Result<Vec<u64>, ()> =
            values.iter().map(|s| s.parse::<u64>().map_err(|_| ())).collect();

        fn build_hashmap<K: std::hash::Hash + std::cmp::Eq, V>(
            keys: Vec<K>,
            values: Vec<V>,
        ) -> HashMap<K, V> {
            keys.into_iter().zip(values.into_iter()).collect()
        }

        if let Ok(converted_values) = values_as_int {
            Ok(ValueMap::Fixed(build_hashmap(converted_keys, converted_values)))
        } else if let Ok(converted_values) =
            values.iter().map(|s| s.parse::<f64>().map_err(|_| ())).collect()
        {
            Ok(ValueMap::Floating(build_hashmap(converted_keys, converted_values)))
        } else {
            Ok(ValueMap::Keywords(converted_keys.into_iter().zip(values.into_iter()).collect()))
        }
    }
}
