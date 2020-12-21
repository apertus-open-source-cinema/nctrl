use crate::lua_util::FailureCompat;
use failure::{format_err, ResultExt};
use fuseable::Result;
use fuseable_derive::Fuseable;
use rlua::{MetaMethod, UserData, UserDataMethods, Value as LuaValue};
use std::convert::TryInto;

// TODO(robin): how do specific wide integers work with the new system? (0x01 vs
// 0x1) Do we just not allow them (as they currently get lost anyways for
// example going through lua and back)? Or do we find some way to allow them?
//
// I would say we don't allow them, as its quite tricky to get right and also
// adds even more implicit assumptions. So the way to write / read values with
// specific width is then to directly use bytes (as thas is what one means
// anyways).
//
// This still leaves the question, how for example a value specified as integer
// (for convenience) is then converted into bytes: Do we always strip leading
// zeros and say the user has to directly use bytes when they don't want that?
// Or do we add a hint that saves how wide a integer actually is?

impl UserData for Value {
    fn add_methods<'lua, M: UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Mul, |_, this, value: rlua::Value| {
            this.clone().mul(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        });
        methods.add_meta_method(MetaMethod::Div, |_, this, value: rlua::Value| {
            this.clone().div(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        });
        methods.add_meta_method(MetaMethod::Shl, |_, this, value: rlua::Value| {
            this.clone().shl(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        });
        methods.add_meta_method(MetaMethod::BOr, |_, this, value: rlua::Value| {
            this.clone().bor(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        });
        methods.add_meta_method(MetaMethod::BAnd, |_, this, value: rlua::Value| {
            this.clone().band(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        });
        methods.add_meta_method(MetaMethod::Add, |_, this, value: rlua::Value| {
            this.clone().add(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        });
        methods.add_meta_method(MetaMethod::Sub, |_, this, value: rlua::Value| {
            this.clone().sub(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        });
        methods.add_meta_method(MetaMethod::Lt, |_, this, value: rlua::Value| {
            this.clone().lt(value.clone()).map_err(|e| FailureCompat::failure_to_lua(e.into()))
                .and_then(|v| {
                    match v {
                        Value::Boolean(b) => Ok(b),
                        e => Err(FailureCompat::failure_to_lua(format_err!("tried to compare {:?} with {:?} but did not get a bool, got {:?}", this, value, e)))
                    }
                })
        });

        // methods.add_meta_function(MetaMethod::Concat, |_, (lhs, rhs):
        // (rlua::Value, rlua::Value)| {     Ok("riea".to_owned())
        //     // String::from_utf8([&this.clone().display_representation().
        // map_err(|e| FailureCompat::failure_to_lua(e.into()))?[..],
        // value.as_bytes()].concat()).map_err(|e|
        // FailureCompat::failure_to_lua(e.into()))     // this.clone().
        // mul(value).map_err(|e| FailureCompat::failure_to_lua(e.into()))
        // });
    }
}

#[derive(Debug, Fuseable, Clone, PartialEq)]
pub enum Value {
    Boolean(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    String(String),
    Bytes(Vec<u8>),
    // TODO(robin): better solution for arrays?
    IntArray(Vec<i64>),
    UIntArray(Vec<u64>),
    FloatArray(Vec<f64>),
    Nil,
}

impl core::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8(self.clone().display_representation().unwrap()).unwrap())
    }
}

macro_rules! string_to_int {
    ($string:expr, $ty:ty) => {{
        let mut s: &[char] = &$string.trim().chars().collect::<Vec<_>>();

        let negative = match s.get(0) {
            Some('-') => {
                s = &s[1..];
                true
            }
            _ => false,
        };

        let (radix, offset) = match (s.get(0), s.get(1)) {
            (Some('0'), Some('x')) => (16, 2),
            (Some('0'), Some('o')) => (8, 2),
            (Some('0'), Some('b')) => (2, 2),
            (Some('0'..='9'), _) => (10, 0),
            _ => {
                return Err(format_err!(
                    "could not determine radix of {}",
                    s.iter().collect::<String>()
                ))
            }
        };

        let s = if negative {
            format!("-{}", &s[offset..].iter().collect::<String>())
        } else {
            s[offset..].iter().collect()
        };

        Ok(<$ty>::from_str_radix(&s, radix)?)
    }};
}

impl Value {
    // TODO(robin): consider adding as_int, as_float and so on, to transform values
    // into each otheR

    pub fn into<T: FromValue>(self) -> Result<T> { T::from_value(self) }

    pub fn mul<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Int(a * b)),
            (Int(a), UInt(b)) => Ok(Int(a * b as i64)),
            (UInt(a), Int(b)) => Ok(Int(a as i64 * b)),
            (UInt(a), UInt(b)) => Ok(UInt(a * b)),
            (Float(a), UInt(b)) => Ok(Float(a * (b as f64))),
            (Float(a), Int(b)) => Ok(Float(a * (b as f64))),
            (UInt(a), Float(b)) => Ok(Float((a as f64) * b)),
            (Int(a), Float(b)) => Ok(Float((a as f64) * b)),
            (Float(a), Float(b)) => Ok(Float(a * b)),
            _ => Err(format_err!(
                "tried to multiply {:?} with {:?} but don't know how to do that",
                self,
                other
            )),
        }
    }

    pub fn div<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Int(a / b)),
            (Int(a), UInt(b)) => Ok(Int(a / b as i64)),
            (UInt(a), Int(b)) => Ok(Int(a as i64 / b)),
            (UInt(a), UInt(b)) => Ok(UInt(a / b)),
            (Float(a), UInt(b)) => Ok(Float(a / (b as f64))),
            (Float(a), Int(b)) => Ok(Float(a / (b as f64))),
            (UInt(a), Float(b)) => Ok(Float((a as f64) / b)),
            (Int(a), Float(b)) => Ok(Float((a as f64) / b)),
            (Float(a), Float(b)) => Ok(Float(a / b)),
            _ => Err(format_err!(
                "tried to divide {:?} by {:?} but don't know how to do that",
                self,
                other
            )),
        }
    }

    pub fn shl<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Int(a << (b as usize))),
            (Int(a), UInt(b)) => Ok(Int(a << (b as usize))),
            (UInt(a), Int(b)) => Ok(UInt(a << (b as usize))),
            (UInt(a), UInt(b)) => Ok(UInt(a << (b as usize))),
            (Bytes(a), UInt(b)) => Ok(UInt(u64::from_value(self)? << (b as usize))),
            (Bytes(a), Int(b)) => Ok(UInt(u64::from_value(self)? << (b as usize))),
            _ => Err(format_err!(
                "tried to shl {:?} by {:?} but don't know how to do that",
                self,
                other
            )),
        }
    }

    pub fn bor<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Int(a | b)),
            (Int(a), UInt(b)) => Ok(Int(a | (b as i64))),
            (UInt(a), Int(b)) => Ok(Int(a as i64 | b)),
            (UInt(a), UInt(b)) => Ok(UInt(a | b)),
            (Bytes(a), UInt(b)) => Ok(UInt(u64::from_value(self)? | b)),
            (Bytes(a), Int(b)) => Ok(UInt(u64::from_value(self)? | (b as u64))),
            _ => Err(format_err!(
                "tried to bor {:?} and {:?} but don't know how to do that",
                self,
                other
            )),
        }
    }

    pub fn lt<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Boolean(a < b)),
            (Int(a), UInt(b)) => Ok(Boolean(a < (b as i64))),
            (UInt(a), Int(b)) => Ok(Boolean((a as i64) < b)),
            (UInt(a), UInt(b)) => Ok(Boolean(a < b)),
            (Bytes(a), UInt(b)) => Ok(Boolean(u64::from_value(self)? < b)),
            (Bytes(a), Int(b)) => Ok(Boolean((u64::from_value(self)? as i64) < b)),
            (Bytes(a), Float(b)) => Ok(Boolean((u64::from_value(self)? as f64) < b)),
            (Int(a), Float(b)) => Ok(Boolean((a as f64) < b)),
            (UInt(a), Float(b)) => Ok(Boolean((a as f64) < b)),
            (Float(a), Int(b)) => Ok(Boolean(a < (b as f64))),
            (Float(a), UInt(b)) => Ok(Boolean(a < (b as f64))),
            (Float(a), Float(b)) => Ok(Boolean(a < b)),
            _ => Err(format_err!(
                "tried to compare {:?} with {:?} but don't know how to do that",
                self,
                other
            )),
        }
    }

    pub fn add<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Int(a + b)),
            (Int(a), UInt(b)) => Ok(Int(a + (b as i64))),
            (UInt(a), Int(b)) => Ok(Int(a as i64 + b)),
            (UInt(a), UInt(b)) => Ok(UInt(a + b)),
            (Bytes(a), UInt(b)) => Ok(UInt(u64::from_value(self)? + b)),
            (Bytes(a), Int(b)) => Ok(UInt(u64::from_value(self)? + (b as u64))),
            (Bytes(a), Float(b)) => Ok(Float(u64::from_value(self)? as f64 + b)),
            (Float(a), UInt(b)) => Ok(Float(a + (b as f64))),
            (Float(a), Int(b)) => Ok(Float(a + (b as f64))),
            (UInt(a), Float(b)) => Ok(Float((a as f64) + b)),
            (Int(a), Float(b)) => Ok(Float((a as f64) + b)),
            (Float(a), Float(b)) => Ok(Float(a + b)),
            _ => Err(format_err!(
                "tried to and {:?} and {:?} but don't know how to do that",
                self,
                other
            )),
        }
    }

    pub fn sub<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Int(a - b)),
            (Int(a), UInt(b)) => Ok(Int(a - (b as i64))),
            (UInt(a), Int(b)) => Ok(Int(a as i64 - b)),
            (UInt(a), UInt(b)) => Ok(Int(a as i64 - b as i64)),
            (Bytes(a), UInt(b)) => Ok(Int(u64::from_value(self)? as i64 - b as i64)),
            (Bytes(a), Int(b)) => Ok(Int(u64::from_value(self)? as i64 - b)),
            (Float(a), UInt(b)) => Ok(Float(a - (b as f64))),
            (Float(a), Int(b)) => Ok(Float(a - (b as f64))),
            (UInt(a), Float(b)) => Ok(Float((a as f64) - b)),
            (Int(a), Float(b)) => Ok(Float((a as f64) - b)),
            (Float(a), Float(b)) => Ok(Float(a - b)),
            _ => Err(format_err!(
                "tried to sub {:?} from {:?} but don't know how to do that",
                other,
                self,
            )),
        }
    }

    pub fn band<T: ToValue>(self, other: T) -> Result<Value> {
        use Value::*;
        let other = other.to_value()?;

        match (self.clone(), other.clone()) {
            (Int(a), Int(b)) => Ok(Int(a & b)),
            (Int(a), UInt(b)) => Ok(Int(a & (b as i64))),
            (UInt(a), Int(b)) => Ok(Int(a as i64 & b)),
            (UInt(a), UInt(b)) => Ok(UInt(a & b)),
            (Bytes(a), UInt(b)) => Ok(UInt(u64::from_value(self)? & b)),
            (Bytes(a), Int(b)) => Ok(UInt(u64::from_value(self)? & (b as u64))),
            _ => Err(format_err!(
                "tried to band {:?} and {:?} but don't know how to do that",
                self,
                other
            )),
        }
    }

    // If the value is a string, try to convert the value to a int
    pub fn string_to_uint(self) -> Result<Self> {
        match self {
            Value::String(s) => string_to_int!(s, u64).map(Value::UInt),
            other => Ok(other),
        }
    }

    // return the "canonical" (defined by some random thing) way to represent the
    // value as bytes
    // if num_bytes is specified, this tries to convert unsigned integers to
    // num_bytes bytes, if this is not possible, it returns an error
    // also returns an error for negative integers or when converting bytes ot bytes
    pub fn byte_representation(self, num_bytes: Option<usize>) -> Result<Vec<u8>> {
        use Value::*;

        fn pad_or_cut_int_to_num_bytes(mut bytes: Vec<u8>, num_bytes: usize) -> Result<Vec<u8>> {
            while bytes.len() < num_bytes {
                bytes.insert(0, 0);
            }

            while bytes.len() > num_bytes {
                if bytes[0] == 0 {
                    bytes.remove(0);
                } else {
                    return Err(format_err!(
                        "tried to convert {:?} to {} bytes, but it had a non zero prefix",
                        bytes,
                        num_bytes
                    ))
                }
            }

            Ok(bytes)
        }

        match self.clone() {
            Int(int) => {
                let int_bytes = int.to_be_bytes().to_vec();
                match num_bytes {
                    Some(num_bytes) => {
                        if int >= 0 {
                            pad_or_cut_int_to_num_bytes(int_bytes, num_bytes)
                        } else {
                            Err(format_err!(
                                "tried to convert {:?} to {} bytes, but its smaller than zero",
                                self,
                                num_bytes
                            ))
                        }
                    }
                    None => Ok(int_bytes),
                }
            }
            UInt(int) => {
                let int_bytes = int.to_be_bytes().to_vec();
                match num_bytes {
                    Some(num_bytes) => pad_or_cut_int_to_num_bytes(int_bytes, num_bytes),
                    None => Ok(int_bytes),
                }
            }
            Bytes(bytes) => match num_bytes {
                Some(num_bytes) => {
                    if num_bytes == bytes.len() {
                        Ok(bytes)
                    } else {
                        Err(format_err!(
                            "tried to convert {:?} to {} bytes, but this is not possible",
                            self,
                            num_bytes
                        ))
                    }
                }
                None => Ok(bytes),
            },
            _ => Err(format_err!(
                "tried to convert {:?} to byte representation, but don't know how to do that",
                self
            )),
        }
    }

    // return the "canonical" (defined by some random thing) way to display the
    // value to a user
    pub fn display_representation(self) -> Result<Vec<u8>> {
        use Value::*;
        match self {
            Int(int) => Ok(format!("{:#x}", int).as_bytes().to_vec()),
            UInt(int) => Ok(format!("{:#x}", int).as_bytes().to_vec()),
            Float(float) => Ok(format!("{}", float).as_bytes().to_vec()),
            Bytes(bytes) => {
                if bytes.len() == 0 {
                    Ok("".as_bytes().to_vec())
                } else if bytes.len() <= 8 {
                    Ok(("0x".to_string()
                        + &bytes
                            .iter()
                            .map(|v| format!("{:02X}", v).to_string())
                            .collect::<std::string::String>())
                        .as_bytes()
                        .to_vec())
                } else {
                    Ok(format!("{:?}", bytes).as_bytes().to_vec())
                }
            }
            String(s) => Ok(s.as_bytes().to_vec()),
            Nil => Ok(vec![]),
            _ => Err(format_err!(
                "tried to convert {:?} to display representation, but don't know how to do that",
                self
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Bytes(pub Vec<u8>);

pub trait ToValue {
    fn to_value(self) -> Result<Value>;
}

impl<'a> ToValue for LuaValue<'a> {
    fn to_value(self) -> Result<Value> {
        use Value::*;

        match self {
            LuaValue::Integer(int) => Ok(Int(int)),
            LuaValue::UserData(userdata) => Ok(userdata.borrow::<Value>()?.clone()),
            LuaValue::String(s) => Ok(String(s.to_str()?.to_owned())),
            LuaValue::Number(float) => Ok(Float(float)),
            LuaValue::Boolean(b) => Ok(Boolean(b)),
            LuaValue::Nil => Ok(Nil),
            _ => Err(format_err!(
                "tried to convert {:?} to Value, but don't know how to do that",
                self
            )),
        }
    }
}

macro_rules! impl_tovalue_for_int {
    ($ty:ty) => {
        impl ToValue for $ty {
            fn to_value(self) -> Result<Value> { Ok(Value::Int(self as i64)) }
        }
    };
}

impl_tovalue_for_int!(i64);
impl_tovalue_for_int!(i32);
impl_tovalue_for_int!(i16);
impl_tovalue_for_int!(i8);

macro_rules! impl_tovalue_for_uint {
    ($ty:ty) => {
        impl ToValue for $ty {
            fn to_value(self) -> Result<Value> { Ok(Value::UInt(self as u64)) }
        }
    };
}

impl_tovalue_for_uint!(usize);
impl_tovalue_for_uint!(u64);
impl_tovalue_for_uint!(u32);
impl_tovalue_for_uint!(u16);
impl_tovalue_for_uint!(u8);

macro_rules! impl_tovalue_for_float {
    ($ty:ty) => {
        impl ToValue for $ty {
            fn to_value(self) -> Result<Value> { Ok(Value::Float(self as f64)) }
        }
    };
}

impl_tovalue_for_float!(f64);
impl_tovalue_for_float!(f32);

macro_rules! impl_tovalue_for_intarray {
    ($ty:ty) => {
        impl ToValue for Vec<$ty> {
            fn to_value(self) -> Result<Value> {
                let res = self.into_iter().map(|v| v as _).collect();
                Ok(Value::IntArray(res))
            }
        }

        impl ToValue for &[$ty] {
            fn to_value(self) -> Result<Value> {
                let res = self.into_iter().map(|v| v.clone() as _).collect();
                Ok(Value::IntArray(res))
            }
        }
    };
}

impl_tovalue_for_intarray!(i64);
impl_tovalue_for_intarray!(i32);
impl_tovalue_for_intarray!(i16);
impl_tovalue_for_intarray!(i8);

macro_rules! impl_tovalue_for_uintarray {
    ($ty:ty) => {
        impl ToValue for Vec<$ty> {
            fn to_value(self) -> Result<Value> {
                let res = self.into_iter().map(|v| v as _).collect();
                Ok(Value::UIntArray(res))
            }
        }

        impl ToValue for &[$ty] {
            fn to_value(self) -> Result<Value> {
                let res = self.to_vec().into_iter().map(|v| v as _).collect();
                Ok(Value::UIntArray(res))
            }
        }
    };
}

impl_tovalue_for_uintarray!(u64);
impl_tovalue_for_uintarray!(u32);
impl_tovalue_for_uintarray!(u16);
impl_tovalue_for_uintarray!(u8);

macro_rules! impl_tovalue_for_floatarray {
    ($ty:ty) => {
        impl ToValue for Vec<$ty> {
            fn to_value(self) -> Result<Value> {
                let res = self.into_iter().map(|v| v as _).collect();
                Ok(Value::FloatArray(res))
            }
        }

        impl ToValue for &[$ty] {
            fn to_value(self) -> Result<Value> {
                let res = self.to_vec().into_iter().map(|v| v as _).collect();
                Ok(Value::FloatArray(res))
            }
        }
    };
}

impl_tovalue_for_floatarray!(f64);
impl_tovalue_for_floatarray!(f32);

impl ToValue for () {
    fn to_value(self) -> Result<Value> { Ok(Value::Nil) }
}

impl<'a> ToValue for &'a str {
    fn to_value(self) -> Result<Value> { Ok(Value::String(self.to_owned())) }
}

impl ToValue for String {
    fn to_value(self) -> Result<Value> { Ok(Value::String(self)) }
}

impl ToValue for Bytes {
    fn to_value(self) -> Result<Value> { Ok(Value::Bytes(self.0)) }
}

impl ToValue for Value {
    fn to_value(self) -> Result<Value> { Ok(self) }
}

pub trait FromValue: Sized {
    fn from_value(value: Value) -> Result<Self>;
}

macro_rules! value_to_int {
    ($ty:ty) => {
        impl FromValue for $ty {
            fn from_value(value: Value) -> Result<Self> {
                match value.clone() {
                    Value::Bytes(b) => {
                        const MAX_BYTES: usize = std::mem::size_of::<$ty>();
                        if b.len() > MAX_BYTES {
                            Err(format_err!(
                                "tried to cast {:?} to {}, but had more bytes than {} can fix ({})",
                                value,
                                stringify!($ty),
                                stringify!($ty),
                                MAX_BYTES
                            ))
                        } else {
                            // TODO(robin): is this actually correct?
                            let mut v = [0u8; MAX_BYTES];

                            if b.len() > 0 {
                                let sign_extension = if (b[0] & 0x80) > 0 { 0xff } else { 0x0 };

                                let mut write_idx: i64 = (MAX_BYTES - 1) as i64;
                                let mut read_idx: i64 = (b.len() - 1) as i64;

                                while read_idx >= 0 {
                                    v[write_idx as usize] = b[read_idx as usize];

                                    read_idx -= 1;
                                    write_idx -= 1;
                                }

                                while write_idx >= 0 {
                                    v[write_idx as usize] = sign_extension;

                                    write_idx -= 1
                                }

                                let out: $ty = unsafe { std::mem::transmute(v) };

                                Ok(<$ty>::from_be(out))
                            } else {
                                Ok(0)
                            }
                        }
                    }
                    Value::Int(i) => Ok(i.try_into()?),
                    Value::Float(f) => Ok(f as $ty),
                    Value::String(s) => string_to_int!(s, $ty),
                    _ => Err(format_err!(
                        "tried to cast {:?} to {}, but don't know how to do that",
                        value,
                        stringify!($ty)
                    )),
                }
            }
        }
    };
}

value_to_int!(i64);
value_to_int!(i32);
value_to_int!(i16);
value_to_int!(i8);

macro_rules! value_to_uint {
    ($ty:ty) => {
        impl FromValue for $ty {
            fn from_value(value: Value) -> Result<Self> {
                match value.clone() {
                    Value::Bytes(b) => {
                        const MAX_BYTES: usize = std::mem::size_of::<$ty>();
                        if b.len() > MAX_BYTES {
                            Err(format_err!(
                                "tried to cast {:?} to {}, but had more bytes than {} can fix ({})",
                                value,
                                stringify!($ty),
                                stringify!($ty),
                                MAX_BYTES
                            ))
                        } else {
                            // TODO(robin): is this actually correct?
                            let mut v = [0u8; MAX_BYTES];

                            if b.len() > 0 {
                                let mut write_idx: i64 = (MAX_BYTES - 1) as i64;
                                let mut read_idx: i64 = (b.len() - 1) as i64;

                                while read_idx >= 0 {
                                    v[write_idx as usize] = b[read_idx as usize];

                                    read_idx -= 1;
                                    write_idx -= 1;
                                }

                                let out: $ty = unsafe { std::mem::transmute(v) };

                                Ok(<$ty>::from_be(out))
                            } else {
                                Ok(0)
                            }
                        }
                    }
                    Value::UInt(i) => Ok(i.try_into()?),
                    Value::Int(i) => Ok(i.try_into()?),
                    Value::Float(f) => Ok(f as $ty),
                    Value::String(s) => string_to_int!(s, $ty),
                    _ => Err(format_err!(
                        "tried to cast {:?} to {}, but don't know how to do that",
                        value,
                        stringify!($ty)
                    )),
                }
            }
        }
    };
}

value_to_uint!(usize);
value_to_uint!(u64);
value_to_uint!(u32);
value_to_uint!(u16);
value_to_uint!(u8);

macro_rules! value_to_float {
    ($ty:ty) => {
        impl FromValue for $ty {
            fn from_value(value: Value) -> Result<Self> {
                match value {
                    Value::Int(i) => Ok(i as _), /* TODO(robin): check wether the integer has a */
                    // exact representation?
                    Value::UInt(i) => Ok(i as _),
                    Value::Float(i) => Ok(i as _),
                    Value::String(s) => Ok(s.trim().parse()?),
                    _ => Err(format_err!(
                        "tried to cast {:?} to {}, but don't know how to do that",
                        value,
                        stringify!($ty)
                    )),
                }
            }
        }
    };
}

value_to_float!(f64);
value_to_float!(f32);

macro_rules! value_to_uintarray {
    ($ty:ty) => {
        impl FromValue for Vec<$ty> {
            fn from_value(value: Value) -> Result<Self> {
                match value {
                    Value::UIntArray(a) => Ok(a
                        .into_iter()
                        .map(|v| v.try_into())
                        .collect::<std::result::Result<_, _>>()?),
                    _ => Err(format_err!(
                        "tried to cast {:?} to {}, but don't know how to do that",
                        value,
                        stringify!(Vec<$ty>)
                    )),
                }
            }
        }
    };
}

value_to_uintarray!(u64);
value_to_uintarray!(u32);
value_to_uintarray!(u16);
value_to_uintarray!(u8);

macro_rules! value_to_intarray {
    ($ty:ty) => {
        impl FromValue for Vec<$ty> {
            fn from_value(value: Value) -> Result<Self> {
                match value {
                    Value::IntArray(a) => Ok(a
                        .into_iter()
                        .map(|v| v.try_into())
                        .collect::<std::result::Result<_, _>>()?),
                    _ => Err(format_err!(
                        "tried to cast {:?} to {}, but don't know how to do that",
                        value,
                        stringify!(Vec<$ty>)
                    )),
                }
            }
        }
    };
}

value_to_intarray!(i64);
value_to_intarray!(i32);
value_to_intarray!(i16);
value_to_intarray!(i8);

macro_rules! value_to_floatarray {
    ($ty:ty) => {
        impl FromValue for Vec<$ty> {
            fn from_value(value: Value) -> Result<Self> {
                match value {
                    Value::FloatArray(a) => Ok(a.into_iter().map(|v| v as _).collect()),
                    _ => Err(format_err!(
                        "tried to cast {:?} to {}, but don't know how to do that",
                        value,
                        stringify!(Vec<$ty>)
                    )),
                }
            }
        }
    };
}

value_to_floatarray!(f64);
value_to_floatarray!(f32);

impl FromValue for () {
    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::Nil => Ok(()),
            _ => Err(format_err!(
                "tried to cast {:?} to {}, but don't know how to do that",
                value,
                stringify!(())
            )),
        }
    }
}

impl FromValue for String {
    fn from_value(value: Value) -> Result<Self> {
        let value_for_error = value.clone();

        match value {
            Value::Int(i) => Ok(i.to_string()),
            Value::UInt(i) => Ok(i.to_string()),
            Value::Float(i) => Ok(i.to_string()),
            Value::String(s) => Ok(s),
            Value::Bytes(b) => String::from_utf8(b)
                .with_context(|e| {
                    format!(
                        "error while casting {:?} to {}: {}",
                        value_for_error,
                        stringify!(String),
                        e
                    )
                })
                .map_err(|e| e.into()),
            _ => Err(format_err!(
                "tried to cast {:?} to {}, but don't know how to do that",
                value,
                stringify!(String)
            )),
        }
    }
}

impl FromValue for Bytes {
    fn from_value(value: Value) -> Result<Self> {
        match value {
            Value::Bytes(bytes) => Ok(Bytes(bytes)),
            _ => Err(format_err!(
                "tried to cast {:?} to {}, but don't know how to do that",
                value,
                stringify!(Bytes)
            )),
        }
    }
}

impl FromValue for Value {
    fn from_value(value: Value) -> Result<Self> { Ok(value) }
}

/*
impl ToValue for LuaValue<'_> {
    fn to_value(self) -> Result<Value> {
        match self {
            LuaValue::Integer(i) => Ok(Value::Int(i)),
            LuaValue::Number(n) => Ok(Value::Float(n)),
            _ => Err(format_err!("tried to create Value from LuaValue {:?}, but don't known how to do that", self))
        }
    }
}

impl FromValue for LuaValue {
    fn from_value(value: Value) -> Result<Self> {

    }
}

pub enum Value<'lua> {
    Nil,
    Boolean(bool),
    LightUserData(LightUserData),
    Integer(Integer),
    Number(Number),
    String(String<'lua>),
    Table(Table<'lua>),
    Function(Function<'lua>),
    Thread(Thread<'lua>),
    UserData(AnyUserData<'lua>),
    Error(Error),
}
*/
