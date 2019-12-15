use failure::format_err;

pub trait FromBytes: Sized {
    fn from_bytes(bytes: Vec<u8>) -> fuseable::Result<Self>;
}

macro_rules! impl_frombytes_with_fromstr {
    ($ty:ty) => {
        impl FromBytes for $ty {
            fn from_bytes(bytes: Vec<u8>) -> fuseable::Result<Self> {
                Ok(String::from_utf8(bytes)?.parse()?)
            }
        }
    };
}

macro_rules! impl_frombytes_with_fromstr_trim {
    ($ty:ty) => {
        impl FromBytes for $ty {
            fn from_bytes(bytes: Vec<u8>) -> fuseable::Result<Self> {
                Ok((&String::from_utf8(bytes)?).trim().parse()?)
            }
        }
    };
}

impl_frombytes_with_fromstr!(String);

impl_frombytes_with_fromstr_trim!(f32);
impl_frombytes_with_fromstr_trim!(f64);

impl_frombytes_with_fromstr_trim!(isize);
impl_frombytes_with_fromstr_trim!(i128);
impl_frombytes_with_fromstr_trim!(i64);
impl_frombytes_with_fromstr_trim!(i32);
impl_frombytes_with_fromstr_trim!(i16);
impl_frombytes_with_fromstr_trim!(i8);

impl_frombytes_with_fromstr_trim!(usize);
impl_frombytes_with_fromstr_trim!(u128);
impl_frombytes_with_fromstr_trim!(u64);
impl_frombytes_with_fromstr_trim!(u32);
impl_frombytes_with_fromstr_trim!(u16);
impl_frombytes_with_fromstr_trim!(u8);

impl FromBytes for Vec<u8> {
    fn from_bytes(bytes: Vec<u8>) -> fuseable::Result<Self> { Ok(bytes) }
}

pub trait ToBytes: Sized {
    fn to_bytes(self) -> fuseable::Result<Vec<u8>>;
}

macro_rules! impl_tobytes_with_tostring {
    ($ty:ty) => {
        impl ToBytes for $ty {
            fn to_bytes(self) -> fuseable::Result<Vec<u8>> {
                Ok(self.to_string().as_bytes().to_vec())
            }
        }
    };
}

macro_rules! impl_tobytes_with_tostring_newline {
    ($ty:ty) => {
        impl ToBytes for $ty {
            fn to_bytes(self) -> fuseable::Result<Vec<u8>> {
                let mut string = self.to_string();
                string.push('\n');
                Ok(string.as_bytes().to_vec())
            }
        }
    };
}

impl_tobytes_with_tostring!(&str);

impl_tobytes_with_tostring_newline!(f32);
impl_tobytes_with_tostring_newline!(f64);

impl_tobytes_with_tostring_newline!(isize);
impl_tobytes_with_tostring_newline!(i128);
impl_tobytes_with_tostring_newline!(i64);
impl_tobytes_with_tostring_newline!(i32);
impl_tobytes_with_tostring_newline!(i16);
impl_tobytes_with_tostring_newline!(i8);

impl_tobytes_with_tostring_newline!(usize);
impl_tobytes_with_tostring_newline!(u128);
impl_tobytes_with_tostring_newline!(u64);
impl_tobytes_with_tostring_newline!(u32);
impl_tobytes_with_tostring_newline!(u16);
impl_tobytes_with_tostring_newline!(u8);

impl ToBytes for Vec<u8> {
    fn to_bytes(self) -> fuseable::Result<Vec<u8>> { Ok(self) }
}

impl ToBytes for () {
    fn to_bytes(self) -> fuseable::Result<Vec<u8>> { Ok(vec![]) }
}

impl<'a> ToBytes for rlua::Value<'a> {
    fn to_bytes(self) -> fuseable::Result<Vec<u8>> {
        use rlua::Value;

        match self {
            Value::Nil => ().to_bytes(),
            Value::Integer(int) => int.to_bytes(),
            Value::Number(num) => num.to_bytes(),
            Value::String(string) => Ok(string.as_bytes().to_vec()),
            Value::Table(tbl) => {
                tbl.sequence_values::<u8>().collect::<Result<_, _>>().map_err(|e| e.into())
            }
            _ => Err(format_err!("can't convert lua value {:?} to bytes", self)),
        }
    }
}
