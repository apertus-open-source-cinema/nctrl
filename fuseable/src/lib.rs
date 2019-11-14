use failure::{Error, Fail};
use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
    ops::{Deref, DerefMut},
    result,
    str::FromStr,
    sync::{Arc, Mutex},
};

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

#[derive(Debug, Fail)]
pub enum FuseableError {
    #[fail(
        display = "trying to access path {} of type {}, which is not a directory",
        path, typename
    )]
    NotADirectory { typename: String, path: String },
    #[fail(display = "file or directory {} was not found", path)]
    NotFound { path: String },
    #[fail(display = "index out of bounds {}, len {}", idx, len)]
    IndexOutOfBounds { idx: usize, len: usize },
    #[fail(display = "the action {} is not supported for type {}", action, typename)]
    Unsupported { action: String, typename: String },
}

impl FuseableError {
    pub fn not_a_directory<T: AsRef<str>, G: AsRef<str>>(typename: T, path: G) -> Error {
        Error::from(FuseableError::NotADirectory {
            typename: typename.as_ref().to_string(),
            path: path.as_ref().to_string(),
        })
    }

    pub fn not_found<T: AsRef<str>>(path: T) -> Error {
        Error::from(FuseableError::NotFound { path: path.as_ref().to_string() })
    }

    pub fn index_out_of_bounds(idx: usize, len: usize) -> Error {
        Error::from(FuseableError::IndexOutOfBounds { idx, len })
    }

    pub fn unsupported<T: AsRef<str>, G: AsRef<str>>(action: T, typename: G) -> Error {
        Error::from(FuseableError::Unsupported {
            action: action.as_ref().to_string(),
            typename: typename.as_ref().to_string(),
        })
    }
}

pub fn type_name<T>(_: &T) -> &'static str { std::any::type_name::<T>() }

pub trait Fuseable {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool>;
    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>>;
    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()>;
}

macro_rules! impl_fuseable_with_to_string {
    ($t:ident) => {
        impl Fuseable for $t {
            fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
                match path.next() {
                    Some(s) => Err(FuseableError::not_a_directory(stringify!($t), s)),
                    None => Ok(false),
                }
            }

            fn read(
                &self,
                path: &mut dyn Iterator<Item = &str>,
            ) -> Result<Either<Vec<String>, String>> {
                match path.next() {
                    Some(s) => Err(FuseableError::not_a_directory(stringify!($t), s)),
                    None => Ok(Either::Right(self.to_string())),
                }
            }

            fn write(
                &mut self,
                path: &mut dyn Iterator<Item = &str>,
                value: Vec<u8>,
            ) -> Result<()> {
                match path.next() {
                    Some(s) => Err(FuseableError::not_a_directory(stringify!($t), s)),
                    None => {
                        *self = String::from_utf8(value)?.parse()?;
                        Ok(())
                    }
                }
            }
        }
    };
}

impl_fuseable_with_to_string!(String);
impl_fuseable_with_to_string!(bool);
impl_fuseable_with_to_string!(u8);
impl_fuseable_with_to_string!(i8);
impl_fuseable_with_to_string!(u16);
impl_fuseable_with_to_string!(i16);
impl_fuseable_with_to_string!(u32);
impl_fuseable_with_to_string!(i32);
impl_fuseable_with_to_string!(u64);
impl_fuseable_with_to_string!(i64);
impl_fuseable_with_to_string!(f32);
impl_fuseable_with_to_string!(f64);

impl<T: Fuseable> Fuseable for Vec<T> {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        match path.next() {
            Some(idx) => {
                let idx = idx.parse::<usize>()?;

                let v = self
                    .get(idx)
                    .ok_or_else(|| FuseableError::index_out_of_bounds(idx, self.len()))?;

                Fuseable::is_dir(v, path)
            }
            None => Ok(true),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(idx) => {
                let idx = idx.parse::<usize>()?;

                let v = self
                    .get(idx)
                    .ok_or_else(|| FuseableError::index_out_of_bounds(idx, self.len()))?;

                Fuseable::read(v, path)
            }
            None => Ok(Either::Left((0..self.len()).map(|v| v.to_string()).collect())),
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        match path.next() {
            Some(idx) => {
                let idx = idx.parse::<usize>()?;

                let len = self.len();
                let v = self
                    .get_mut(idx)
                    .ok_or_else(|| FuseableError::index_out_of_bounds(idx, len))?;

                Fuseable::write(v, path, value)
            }
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

impl<T: Fuseable> Fuseable for Arc<Mutex<T>> {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        self.lock().unwrap().is_dir(path)
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        self.lock().unwrap().read(path)
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        self.lock().unwrap().write(path, value)
    }
}

impl<T: Fuseable + ?Sized> Fuseable for Box<T> {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        Deref::deref(self).is_dir(path)
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        Deref::deref(self).read(path)
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        DerefMut::deref_mut(self).write(path, value)
    }
}

impl<T: Fuseable> Fuseable for Mutex<T> {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        self.lock().unwrap().is_dir(path)
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        self.lock().unwrap().read(path)
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        self.lock().unwrap().write(path, value)
    }
}

impl<'a> Fuseable for &'a str {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => Ok(false),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(s) => Err(FuseableError::not_a_directory(type_name(&self), s)),
            None => Ok(Either::Right(self.to_string())),
        }
    }

    fn write(&mut self, _path: &mut dyn Iterator<Item = &str>, _value: Vec<u8>) -> Result<()> {
        Err(FuseableError::unsupported("write", type_name(&self)))
    }
}

impl<TY: Fuseable> Fuseable for Option<TY> {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        match self {
            Some(v) => Fuseable::is_dir(v, path),
            None => Ok(false),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        match self {
            Some(v) => Fuseable::read(v, path),
            None => Ok(Either::Right("None".to_string())),
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        match self {
            Some(v) => Fuseable::write(v, path, value),
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

impl<'a, VT: Fuseable> Fuseable for BTreeMap<String, VT> {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        match path.next() {
            Some(name) => match self.get(&name.to_string()) {
                Some(inner) => inner.is_dir(path),
                None => Err(FuseableError::not_found(name)),
            },
            None => Ok(true),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(name) => match self.get(&name.to_string()) {
                Some(inner) => inner.read(path),
                None => Err(FuseableError::not_found(name)),
            },
            None => {
                let keys: Vec<_> = self.keys().cloned().collect();
                Ok(Either::Left(keys))
            }
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        match path.next() {
            Some(name) => match self.get_mut(&name.to_string()) {
                Some(inner) => inner.write(path, value),
                None => Err(FuseableError::not_found(name)),
            },
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

impl<
        'a,
        VT: Fuseable,
        KT: FromStr + ToString + Sync + Send + Eq + Hash + Clone,
        S: std::hash::BuildHasher + Send + Sync,
    > Fuseable for HashMap<KT, VT, S>
where
    <KT as std::str::FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        match path.next() {
            Some(name) => match self.get(&name.parse()?) {
                Some(inner) => inner.is_dir(path),
                None => Err(FuseableError::not_found(name)),
            },
            None => Ok(true),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(name) => match self.get(&name.parse()?) {
                Some(inner) => inner.read(path),
                None => Err(FuseableError::not_found(name)),
            },
            None => {
                let keys: Vec<_> = self.keys().cloned().map(|k| k.to_string()).collect();
                Ok(Either::Left(keys))
            }
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        match path.next() {
            Some(name) => match self.get_mut(&name.parse()?) {
                Some(inner) => inner.write(path, value),
                None => Err(FuseableError::not_found(name)),
            },
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}

#[cfg(feature = "bimap")]
use isomorphism::BiMap;

#[cfg(feature = "bimap")]
impl<'a, VT: Fuseable + Hash + Eq, KT: FromStr + ToString + Sync + Send + Eq + Hash + Clone>
    Fuseable for BiMap<KT, VT>
where
    <KT as std::str::FromStr>::Err: std::error::Error + Send + Sync + 'static,
{
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        match path.next() {
            Some(name) => match self.get_left(&name.parse()?) {
                Some(inner) => inner.is_dir(path),
                None => Err(FuseableError::not_found(name)),
            },
            None => Ok(true),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        match path.next() {
            Some(name) => match self.get_left(&name.parse()?) {
                Some(inner) => inner.read(path),
                None => Err(FuseableError::not_found(name)),
            },
            None => {
                let keys: Vec<_> =
                    self.iter().map(|(k, _v)| k).cloned().map(|k| k.to_string()).collect();
                Ok(Either::Left(keys))
            }
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        match path.next() {
            Some(name) => {
                let parsed_name = name.parse()?;
                let mut right =
                    self.remove_left(&parsed_name).ok_or_else(|| FuseableError::not_found(name))?;
                let ret = Fuseable::write(&mut right, path, value);
                self.insert(parsed_name, right);

                ret
            }
            None => Err(FuseableError::unsupported("write", type_name(&self))),
        }
    }
}
