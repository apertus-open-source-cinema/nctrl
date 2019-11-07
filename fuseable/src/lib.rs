#![feature(core_intrinsics)]
use failure::{Error, Fail};
use fuse_mt::*;
use itertools::Itertools;
use ::log::{error, log};
use lru::LruCache;
use std::{
    collections::{BTreeMap, HashMap},
    ffi::OsString,
    ops::{Deref, DerefMut},
    os::raw::c_int,
    path::Path,
    result,
    str::FromStr,
    sync::{Arc, Mutex, RwLock},
};
use time::*;

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

pub fn type_name<T>(_: &T) -> &'static str { unsafe { std::intrinsics::type_name::<T>() } }

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
                // let keys = keys.into_iter().map(|k| String::from(k)).collect();
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

/*
impl<'a, VT: Fuseable> Fuseable for HashMap<String, VT> {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool, ()> {
        match path.next() {
            Some(name) => match self.get(&name.to_string()) {
                Some(inner) => inner.is_dir(path),
                None => Err(()),
            },
            None => Ok(true),
        }
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>, ()> {
        match path.next() {
            Some(name) => match self.get(&name.to_string()) {
                Some(inner) => inner.read(path),
                None => Err(()),
            },
            None => {
                let keys: Vec<_> = self.keys().cloned().collect();
                let keys = keys.into_iter().map(|k| String::from(k)).collect();
                Ok(Either::Left(keys))
            }
        }
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<(), ()> {
        match path.next() {
            Some(name) => match self.get_mut(&name.to_string()) {
                Some(inner) => inner.write(path, value),
                None => Err(()),
            },
            None => Err(()),
        }
    }
}
*/

// The idea behind this is, that dir entries are always static, but file
// contents are not
pub struct CachedFuseable {
    is_dir_cache: RwLock<LruCache<u64, bool>>,
    read_dir_cache: RwLock<LruCache<u64, Either<Vec<String>, String>>>,
    fuseable: Box<dyn Fuseable>,
}

impl CachedFuseable {
    pub fn new(fuseable: Box<dyn Fuseable>, cache_size: usize) -> CachedFuseable {
        CachedFuseable {
            is_dir_cache: RwLock::new(LruCache::new(cache_size)),
            read_dir_cache: RwLock::new(LruCache::new(cache_size)),
            fuseable,
        }
    }
}

use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

fn calculate_hash<T: Hash>(t: &mut dyn Iterator<Item = T>) -> u64 {
    let mut s = DefaultHasher::new();
    for i in t {
        i.hash(&mut s);
    }
    s.finish()
}

impl Fuseable for CachedFuseable {
    fn is_dir(&self, path: &mut dyn Iterator<Item = &str>) -> Result<bool> {
        let (mut path, mut path_for_hash) = path.tee();
        let hash = calculate_hash(&mut path_for_hash);

        let mut update_cache = false;
        let is_dir = match self.is_dir_cache.write().unwrap().get(&hash) {
            Some(r) => Ok(*r),
            None => {
                update_cache = true;
                Fuseable::is_dir(self.fuseable.deref(), &mut path)
            }
        };

        if update_cache {
            if let Ok(is_dir) = is_dir {
                self.is_dir_cache.write().unwrap().put(hash, is_dir);
            }
        }

        is_dir

        // Fuseable::is_dir(self.fuseable.deref(), path)
    }

    fn read(&self, path: &mut dyn Iterator<Item = &str>) -> Result<Either<Vec<String>, String>> {
        let (mut path, mut path_for_hash) = path.tee();
        let hash = calculate_hash(&mut path_for_hash);

        // let path_string = path_for_string.collect::<Vec<_>>().concat();

        let mut update_cache = false;
        let read = match self.read_dir_cache.write().unwrap().get(&hash) {
            Some(r) => Ok(r.clone()),
            None => {
                update_cache = true;
                Fuseable::read(self.fuseable.deref(), &mut path)
            }
        };

        if update_cache {
            if let Ok(Either::Left(l)) = &read {
                self.read_dir_cache.write().unwrap().put(hash, Either::Left(l.to_vec()));
            }
        }

        read

        // Fuseable::read(self.fuseable.deref(), path)
    }

    fn write(&mut self, path: &mut dyn Iterator<Item = &str>, value: Vec<u8>) -> Result<()> {
        Fuseable::write(self.fuseable.deref_mut(), path, value)
    }
}

pub struct FuseableWrapper<'a> {
    inner: RwLock<Box<dyn Fuseable + 'a>>,
    /*    getattr_cache: RwLock<LruCache<String, Result<bool, ()>>>,
     *    readdir_cache: RwLock<LruCache<String, Result<Either<Vec<String>, String>, ()>>>, */
}

impl<'a> FuseableWrapper<'a> {
    pub fn new<T: Fuseable + 'a>(f: T) -> FuseableWrapper<'a> {
        FuseableWrapper {
            inner: RwLock::new(Box::new(f)),
            /*
            getattr_cache: RwLock::new(LruCache::new(65535)),
            readdir_cache: RwLock::new(LruCache::new(65535)),
            */
        }
    }
}

impl<'a> FilesystemMT for FuseableWrapper<'a> {
    fn init(&self, _req: RequestInfo) -> ResultEmpty { Ok(()) }

    // fn destroy(&self, _req: RequestInfo) {}

    fn getattr(&self, _req: RequestInfo, path: &Path, fh: Option<u64>) -> ResultEntry {
        //        println!("getattr: {:?}", path);
        if let Some(_fh) = fh {
            //            println!("getattr: unhandled open file {}", fh);
            Err(1)
        } else {
            //            fn read(&self, path: &mut dyn Iterator<Item = String>) ->
            // Result<Either<Vec<String>, String>, ()>;

            Fuseable::is_dir(
                (&*self.inner.read().unwrap()).deref(),
                &mut path.to_string_lossy().split_terminator('/').skip(1),
            )
            .map(|v| {
                (
                    std::time::Duration::from_secs(3600),
                    FileAttr {
                        size: 4096, /* TODO(robin): this is shitty, but needed to convince the
                                     * vfs to actually use the results of a read */
                        blocks: 0,
                        atime: std::time::UNIX_EPOCH,
                        mtime: std::time::UNIX_EPOCH,
                        ctime: std::time::UNIX_EPOCH,
                        crtime: std::time::UNIX_EPOCH,
                        kind: if v { FileType::Directory } else { FileType::RegularFile },
                        perm: 0o777,
                        nlink: 2,
                        uid: 0,
                        gid: 0,
                        rdev: 0,
                        flags: 0,
                    },
                )
            })
            .map_err(|e| {
                error!("{}", e);

                0
            })
        }
    }

    fn opendir(&self, _req: RequestInfo, path: &Path, _flags: u32) -> ResultOpen {
        //        println!("opendir: {:?} (flags = {:#o})", path, _flags);

        match Fuseable::is_dir(
            (&*self.inner.read().unwrap()).deref(),
            &mut path.to_string_lossy().split_terminator('/').skip(1),
        ) {
            Ok(true) => Ok((0, 0)),
            Ok(false) => Err(1),
            Err(e) => {
                error!("{}", e);

                Err(1)
            }
        }
    }

    fn readdir(&self, _req: RequestInfo, path: &Path, _fh: u64) -> ResultReaddir {
        //        println!("readdir: {:?}", path);
        Fuseable::read(
            (&*self.inner.read().unwrap()).deref(),
            &mut path.to_string_lossy().split_terminator('/').skip(1),
        )
        .map(|v| match v {
            Either::Left(fields) => fields
                .iter()
                .map(|f| DirectoryEntry { name: OsString::from(f), kind: FileType::Directory })
                .collect(),
            _ => unimplemented!(),
        })
        .map_err(|e| {
            error!("{}", e);

            1
        })
    }
    fn open(&self, _req: RequestInfo, path: &Path, _flags: u32) -> ResultOpen {
        //        println!("open: {:?} flags={:#x}", path, flags);

        match Fuseable::is_dir(
            (&*self.inner.read().unwrap()).deref(),
            &mut path.to_string_lossy().split_terminator('/').skip(1),
        ) {
            Ok(false) => Ok((0, 0)),
            Ok(true) => Err(1),
            Err(e) => {
                error!("{}", e);
                Err(1)
            }
        }
    }

    fn read(
        &self,
        _req: RequestInfo,
        path: &Path,
        _fh: u64,
        _offset: u64,
        _size: u32,
        result: impl FnOnce(result::Result<&[u8], c_int>),
    ) {
        //        println!("read: {:?} {:#x} @ {:#x}", path, size, offset);

        match Fuseable::read(
            (&*self.inner.read().unwrap()).deref(),
            &mut path.to_string_lossy().split_terminator('/').skip(1),
        ) {
            Ok(Either::Left(_)) => result(Err(1)),
            Ok(Either::Right(s)) => result(Ok(&s.into_bytes())),
            Err(e) => {
                error!("{}", e);
                result(Err(1))
            }
        }
    }

    fn write(
        &self,
        _req: RequestInfo,
        path: &Path,
        _fh: u64,
        _offset: u64,
        data: Vec<u8>,
        _flags: u32,
    ) -> ResultWrite {
        //        println!("write: {:?} {:#x} @ {:#x}", path, data.len(), offset);
        // Ok(data.len() as u32)
        let len = data.len();
        Fuseable::write(
            (&mut *self.inner.write().unwrap()).deref_mut(),
            &mut path.to_string_lossy().split_terminator('/').skip(1),
            data,
        )
        .map_err(|e| {
            error!("{}", e);
            1i32
        })
        .map(|_| len as u32)
    }

    fn truncate(
        &self,
        _req: RequestInfo,
        _path: &Path,
        _fh: Option<u64>,
        _size: u64,
    ) -> ResultEmpty {
        //        println!("truncate: {:?} to {:#x}", path, size);
        Ok(())
    }

    /*

    fn release(&self, _req: RequestInfo, path: &Path, fh: u64, _flags: u32, _lock_owner: u64, _flush: bool) -> ResultEmpty {
        println!("release: {:?}", path);
        Ok(())
    }

    fn flush(&self, _req: RequestInfo, path: &Path, fh: u64, _lock_owner: u64) -> ResultEmpty {
        println!("flush: {:?}", path);
        Ok(())
    }

    fn fsync(&self, _req: RequestInfo, path: &Path, fh: u64, datasync: bool) -> ResultEmpty {
        println!("fsync: {:?}, data={:?}", path, datasync);
        Err(1)
    }

    fn chmod(&self, _req: RequestInfo, path: &Path, fh: Option<u64>, mode: u32) -> ResultEmpty {
        println!("chown: {:?} to {:#o}", path, mode);
        Err(1)
    }

    fn chown(&self, _req: RequestInfo, path: &Path, fh: Option<u64>, uid: Option<u32>, gid: Option<u32>) -> ResultEmpty {
        println!("chmod: {:?} to {}:{}", path, uid.unwrap_or(::std::u32::MAX), gid.unwrap_or(::std::u32::MAX));
        Err(1)
    }

    fn utimens(&self, _req: RequestInfo, path: &Path, fh: Option<u64>, atime: Option<Timespec>, mtime: Option<Timespec>) -> ResultEmpty {
        println!("utimens: {:?}: {:?}, {:?}", path, atime, mtime);
        Err(1)
    }

    fn readlink(&self, _req: RequestInfo, path: &Path) -> ResultData {
        println!("readlink: {:?}", path);
        Err(1)
    }

    fn statfs(&self, _req: RequestInfo, path: &Path) -> ResultStatfs {
        println!("statfs: {:?}", path);
        Err(1)
    }

    fn fsyncdir(&self, _req: RequestInfo, path: &Path, fh: u64, datasync: bool) -> ResultEmpty {
        println!("fsyncdir: {:?} (datasync = {:?})", path, datasync);
        Err(1)
    }

    fn mknod(&self, _req: RequestInfo, parent_path: &Path, name: &OsStr, mode: u32, rdev: u32) -> ResultEntry {
        println!("mknod: {:?}/{:?} (mode={:#o}, rdev={})", parent_path, name, mode, rdev);
        Err(1)
    }

    fn mkdir(&self, _req: RequestInfo, parent_path: &Path, name: &OsStr, mode: u32) -> ResultEntry {
        println!("mkdir {:?}/{:?} (mode={:#o})", parent_path, name, mode);
        Err(1)
    }

    fn unlink(&self, _req: RequestInfo, parent_path: &Path, name: &OsStr) -> ResultEmpty {
        println!("unlink {:?}/{:?}", parent_path, name);
        Err(1)
    }

    fn rmdir(&self, _req: RequestInfo, parent_path: &Path, name: &OsStr) -> ResultEmpty {
        println!("rmdir: {:?}/{:?}", parent_path, name);
        Err(1)
    }

    fn symlink(&self, _req: RequestInfo, parent_path: &Path, name: &OsStr, target: &Path) -> ResultEntry {
        println!("symlink: {:?}/{:?} -> {:?}", parent_path, name, target);
        Err(1)
    }

    fn rename(&self, _req: RequestInfo, parent_path: &Path, name: &OsStr, newparent_path: &Path, newname: &OsStr) -> ResultEmpty {
        println!("rename: {:?}/{:?} -> {:?}/{:?}", parent_path, name, newparent_path, newname);
        Err(1)
    }

    fn link(&self, _req: RequestInfo, path: &Path, newparent: &Path, newname: &OsStr) -> ResultEntry {
        println!("link: {:?} -> {:?}/{:?}", path, newparent, newname);
        Err(1)
    }

    fn create(&self, _req: RequestInfo, parent: &Path, name: &OsStr, mode: u32, flags: u32) -> ResultCreate {
        println!("create: {:?}/{:?} (mode={:#o}, flags={:#x})", parent, name, mode, flags);
        Err(1)
    }
    */

    /*
    fn listxattr(&self, _req: RequestInfo, path: &Path, size: u32) -> ResultXattr {
        println!("listxattr: {:?}", path);
        Err(1)
    }
    */

    /*
    fn getxattr(&self, _req: RequestInfo, path: &Path, name: &OsStr, size: u32) -> ResultXattr {
        println!("getxattr: {:?} {:?} {}", path, name, size);
        Err(1)
    }
    */

    /*
    fn setxattr(&self, _req: RequestInfo, path: &Path, name: &OsStr, value: &[u8], flags: u32, position: u32) -> ResultEmpty {
        println!("setxattr: {:?} {:?} {} bytes, flags = {:#x}, pos = {}", path, name, value.len(), flags, position);
        Err(1)
    }

    fn removexattr(&self, _req: RequestInfo, path: &Path, name: &OsStr) -> ResultEmpty {
        println!("removexattr: {:?} {:?}", path, name);
        Err(1)
    }
    */
}
