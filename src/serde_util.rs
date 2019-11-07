use lazy_static::lazy_static;
use num::Num;
use serde::{de::Error, Deserialize, Deserializer};
use std::{fs::File, io::Read, iter::FromIterator, path::PathBuf, sync::Mutex};

pub struct FileOpener {
    path: Mutex<Option<PathBuf>>,
}

impl FileOpener {
    pub fn set_path(&self, path: PathBuf) { *self.path.lock().unwrap() = Some(path); }

    pub fn open(&self, filename: &str) -> std::io::Result<File> {
        let path = match *self.path.lock().unwrap() {
            Some(ref path) => path.with_file_name(filename),
            None => PathBuf::from(filename),
        };

        File::open(path)
    }
}

lazy_static! {
    pub static ref FILE_OPENER: FileOpener = FileOpener { path: Mutex::new(None) };
}

pub fn by_path<'de, T, D>(deserializer: D) -> Result<T, D::Error>
where
    for<'a> T: Deserialize<'a>,
    D: Deserializer<'de>,
{
    let path = String::deserialize(deserializer)?;

    let mut f = FILE_OPENER.open(&path).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");

    Ok(serde_yaml::from_str(&contents).unwrap())
}

pub fn by_string_option_num<'de, T, D>(deserializer: D) -> Result<Option<T>, D::Error>
where
    for<'a> T: Deserialize<'a> + Num,
    D: Deserializer<'de>,
    <T as Num>::FromStrRadixErr: std::fmt::Display,
{
    let s = Option::<String>::deserialize(deserializer)?;

    match s {
        None => Ok(None),
        Some(v) => {
            let v: Vec<_> = v.chars().collect();
            let (base, start) = match (v.get(0), v.get(1)) {
                (Some('0'), Some('b')) => (2, 2),
                (Some('0'), Some('o')) => (8, 2),
                (Some('0'), Some('x')) => (16, 2),
                (Some('0'..='9'), _) => (10, 0),
                (..) => panic!("invalid address {:?}", v),
            };

            T::from_str_radix(&String::from_iter(&v[start..]), base)
                .map(Some)
                .map_err(D::Error::custom)
        }
    }
}

/*
fn by_string<'de, T, D>(deserializer: D) -> Result<T, D::Error>
where
    for<'a> T: Deserialize<'a>,
    D: Deserializer<'de>,
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Display
{
    let s = String::deserialize(deserializer)?;

    T::from_str(&s).map_err(D::Error::custom)
}

fn by_string_option<'de, T, D>(deserializer: D) -> Result<Option<T>, D::Error>
where
    for<'a> T: Deserialize<'a>,
    D: Deserializer<'de>,
    T: FromStr,
    <T as FromStr>::Err: std::fmt::Display
{
    let s = Option::<String>::deserialize(deserializer)?;

    match s {
        None => Ok(None),
        Some(v) => T::from_str(&v).map(|t| Some(t)).map_err(D::Error::custom)
    }
}
*/

pub fn bool_false() -> bool { false }

#[allow(dead_code)]
pub fn bool_true() -> bool { true }
