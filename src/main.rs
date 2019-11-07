use ctrl::{sensor::Camera, serde_util::FILE_OPENER};
use ftable::{DirOrFile, Entry, FTable, Inode};
use fuse::{
    FileAttr, FileType, Filesystem, ReplyAttr, ReplyData, ReplyWrite, ReplyDirectory, ReplyEntry, Request,
};
use fuseable::{Either, Fuseable};
use libc::{ENOENT, EINVAL};
use std::env;
use std::ffi::OsStr;
use std::io::Read;
use std::iter::{empty, once};
use std::path::PathBuf;
use std::time::{Duration, UNIX_EPOCH, SystemTime};
use structopt::StructOpt;
use log::error;

const TTL: Duration = Duration::from_secs(3600); // 1 hour

struct FuseableFS<'a, T> {
    fuseable: T,
    ftable: FTable<'a>,
}

impl<'a, T: Fuseable> FuseableFS<'a, T> {
    fn new(fuseable: T) -> Self {
        let mut ftable = FTable::new();

        match fuseable.read(&mut std::iter::empty()).unwrap() {
            Either::Left(files) => {
                for file in files.into_iter() {
                    ftable.add(
                        Inode::root(),
                        fuseable.is_dir(&mut std::iter::once(&*file)).unwrap(),
                        OsStr::new(Box::leak(file.into_boxed_str())),
                    );
                }
            }
            _ => {}
        }

        FuseableFS { fuseable, ftable }
    }

    fn attr<I: Into<u64>>(&self, ino: I, is_dir: bool) -> FileAttr {
        FileAttr {
            ino: ino.into(),
            size: 4096,
            blocks: 1,
            atime: UNIX_EPOCH,
            mtime: UNIX_EPOCH,
            ctime: UNIX_EPOCH,
            crtime: UNIX_EPOCH,
            kind: if is_dir {
                FileType::Directory
            } else {
                FileType::RegularFile
            },
            perm: 0o777,
            nlink: 2,
            uid: 0,
            gid: 0,
            rdev: 0,
            flags: 0,
        }
    }

    fn add_dir(&mut self, ino: Inode) {
        match self
            .fuseable
            .read(&mut self.ftable.iter_path(ino).map(|e| e.name.to_str().unwrap()))
            .unwrap()
        {
            Either::Left(files) => {
                self.ftable.mark_empty(ino);

                for f in files {
                    self.ftable.add(
                        ino,
                        self.fuseable
                            .is_dir(
                                &mut self
                                    .ftable
                                    .iter_path(ino)
                                    .map(|e| e.name.to_str().unwrap())
                                    .chain(&mut std::iter::once(&*f)),
                            )
                            .unwrap(),
                        OsStr::new(Box::leak(f.into_boxed_str())),
                    );
                }

                self.ftable.optimize_from(ino);
            }
            Either::Right(_) => panic!("we got a mismatch"),
        };
    }
}

impl<'a, T: Fuseable> Filesystem for FuseableFS<'a, T> {
    fn lookup(&mut self, _req: &Request, parent: u64, name: &OsStr, reply: ReplyEntry) {
        if let Some(Entry {
            ty: DirOrFile::Dir(Inode::None),
            ..
        }) = &self.ftable.get(Inode(parent))
        {
            self.add_dir(Inode(parent));
        }

        match self.ftable.lookup_directory_entry(Inode(parent), name) {
            Some((ino, e)) => reply.entry(&TTL, &self.attr(ino, e.is_dir()), 0),
            None => reply.error(ENOENT),
        }
    }

    fn getattr(&mut self, _req: &Request, ino: u64, reply: ReplyAttr) {
        match self.ftable.get(Inode(ino)) {
            Some(e) => {
                reply.attr(&TTL, &self.attr(ino, e.is_dir()));
            }
            None => {
                reply.error(ENOENT);
            }
        }
    }

    fn setattr(&mut self, _req: &Request<'_>, ino: u64, _mode: Option<u32>, _uid: Option<u32>, _gid: Option<u32>, _size: Option<u64>, _atime: Option<SystemTime>, _mtime: Option<SystemTime>, _fh: Option<u64>, _crtime: Option<SystemTime>, _chgtime: Option<SystemTime>, _bkuptime: Option<SystemTime>, _flags: Option<u32>, reply: ReplyAttr) {
        match self.ftable.get(Inode(ino)) {
            Some(e) => {
                reply.attr(&TTL, &self.attr(ino, e.is_dir()));
            }
            None => {
                reply.error(ENOENT);
            }
        }
    }

    fn read(
        &mut self,
        _req: &Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        _size: u32,
        reply: ReplyData,
    ) {

        match &self.ftable.get(Inode(ino)) {
            Some(_entry) => {
                match self
                    .fuseable
                    .read(&mut self.ftable.iter_path(Inode(ino)).map(|e| e.name.to_str().unwrap()))
                {
                    Ok(Either::Left(_)) => {
                        panic!("we got a mismatch");
                    }
                    Ok(Either::Right(data)) => {
                        reply.data(data.as_bytes())
                    }
                    Err(e) => {
                        error!("{}", e);
                        reply.error(EINVAL)
                    }
                }
            }
            None => {
                reply.error(ENOENT)
            }
        }
    }

    fn readdir(
        &mut self,
        _req: &Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: ReplyDirectory,
    ) {
        let ino = Inode(ino);

        match &self.ftable.get(ino) {
            Some(entry) => {
                match entry.ty {
                    DirOrFile::Dir(Inode::None) => {
                        self.add_dir(ino);

                        self.readdir(_req, ino.into(), _fh, offset, reply)
                    }
                    DirOrFile::Dir(_) => {
                        let mut this_dir = self.ftable.get(ino).unwrap().clone();
                        this_dir.name = OsStr::new(".");

                        let (top_ino, mut top_dir) = if ino.is_root() {
                            (ino, this_dir.clone())
                        } else {
                            (
                                this_dir.parent,
                                self.ftable.get(this_dir.parent).unwrap().clone(),
                            )
                        };

                        top_dir.name = OsStr::new("..");

                        for (i, (ino, entry)) in once((ino, &this_dir))
                            .chain(once((top_ino, &top_dir)))
                            .chain(self.ftable.iter_dir(ino).unwrap())
                            .enumerate()
                            .skip(offset as usize)
                        {
                            // i + 1 means the index of the next entry
                            if reply.add(
                                ino.into(),
                                (i + 1) as i64,
                                if entry.is_dir() {
                                    FileType::Directory
                                } else {
                                    FileType::RegularFile
                                },
                                entry.name,
                            ) {
                                break; // buffer full
                            }
                        }
                        reply.ok();
                    }
                    DirOrFile::File => reply.error(ENOENT),
                }
            }
            None => {
                reply.error(ENOENT);
            }
        }
    }

    fn write(&mut self, _req: &Request<'_>, ino: u64, _fh: u64, offset: i64, data: &[u8], _flags: u32, reply: ReplyWrite) {
        assert!(offset == 0, "tried to call write with offset != 0 (it is {}) this is not supported!", offset);

        match &self.ftable.get(Inode(ino)) {
            Some(_entry) => {
                match self.fuseable                   // TODO(robin): possibly unnecessary copy here vvvvvvvvvvvvv
                    .write(&mut self.ftable.iter_path(Inode(ino)).map(|e| e.name.to_str().unwrap()), data.to_vec()) {
                        Ok(_) => {
                            reply.written(data.len() as u32);
                        },
                        Err(e) => {
                            error!("{}", e);
                            reply.error(EINVAL)
                        }
                    }
            }
            None => {
                reply.error(ENOENT)
            }
        }

    }
}

/// Basic daemon for controlling the various components of a camera
#[derive(StructOpt, Debug)]
#[structopt(name = "ctrl")]
struct Opt {
    /// Config file describing the camera components and their functionality
    #[structopt(name = "FILE")]
    file: String,
    /// Set all communication channels to mock mode to prevent them from
    /// actually doing anything
    #[structopt(short = "m", long = "mock")]
    mock: bool,
    /// Mountpoint of the fuse config filesystem
    #[structopt(short = "d", long = "mountpoint", default_value = ".propfs")]
    mountpoint: String,
}

fn main() {
    env_logger::init();

    let opt = Opt::from_args();

    let mut f = FILE_OPENER.open(&opt.file).unwrap();
    FILE_OPENER.set_path(PathBuf::from(opt.file));

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("something went wrong reading the file");

    let mut sensor: Camera = serde_yaml::from_str(&contents).unwrap();
    sensor.mocked(opt.mock);

    let options = ["-o", "rw", "-o", "fsname=propfs", "-o", "auto_unmount"]
        .iter()
        .map(|o| o.as_ref())
        .collect::<Vec<&OsStr>>();

    fuse::mount(FuseableFS::new(sensor), opt.mountpoint, &options).unwrap();
}
