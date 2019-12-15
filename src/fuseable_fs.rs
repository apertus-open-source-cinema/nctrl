use ::log::error;
use ftable::{DirOrFile, Entry, FTable, Inode};
use fuse::{
    FileAttr, FileType, Filesystem, ReplyAttr, ReplyData, ReplyDirectory, ReplyEntry, ReplyOpen,
    ReplyWrite, Request,
};
use fuseable::{Either, Fuseable};
use libc::{EINVAL, ENOENT};
use std::{
    ffi::OsStr,
    iter::once,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

const TTL: Duration = Duration::from_secs(3600); // 1 hour

pub struct FuseableFS<'a, T> {
    fuseable: &'a mut T,
    ftable: FTable<'a>,
}

impl<'a, T: Fuseable> FuseableFS<'a, T> {
    pub fn new(fuseable: &'a mut T) -> Self {
        let mut ftable = FTable::new();

        if let Either::Left(files) = fuseable.read(&mut std::iter::empty()).unwrap() {
            for file in files.into_iter() {
                ftable.add(
                    Inode::root(),
                    fuseable.is_dir(&mut std::iter::once(&*file)).unwrap(),
                    OsStr::new(Box::leak(file.into_boxed_str())),
                );
            }
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
            kind: if is_dir { FileType::Directory } else { FileType::RegularFile },
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
        if let Some(Entry { ty: DirOrFile::Dir(Inode::NONE), .. }) = &self.ftable.get(Inode(parent))
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

    fn setattr(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _mode: Option<u32>,
        _uid: Option<u32>,
        _gid: Option<u32>,
        _size: Option<u64>,
        _atime: Option<SystemTime>,
        _mtime: Option<SystemTime>,
        _fh: Option<u64>,
        _crtime: Option<SystemTime>,
        _chgtime: Option<SystemTime>,
        _bkuptime: Option<SystemTime>,
        _flags: Option<u32>,
        reply: ReplyAttr,
    ) {
        match self.ftable.get(Inode(ino)) {
            Some(e) => {
                reply.attr(&TTL, &self.attr(ino, e.is_dir()));
            }
            None => {
                reply.error(ENOENT);
            }
        }
    }

    fn open(&mut self, _req: &Request<'_>, _ino: u64, flags: u32, reply: ReplyOpen) {
        // set direct io, as we don't know the filesize befor a read
        let flags = flags | (fuse::consts::FOPEN_DIRECT_IO as u32);
        reply.opened(0, flags);
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
        // TODO(robin): we are using direct io, which means a first
        // read without offset will return the whole file (is this
        // actually true? what about huge files?) and any read with
        // offset should return EOF, there are many ways to encode EOF
        // as far as i can tell (see for example
        // http://fuse.996288.n3.nabble.com/What-to-return-on-EOF-td4516.html)
        if offset != 0 {
            reply.error(0); // ESUCCESS ⇔ success
        } else {
            match &self.ftable.get(Inode(ino)) {
                Some(_entry) => {
                    match self.fuseable.read(
                        &mut self.ftable.iter_path(Inode(ino)).map(|e| e.name.to_str().unwrap()),
                    ) {
                        Ok(Either::Left(_)) => {
                            panic!("we got a mismatch");
                        }
                        Ok(Either::Right(data)) => reply.data(&data),
                        Err(e) => {
                            error!("{:}", e);
                            reply.error(EINVAL)
                        }
                    }
                }
                None => reply.error(ENOENT),
            }
        }
    }

    // could be optimized using the open filehandle and storing the
    // partially used iterator in some kind of cache
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
                    DirOrFile::Dir(Inode::NONE) => {
                        self.add_dir(ino);

                        self.readdir(_req, ino.into(), _fh, offset, reply)
                    }
                    DirOrFile::Dir(_) => {
                        let mut this_dir = self.ftable.get(ino).unwrap().clone();
                        this_dir.name = OsStr::new(".");

                        let (top_ino, mut top_dir) = if ino.is_root() {
                            (ino, this_dir.clone())
                        } else {
                            (this_dir.parent, self.ftable.get(this_dir.parent).unwrap().clone())
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
                                break // buffer full
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

    fn write(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        data: &[u8],
        _flags: u32,
        reply: ReplyWrite,
    ) {
        assert!(
            offset == 0,
            "tried to call write with offset != 0 (it is {}) this is not supported!",
            offset
        );

        match &self.ftable.get(Inode(ino)) {
            Some(_entry) => {
                match self.fuseable                   // TODO(robin): possibly unnecessary copy here vvvvvvvvvvvvv
                    .write(&mut self.ftable.iter_path(Inode(ino)).map(|e| e.name.to_str().unwrap()), data.to_vec())
                {
                    Ok(_) => {
                        reply.written(data.len() as u32);
                    }
                    Err(e) => {
                        error!("{}", e);
                        reply.error(EINVAL)
                    }
                }
            }
            None => reply.error(ENOENT),
        }
    }
}
