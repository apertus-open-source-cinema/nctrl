#![feature(const_fn)]
use std::ffi::OsStr;
use std::fmt::Debug;

// Things to further improve performance:
// Use filehandle in opendir / releasedir 
// to store directory iterator for readdir
// (as we only ever get a small amount of dir entries through in one readdir)
//
// Use a binary search tree (maybe order index tree??) for the directory entries
// -> log N lookup, O(1) space overhead
//
// optionally: make Inode transparent over the type -> more compact for small filesystems
// make agnostic over the data / string type??  

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Inode(pub u64);

impl Debug for Inode {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.is_none() {
            write!(fmt, "None")
        } else if self.is_empty() {
            write!(fmt, "Empty")
        } else {
            self.0.fmt(fmt)
        }
    }
}

impl Into<u64> for Inode {
    fn into(self) -> u64 {
        self.0 
    }
}

const NONE_INODE: u64 = std::u64::MAX;
const EMPTY_INODE: u64 = std::u64::MAX - 1;

impl Inode {
    pub const None: Inode = Inode::none();
    pub const Empty: Inode = Inode::empty();
    pub const Root: Inode = Inode::root();

    pub const fn none() -> Inode {
        Inode(NONE_INODE)
    }

    pub const fn empty() -> Inode {
        Inode(EMPTY_INODE)
    }

    pub const fn root() -> Inode {
        Inode(1)
    }

    pub const fn is_some(self) -> bool {
        (!self.is_none()) & (!self.is_empty())
    }

    pub const fn is_root(self) -> bool {
        self.0 == Inode::Root.0
    }

    pub const fn is_none(self) -> bool {
        self.0 == Inode::None.0
    }

    pub const fn is_empty(self) -> bool {
        self.0 == Inode::Empty.0
    }

    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub enum DirOrFile {
    Dir(Inode), // Inode of last child
    File, // Inode of prev child in directory
}

#[derive(Debug, Clone)]
pub struct Entry<'a> {
    pub parent: Inode,
    pub ty: DirOrFile,
    pub name: &'a OsStr,
    prev: Inode
}

impl<'a> Entry<'a> {
    pub fn is_dir(&self) -> bool {
        match self.ty {
            DirOrFile::Dir(_) => true,
            DirOrFile::File => false
        }
    }
}

#[derive(Debug)]
pub struct FTable<'a> {
    table: Vec<Entry<'a>>,
}

pub struct FTablePathIter<'a> {
    table: &'a FTable<'a>,
    ino: Inode,
    pos: u64,
    len: u64
}

impl<'a> FTablePathIter<'a> {
    fn new(table: &'a FTable, ino: Inode) -> FTablePathIter<'a> {
        let mut parent = table.table[ino.as_usize()].parent;
        let mut len = 1;

        // Only root can have NONE_INODE as parent
        while parent.is_some() {
            len += 1; 
            parent = table.table[parent.as_usize()].parent
        }

        FTablePathIter {
            table,
            ino,
            len,
            pos: 0,
        }
    }
}

impl<'a> Iterator for FTablePathIter<'a> {
    type Item = &'a Entry<'a>;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < (self.len - 1) {
            Some({
                let mut ino = self.ino;

                for _ in 2..(self.len - self.pos) {
                    ino = self.table.table[ino.as_usize()].parent;
                }

                self.pos += 1;

                &self.table.table[ino.as_usize()]
            })
        } else {
            None
        }
    }
}

enum FTableDirIterState {
    ThisDir,
    UpperDir,
    Entries
}

pub struct FTableDirIter<'a> {
    table: &'a FTable<'a>,
    ino: Inode,
//    state: u8
}


impl<'a> Iterator for FTableDirIter<'a> {
    type Item = (Inode, &'a Entry<'a>);
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.ino.is_some() {
            let entry = &self.table.table[self.ino.as_usize()];

            let old_ino = self.ino;
            self.ino = entry.prev;

            Some((old_ino, entry))
        } else {
            None
        }
    }
}

impl<'a> FTable<'a> {
    pub fn new() -> FTable<'a> {
        FTable {
            table: vec![Entry { parent: Inode::none(), ty: DirOrFile::Dir(Inode::none()), prev: Inode::none(), name: OsStr::new("") }; 2],
        }
    }

    pub fn get(&self, ino: Inode) -> Option<&Entry<'a>> {
        self.table.get(ino.as_usize())
    }

    pub fn add<'b: 'a>(&mut self, parent: Inode, is_dir: bool, name: &'b OsStr) -> Inode {
        let ino = Inode(self.table.len() as u64);

        let new_entry =  match self.table[parent.as_usize()].ty {
            DirOrFile::Dir(ref mut old_child) => {
                let new_entry = Entry {
                    parent: parent,
                    name: name,
                    prev: if old_child.is_empty() { Inode::none() } else { *old_child },
                    ty: if is_dir {
                        DirOrFile::Dir(Inode::none())
                    } else {
                        DirOrFile::File
                    }
                };

                *old_child = ino;

                new_entry
            },
            DirOrFile::File => panic!("attempted to att child to file")
        };

        self.table.push(new_entry);

        ino
    }

    // traverses from root to the Inode
    pub fn iter_path(&'a self, ino: Inode) -> FTablePathIter<'a> {
        FTablePathIter::new(self, ino)
    }

    // traverses through all childs of a directory
    pub fn iter_dir(&'a self, ino: Inode) -> Option<FTableDirIter<'a>> {
        match &self.table[ino.as_usize()].ty {
            DirOrFile::Dir(ino) => {
                Some(FTableDirIter {
                    table: self,
                    ino: *ino
                })
            },
            DirOrFile::File => None
        }
    }

    // get the entry with name in directory parent
    pub fn lookup_directory_entry(&'a self, parent: Inode, name: &OsStr) -> Option<(Inode, &Entry<'a>)> {
        self.iter_dir(parent).and_then(|mut iter| iter.find(|e| e.1.name == name))
    }

    pub fn mark_empty(&mut self, ino: Inode) {
        match self.table[ino.as_usize()].ty {
            DirOrFile::Dir(ref mut child) => {
                if child.is_some() {
                    panic!("marking child with content as empty");
                } else {
                }

                *child = Inode::empty();
            },
            DirOrFile::File => panic!("attempted to mark file as empty")
        };
    }
}

#[cfg(test)]
mod tests {
    use super::{FTable, Inode};
    use std::ffi::OsStr;

    fn s(s: &str) -> &OsStr {
        OsStr::new(s)
    }

    #[test]
    fn it_works() {
        let mut t = FTable::new();

        println!("{:#?}", t);
        let a_ino = t.add(Inode::root(), false, s("a"));
        println!("{:#?}", t);
        let b_ino = t.add(Inode::root(), false, s("b"));
        println!("{:#?}", t);
        let d_ino = t.add(Inode::root(), true, s("d"));
        println!("{:#?}", t);
        let c_ino = t.add(d_ino, true, s("c"));
        println!("{:#?}", t);

        println!("{:#?}", t.lookup_directory_entry(Inode::root(), s("a")));
        println!("{:#?}", t.lookup_directory_entry(Inode::root(), s("b")));
        println!("{:#?}", t.lookup_directory_entry(Inode::root(), s("c")));

        println!("iter_dir {:#?}", t.iter_dir(Inode::root()).unwrap().collect::<Vec<_>>());

        println!("{:#?}", t.iter_path(Inode::root()).collect::<Vec<_>>());
        println!("{:#?}", t.iter_path(a_ino).collect::<Vec<_>>());
        println!("{:#?}", t.iter_path(b_ino).collect::<Vec<_>>());

        println!("{:#?}", t.iter_path(c_ino).collect::<Vec<_>>());
        println!("{:#?}", t.iter_path(d_ino).collect::<Vec<_>>());

        panic!();
    }
}
