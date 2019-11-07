#![feature(const_fn)]
use std::ffi::OsStr;
use std::fmt::Debug;
use btree::{BTree, BTreeProxy, BTreeIdx, BTreeDFIter};

// TODO(robin):
// - Things to further improve performance:
//   - Use filehandle in opendir / releasedir
//     to store directory iterator for readdir
//     (as we only ever get a small amount of dir entries through in one readdir)
//   - rework path iter to use smallvec? (currently this is O(n^2) where n is the number of directories, so roughly O((log n)^2))
//
// - maybe: make Inode transparent over the type -> more compact for small filesystems
// - make agnostic over the data / string type?

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Inode(pub u64);

impl Debug for Inode {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        if self.is_none() {
            write!(fmt, "None")
        } else if self.is_empty() {
            write!(fmt, "Empty")
        } else if self.is_root() {
            write!(fmt, "Root")
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

impl Inode {
    pub const NONE: Inode = Inode::none();
    pub const EMPTY: Inode = Inode::empty();
    pub const ROOT: Inode = Inode::root();

    pub const fn none() -> Inode {
        Inode(std::u64::MAX)
    }

    pub const fn empty() -> Inode {
        Inode(std::u64::MAX - 1)
    }

    pub const fn root() -> Inode {
        Inode(1)
    }

    pub const fn is_some(self) -> bool {
        (!self.is_none()) & (!self.is_empty())
    }

    pub const fn is_root(self) -> bool {
        self.0 == Inode::ROOT.0
    }

    pub const fn is_none(self) -> bool {
        self.0 == Inode::NONE.0
    }

    pub const fn is_empty(self) -> bool {
        self.0 == Inode::EMPTY.0
    }

    // TODO(robin): this is used only when indexing into a array of
    // entries, maybe add check we aren't None or Empty?
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
    pub parent: Inode, // parent directory
    pub ty: DirOrFile,
    pub name: &'a OsStr,

    // indices for the binary tree representing the files in a directory
    btree_right: Inode,
    btree_left: Inode,
    btree_parent: Inode
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

        // Only root can have Inode::None as parent;
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

pub struct FTableDirIter<'a> {
    proxy_iter: BTreeDFIter<&'a OsStr, Inode, (&'a FTable<'a>, Inode)>,
    table: &'a FTable<'a>,
}


impl<'a> Iterator for FTableDirIter<'a> {
    type Item = (Inode, &'a Entry<'a>);
    
    fn next(&mut self) -> Option<Self::Item> {
        self.proxy_iter.next().map(|(ino, _)| (ino, &self.table.table[ino.as_usize()]))
    }
}

impl<'a: 'b, 'b> BTreeProxy<&'a OsStr, Inode> for (&'b FTable<'a>, Inode) {
    fn root(&self) -> Inode {
        match self.0.table[self.1.as_usize()].ty {
            DirOrFile::Dir(file) => file,
            _ => panic!("tried to use Table.TableBTreeProxy but the root was not a dir")
        }
    }

    fn set_root(&mut self, _node: Inode) {
        panic!("attempted to use immutable proxy for mutation")
    }

    fn get_left(&self, node: Inode) -> Inode {
        self.0.table[node.as_usize()].btree_left
    }

    fn set_left(&mut self, _node: Inode, _left: Inode) {
        panic!("attempted to use immutable proxy for mutation")
    }

    fn get_right(&self, node: Inode) -> Inode {
        self.0.table[node.as_usize()].btree_right
    }

    fn set_right(&mut self, _node: Inode, _right: Inode) {
        panic!("attempted to use immutable proxy for mutation")
    }

    fn get_parent(&self, node: Inode) -> Inode {
        self.0.table[node.as_usize()].btree_parent
    }

    fn value(&self, node: Inode) -> &'a OsStr {
        self.0.table[node.as_usize()].name
    }
}


impl<'a: 'b, 'b> BTreeProxy<&'a OsStr, Inode> for (&'b mut FTable<'a>, Inode) {
    fn root(&self) -> Inode {
        match self.0.table[self.1.as_usize()].ty {
            DirOrFile::Dir(file) => file,
            _ => panic!("tried to use Table.TableBTreeProxy but the root was not a dir")
        }
    }

    fn set_root(&mut self, node: Inode) {
        match self.0.table[self.1.as_usize()].ty {
            DirOrFile::Dir(ref mut root) => {
                *root = node;
            },
            _ => panic!("tried to use Table.TableBTreeProxy but the root was not a dir")
        }

        self.0.table[node.as_usize()].btree_parent = Inode::none();
    }

    fn get_left(&self, node: Inode) -> Inode {
        self.0.table[node.as_usize()].btree_left
    }

    fn set_left(&mut self, node: Inode, left: Inode) {
        self.0.table[node.as_usize()].btree_left = left;

        if BTreeIdx::is_some(&left) {
            self.0.table[left.as_usize()].btree_parent = node;
        }
    }

    fn get_right(&self, node: Inode) -> Inode {
        self.0.table[node.as_usize()].btree_right
    }

    fn set_right(&mut self, node: Inode, right: Inode) {
        self.0.table[node.as_usize()].btree_right = right;

        if BTreeIdx::is_some(&right) {
            self.0.table[right.as_usize()].btree_parent = node;
        }
    }

    fn get_parent(&self, node: Inode) -> Inode {
        self.0.table[node.as_usize()].btree_parent
    }

    fn value(&self, node: Inode) -> &'a OsStr {
        self.0.table[node.as_usize()].name
    }
}

impl BTreeIdx for Inode {
    fn is_none(&self) -> bool {
        Inode::is_none(*self) || Inode::is_empty(*self)
    }

    fn none() -> Self {
        Inode::none()
    }
}

impl<'a> FTable<'a> {
    pub fn new() -> FTable<'a> {
        FTable {
            table: vec![
                Entry {
                    parent: Inode::none(),
                    ty: DirOrFile::Dir(Inode::none()),
                    btree_left: Inode::none(),
                    btree_right: Inode::none(),
                    btree_parent: Inode::none(),
                    name: OsStr::new("") }
                ; 2],
        }
    }

    pub fn get(&self, ino: Inode) -> Option<&Entry<'a>> {
        self.table.get(ino.as_usize())
    }

    pub fn add(&mut self, parent: Inode, is_dir: bool, name: &'a OsStr) -> Inode {
        let ino = Inode(self.table.len() as u64);

        let new_entry =  match self.table[parent.as_usize()].ty {
            DirOrFile::Dir(_) => {
                let new_entry = Entry {
                    parent: parent,
                    name: name,
                    btree_left: Inode::none(),
                    btree_right: Inode::none(),
                    btree_parent: Inode::none(),
                    ty: if is_dir {
                        DirOrFile::Dir(Inode::none())
                    } else {
                        DirOrFile::File
                    }
                };

                new_entry
            },
            DirOrFile::File => panic!("attempted to add child to file")
        };

        self.table.push(new_entry);

        (self, parent).insert(ino);

        ino
    }

    // traverses from root to the Inode
    pub fn iter_path(&'a self, ino: Inode) -> FTablePathIter<'a> {
        FTablePathIter::new(self, ino)
    }

    // traverses through all childs of a directory
    pub fn iter_dir(&'a self, ino: Inode) -> Option<FTableDirIter> {
        match &self.table[ino.as_usize()].ty {
            DirOrFile::Dir(_) => {
                Some(FTableDirIter {
                    proxy_iter: (self, ino).into_df_iter(),
                    table: self
                })
            },
            DirOrFile::File => None
        }
    }

    // get the entry with name in directory parent
    pub fn lookup_directory_entry(&'a self, parent: Inode, name: &'a OsStr) -> Option<(Inode, &Entry<'a>)> {
        (self, parent).search(name).map(|ino| (ino, &self.table[ino.as_usize()]))
    }

    // quickly remove all entries from a directory, does not deallocate!!!
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

    pub fn is_dir(&self, ino: Inode) -> bool {
        match self.table[ino.as_usize()].ty {
            DirOrFile::Dir(_) => true,
            DirOrFile::File => false
        }
    }

    // optimize (balance) the binary trees for each directory
    // best to call this after no more files / dirs will be added
    pub fn optimize(&mut self) {
        self.optimize_from(Inode::root())
    }

    // rebalance everything down from this inode (of course only
    // actually does something if this inode is a directory)
    pub fn optimize_from(&mut self, ino: Inode) {
        match self.table[ino.as_usize()].ty {
            DirOrFile::Dir(_) => {
                let mut proxy: (&mut FTable, Inode) = (self, ino);
                proxy.rebalance();
            }
            _ => ()
        }

        // because rust doesn't quite get what i want
        let dir_inodes: Vec<_> = self.iter_dir(ino).unwrap()
            .filter(|(_, entry)| entry.is_dir())
            .map(|(ino, _)| ino).collect();

        for ino in dir_inodes {
            self.optimize_from(ino);
        }
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
