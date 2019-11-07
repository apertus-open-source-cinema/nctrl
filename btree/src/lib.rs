#![cfg_attr(not(feature = "std"), no_std)]

use core::fmt::Debug;

pub trait BTreeProxy<V: PartialOrd, I: BTreeIdx> {
    fn root(&self) -> I;
    fn set_root(&mut self, node: I);

    fn get_left(&self, node: I) -> I;

    // Needs to update the parent idx
    fn set_left(&mut self, node: I, left: I);

    fn get_right(&self, node: I) -> I;

    // Needs to update the parent idx
    fn set_right(&mut self, node: I, right: I);

    fn get_parent(&self, node: I) -> I;

    fn value(&self, node: I) -> V;
}

pub trait BTreeIdx: Copy + PartialEq {
    fn is_none(&self) -> bool;
    fn is_some(&self) -> bool { !self.is_none() }

    fn none() -> Self;
}

pub trait BTree<V, I, DFIter: Iterator<Item = (I, V)>> {
    fn into_df_iter_from(self, idx: I) -> DFIter;
    fn into_df_iter(self) -> DFIter;

    fn first(&self) -> I;
    fn last(&self) -> I;

    fn insert(&mut self, new: I);
    fn search(&self, value: V) -> Option<I>;

    #[cfg(feature = "std")]
    fn dump_dot(&self, filename: &str) -> Result<(), std::io::Error>;

    fn rebalance(&mut self);

    fn max_depth(&self) -> usize;
    fn max_depth_from(&self, node: I) -> usize;
}

enum BTreeDFIterState {
    GoingLeft,
    GoingRightOrUp,
}

pub struct BTreeDFIter<V: PartialOrd, I: BTreeIdx, P: BTreeProxy<V, I>> {
    node: I,
    done_left: bool,
    state: BTreeDFIterState,
    proxy: P,
    _marker: core::marker::PhantomData<V>,
}

impl<V: PartialOrd, I: BTreeIdx, P: BTreeProxy<V, I>> BTreeDFIter<V, I, P> {
    fn new(proxy: P, idx: I) -> Self {
        BTreeDFIter {
            node: idx,
            done_left: false,
            state: BTreeDFIterState::GoingLeft,
            proxy,
            _marker: core::marker::PhantomData,
        }
    }
}
// TODO(robin): use morris traversal instead of parent pointer?
impl<V: PartialOrd, I: BTreeIdx, P: BTreeProxy<V, I>> Iterator for BTreeDFIter<V, I, P> {
    type Item = (I, V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.node.is_none() {
            // tree is empty
            None
        } else {
            match self.state {
                // go as far left as possible
                BTreeDFIterState::GoingLeft => {
                    self.state = BTreeDFIterState::GoingRightOrUp;

                    let mut left = self.proxy.get_left(self.node);

                    if !self.done_left {
                        while left.is_some() {
                            self.node = left;
                            left = self.proxy.get_left(self.node);
                        }

                        self.done_left = true;

                        Some((self.node, self.proxy.value(self.node)))
                    } else {
                        self.next()
                    }
                }
                // first go right, if we can't go up until we are at the parent of the last node we
                // did
                BTreeDFIterState::GoingRightOrUp => {
                    self.state = BTreeDFIterState::GoingLeft;

                    let right = self.proxy.get_right(self.node);

                    if right.is_some() {
                        self.node = right;
                        self.done_left = false;

                        self.next()
                    } else {
                        let mut parent = self.proxy.get_parent(self.node);

                        if parent.is_some() {
                            while parent.is_some() && (self.node != self.proxy.get_left(parent)) {
                                self.node = parent;

                                parent = self.proxy.get_parent(self.node);
                            }

                            self.node = parent;

                            if self.node.is_none() {
                                None
                            } else {
                                Some((self.node, self.proxy.value(self.node)))
                            }
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }
}

impl<V: PartialOrd + Debug, I: BTreeIdx + Debug, P: BTreeProxy<V, I>>
    BTree<V, I, BTreeDFIter<V, I, P>> for P
{
    fn max_depth_from(&self, node: I) -> usize {
        use core::cmp::max;

        if node.is_some() {
            1 + max(
                self.max_depth_from(self.get_left(node)),
                self.max_depth_from(self.get_right(node)),
            )
        } else {
            0
        }
    }

    fn max_depth(&self) -> usize { self.max_depth_from(self.root()) }

    fn into_df_iter_from(self, idx: I) -> BTreeDFIter<V, I, P> { BTreeDFIter::new(self, idx) }

    fn into_df_iter(self) -> BTreeDFIter<V, I, P> {
        let root = self.root();
        self.into_df_iter_from(root)
    }

    fn first(&self) -> I {
        let mut cur = self.root();

        loop {
            let left = self.get_left(cur);

            if left.is_some() {
                cur = left;
            } else {
                return cur
            }
        }
    }

    fn last(&self) -> I {
        let mut cur = self.root();

        loop {
            let right = self.get_left(cur);

            if right.is_some() {
                cur = right;
            } else {
                return cur
            }
        }
    }

    fn insert(&mut self, new: I) {
        let mut node = self.root();
        let new_value = self.value(new);

        if node.is_none() {
            // we have no root yet
            self.set_root(new);
            return
        }

        loop {
            let cur_value = self.value(node);

            if new_value > cur_value {
                let next = self.get_right(node);

                if next.is_none() {
                    self.set_right(node, new);
                    break
                } else {
                    node = next;
                }
            } else if new_value < cur_value {
                let next = self.get_left(node);

                if next.is_none() {
                    self.set_left(node, new);
                    break
                } else {
                    node = next;
                }
            } else {
                panic!(
                    "duplicate item, {:?} is already at {:?} (value {:?})",
                    new_value, node, cur_value
                );
            }
        }
    }

    fn search(&self, search: V) -> Option<I> {
        let value_to_search = search;
        let mut current = self.root();
        let mut next;

        loop {
            let value = self.value(current);

            if value_to_search < value {
                next = self.get_left(current);
            } else if value_to_search > value {
                next = self.get_right(current);
            } else {
                return Some(current)
            }

            if next.is_some() {
                current = next;
            } else {
                return None
            }
        }
    }

    #[cfg(feature = "std")]
    fn dump_dot(&self, filename: &str) -> Result<(), std::io::Error> {
        use std::io::Write;
        let mut f = std::fs::File::create(filename).unwrap();

        writeln!(f, "digraph G {{")?;
        writeln!(f, "rankdir = TB;")?;
        writeln!(f, "subgraph {{")?;

        let mut nodes = vec![self.root()];

        while nodes.len() > 0 {
            let mut new_nodes = Vec::new();

            for node in nodes.clone() {
                let left = self.get_left(node);
                if left.is_some() {
                    new_nodes.push(left);
                    writeln!(f, "{:?} -> {:?}", self.value(node), self.value(left))?;
                }

                let right = self.get_right(node);
                if right.is_some() {
                    new_nodes.push(right);
                    writeln!(f, "{:?} -> {:?}", self.value(node), self.value(right))?;
                }
            }

            write!(f, "{{ rank = same;")?;

            for node in nodes {
                write!(f, " {:?};", self.value(node))?;
            }

            writeln!(f, "}}")?;

            nodes = new_nodes;
        }

        writeln!(f, "}}")?;
        writeln!(f, "}}")?;

        Ok(())
    }

    // Algorithm from
    // Tree Rebalancing in Optimal Time and Space
    // QUENTIN F. STOUT and BEllE L. WARREN

    // TODO(robin): maybe add support for pseudo-root sentinal instead of manually
    // handling root
    fn rebalance(&mut self) {
        let root = self.root();

        let mut vine_tail = I::none();
        let mut remainder = root;
        let mut size = 0;

        while remainder.is_some() {
            if self.get_left(remainder).is_none() {
                vine_tail = remainder;
                remainder = self.get_right(remainder);
                size += 1;
            } else {
                let tmp = self.get_left(remainder);
                self.set_left(remainder, self.get_right(tmp));
                self.set_right(tmp, remainder);
                remainder = tmp;

                if vine_tail.is_none() {
                    self.set_root(tmp);
                } else {
                    self.set_right(vine_tail, tmp);
                }
            }
        }

        let mut compress = |count| {
            if count > 0 {
                let mut child = self.root();
                let mut child_right = self.get_right(child);
                self.set_root(child_right);

                let mut scanner = child_right;
                self.set_right(child, self.get_left(scanner));
                self.set_left(scanner, child);


                for _ in 1..count {
                    child = self.get_right(scanner);
                    child_right = self.get_right(child);
                    self.set_right(scanner, child_right);
                    scanner = child_right;

                    self.set_right(child, self.get_left(scanner));
                    self.set_left(scanner, child);
                }
            }
        };

        let size_plus_one: u64 = size + 1;

        let leaf_count = if (size_plus_one).is_power_of_two() {
            0
        } else {
            (size_plus_one) - (size_plus_one.next_power_of_two() >> 1)
        };

        compress(leaf_count);
        size = size - leaf_count;

        while size > 1 {
            size >>= 1;
            compress(size);
        }
    }
}


#[cfg(feature = "std")]
#[cfg(test)]
mod test {
    use crate::{BTree, BTreeIdx, BTreeProxy};
    use rand::{distributions::Uniform, Rng};
    use std::{ffi::OsStr, fmt::Debug};

    impl BTreeIdx for usize {
        fn is_none(&self) -> bool { *self == usize::max_value() }

        fn none() -> usize { usize::max_value() }
    }

    #[derive(Debug, Clone)]
    struct Node<'a> {
        value: &'a OsStr,
        left: usize,
        right: usize,
        parent: usize,
    }

    #[derive(Debug, Clone)]
    struct Tree<'a> {
        nodes: Vec<Node<'a>>,
    }

    #[derive(Debug)]
    struct Proxy<'a> {
        nodes: &'a mut Tree<'a>,
        root: usize,
    }

    impl<'a> BTreeProxy<&'a OsStr, usize> for Proxy<'a> {
        #[inline]
        fn root(&self) -> usize { self.root }

        #[inline]
        fn set_root(&mut self, node: usize) {
            self.nodes.nodes[node].parent = usize::max_value();
            self.root = node;
        }

        #[inline]
        fn get_left(&self, node: usize) -> usize { self.nodes.nodes[node].left }

        #[inline]
        fn set_left(&mut self, node: usize, left: usize) {
            self.nodes.nodes[node].left = left;

            if left.is_some() {
                self.nodes.nodes[left].parent = node;
            }
        }

        #[inline]
        fn get_right(&self, node: usize) -> usize { self.nodes.nodes[node].right }

        #[inline]
        fn set_right(&mut self, node: usize, right: usize) {
            self.nodes.nodes[node].right = right;

            if right.is_some() {
                self.nodes.nodes[right].parent = node;
            }
        }

        #[inline]
        fn get_parent(&self, node: usize) -> usize { self.nodes.nodes[node].parent }

        #[inline]
        fn value(&self, node: usize) -> &'a OsStr { self.nodes.nodes[node].value }
    }

    fn is_sorted<T: Iterator<Item = I>, I: PartialOrd>(iter: T) -> bool {
        is_sorted_by(iter, |a, b| a.partial_cmp(b))
    }

    fn is_sorted_by<I, T: Iterator<Item = I>, F: FnMut(&I, &I) -> Option<core::cmp::Ordering>>(
        mut iter: T,
        mut compare: F,
    ) -> bool {
        let mut last = match iter.next() {
            Some(e) => e,
            None => return true,
        };

        while let Some(curr) = iter.next() {
            if compare(&last, &curr).map(|o| o == core::cmp::Ordering::Greater).unwrap_or(true) {
                return false
            }
            last = curr;
        }

        true
    }

    fn lb(value: usize) -> usize { ((value + 1).next_power_of_two().trailing_zeros()) as usize }

    #[test]
    fn it_works() {
        let rng = rand::thread_rng();
        let M = 10; // 25
        let N = 1 << M;

        let nodes: Vec<Node> = (0..N)
            .map(|_| Node {
                value: OsStr::new(Box::leak(
                    rng.sample_iter(&Uniform::new(0, 9))
                        .map(|v| format!("{}", v))
                        .take(M)
                        .collect::<String>()
                        .into_boxed_str(),
                )),

                left: usize::max_value(),
                right: usize::max_value(),
                parent: usize::max_value(),
            })
            .collect();

        let mut tree = Tree { nodes };

        {
            let mut proxy = Proxy { nodes: &mut tree, root: 0 };

            let k = M - 10;
            let mut pb = pbr::ProgressBar::new((N >> k) as u64);

            for i in 1..N {
                if (i % (1 << k)) == 0 {
                    pb.inc();
                }

                proxy.insert(i);


                let mut proxy = Proxy { nodes: &mut proxy.nodes.clone(), root: proxy.root };
                let mut actual_values: Vec<(&OsStr, usize)> = proxy.nodes.nodes[..=i]
                    .iter()
                    .enumerate()
                    .map(|(idx, node)| (node.value, idx))
                    .collect();

                actual_values.sort_by_key(|ab| ab.0.clone());
                let mut values = Vec::new();

                let mut _nodes_for_iter = proxy.nodes.clone();
                let proxy_for_iter = Proxy { nodes: &mut _nodes_for_iter, root: proxy.root };
                let iter = proxy_for_iter.into_df_iter();

                for (idx, value) in iter {
                    values.push((value, idx));
                }

                assert_eq!(actual_values, values);

                assert!(is_sorted(values.iter()), "not sorted {:#?}", proxy);

                for (value, idx) in values.iter() {
                    assert!(Some(*idx) == proxy.search(value.clone()));
                }

                proxy.rebalance();

                assert_eq!(proxy.max_depth(), lb(actual_values.len()));
                let mut new_values = Vec::new();

                let mut _nodes_for_iter = proxy.nodes.clone();
                let proxy_for_iter = Proxy { nodes: &mut _nodes_for_iter, root: proxy.root };

                for (idx, value) in proxy_for_iter.into_df_iter() {
                    new_values.push((value, idx));
                }


                assert!(is_sorted(new_values.iter()), "not sorted after rebalance {:#?}", proxy);
                assert_eq!(values, new_values);


                for (value, idx) in new_values {
                    assert!(Some(idx) == proxy.search(value));
                }
            }
        }
    }
}
