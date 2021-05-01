use pathfinding::prelude::Matrix;
use std::{collections::HashMap, fmt::Debug, hash::Hash};

type PatchScore = usize;

pub trait Patch: Clone {
    fn score(&self) -> PatchScore;
    fn is_equal(&self) -> bool;
    fn is_maximal(&self) -> bool;
    fn unchanged() -> Self;
}

pub trait Diff {
    type Patch: Patch;
    fn diff(&self, new: &Self) -> Self::Patch;
}

#[derive(Clone, Debug)]
pub enum ScalarChanged<T> {
    Diff { old: T, new: T },
    Equal,
}

impl<T> ScalarChanged<T> {
    fn diff(old: T, new: T) -> Self { ScalarChanged::Diff { old, new } }

    fn equal() -> Self { ScalarChanged::Equal }
}

impl<T: Clone> Patch for ScalarChanged<T> {
    fn score(&self) -> PatchScore {
        match self {
            ScalarChanged::Equal => 0,
            ScalarChanged::Diff { .. } => 1,
        }
    }

    fn is_maximal(&self) -> bool { !matches!(self, ScalarChanged::Equal) }

    fn is_equal(&self) -> bool { matches!(self, ScalarChanged::Equal) }

    fn unchanged() -> Self { ScalarChanged::Equal }
}

macro_rules! impl_diff_for_scalar_type {
    (with_slice $ty:ty) => {
        impl_diff_for_scalar_type!($ty);
        impl Diff for &[$ty] {
            type Patch = ScalarChanged<Vec<$ty>>;

            fn diff(&self, new: &Self) -> Self::Patch {
                if self == new {
                    ScalarChanged::equal()
                } else {
                    ScalarChanged::diff(self.to_vec(), new.to_vec())
                }
            }
        }
    };
    ($ty:ty) => {
        impl Diff for $ty {
            type Patch = ScalarChanged<$ty>;

            fn diff(&self, new: &Self) -> Self::Patch {
                if self == new {
                    ScalarChanged::equal()
                } else {
                    ScalarChanged::diff(self.clone(), new.clone())
                }
            }
        }
    };
}

impl_diff_for_scalar_type!(with_slice u8);
impl_diff_for_scalar_type!(with_slice u16);
impl_diff_for_scalar_type!(with_slice u32);
impl_diff_for_scalar_type!(with_slice u64);
impl_diff_for_scalar_type!(with_slice u128);
impl_diff_for_scalar_type!(with_slice usize);

impl_diff_for_scalar_type!(with_slice i8);
impl_diff_for_scalar_type!(with_slice i16);
impl_diff_for_scalar_type!(with_slice i32);
impl_diff_for_scalar_type!(with_slice i64);
impl_diff_for_scalar_type!(with_slice i128);
impl_diff_for_scalar_type!(with_slice isize);

impl_diff_for_scalar_type!(with_slice f32);
impl_diff_for_scalar_type!(with_slice f64);

impl_diff_for_scalar_type!(String);

impl_diff_for_scalar_type!(with_slice bool);
impl_diff_for_scalar_type!(with_slice char);

impl Diff for &str {
    type Patch = ScalarChanged<String>;

    fn diff(&self, new: &Self) -> Self::Patch {
        if self == new {
            ScalarChanged::equal()
        } else {
            ScalarChanged::diff(self.to_string(), new.to_string())
        }
    }
}

macro_rules! impl_diff_for_tuple {
    ($($digits:expr),*) => {
        paste::paste! {
            impl<$([<T $digits>]: Patch),*> Patch for ($([<T $digits>]),*) {
                fn score(&self) -> PatchScore { let mut score = 0; $(score += self.$digits.score();)* score }

                fn is_maximal(&self) -> bool { let mut is_maximal = true; $(is_maximal = is_maximal && self.$digits.is_maximal();)* is_maximal }

                fn is_equal(&self) -> bool { let mut is_equal = true; $(is_equal = is_equal && self.$digits.is_equal();)* is_equal }

                fn unchanged() -> Self { ($([<T $digits>]::unchanged()),*) }
            }


            impl<$([<T $digits>]: Diff),*> Diff for ($([<T $digits>]),*) {
                type Patch = ($([<T $digits>]::Patch),*);

                fn diff(&self, other: &Self) -> Self::Patch {
                    ($(self.$digits.diff(&other.$digits),)*)
                }
            }
        }
    };
}

impl_diff_for_tuple!(0, 1);
impl_diff_for_tuple!(0, 1, 2);
impl_diff_for_tuple!(0, 1, 2, 3);
impl_diff_for_tuple!(0, 1, 2, 3, 4);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14);
impl_diff_for_tuple!(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
struct NeverMaximalDiff<T>(T);

#[derive(Clone, Debug)]
struct NeverMaximalPatch<T>(T);

impl<T: Patch> Patch for NeverMaximalPatch<T> {
    fn score(&self) -> PatchScore { self.0.score() }

    fn is_equal(&self) -> bool { self.0.is_equal() }

    fn is_maximal(&self) -> bool { false }

    fn unchanged() -> Self { Self(T::unchanged()) }
}

impl<T: Diff> Diff for NeverMaximalDiff<T> {
    type Patch = NeverMaximalPatch<T::Patch>;

    fn diff(&self, new: &Self) -> Self::Patch { NeverMaximalPatch(self.0.diff(&new.0)) }
}

#[derive(Clone, Debug)]
pub enum VecElementPatch<T: Diff> {
    MovedOrChanged { old_index: usize, new_index: usize, patch: T::Patch },
    Removed { old_index: usize, value: T },
    Added { new_index: usize, value: T },
}

#[derive(Clone, Debug)]
pub struct VecPatch<T: Diff>
where
    T::Patch: Debug,
{
    patches: Vec<VecElementPatch<T>>,
    old_elements: usize,
    new_elements: usize,
}

impl<T: Diff + Clone> Patch for VecPatch<T>
where
    T::Patch: Debug,
{
    fn score(&self) -> PatchScore {
        let mut score = 0;
        for patch in &self.patches {
            match patch {
                VecElementPatch::MovedOrChanged { old_index, new_index, patch } => {
                    if old_index != new_index {
                        score += 1;
                    }
                    score += patch.score();
                }
                VecElementPatch::Removed { .. } => score += 1,
                VecElementPatch::Added { .. } => score += 1,
            }
        }

        score
    }

    fn is_equal(&self) -> bool { self.patches.len() == 0 }

    fn is_maximal(&self) -> bool {
        let mut old_removed = 0;
        let mut new_added = 0;

        for patch in &self.patches {
            match patch {
                VecElementPatch::MovedOrChanged { patch, .. } => {
                    assert!(!patch.is_maximal());
                    return false
                }
                VecElementPatch::Removed { .. } => old_removed += 1,
                VecElementPatch::Added { .. } => new_added += 1,
            }
        }

        (old_removed == self.old_elements)
            && (new_added == self.new_elements)
            && (!self.patches.len() == 0)
    }

    fn unchanged() -> Self { VecPatch { patches: vec![], old_elements: 0, new_elements: 0 } }
}

impl<T: Diff + Clone + Debug> Diff for Vec<T>
where
    T::Patch: Debug,
{
    type Patch = VecPatch<T>;

    fn diff(&self, new: &Self) -> Self::Patch {
        let old_map = self
            .iter()
            .enumerate()
            .map(|(k, v)| (NeverMaximalDiff(k), v.clone()))
            .collect::<HashMap<_, _>>();

        let new_map = new
            .iter()
            .enumerate()
            .map(|(k, v)| (NeverMaximalDiff(k), v.clone()))
            .collect::<HashMap<_, _>>();

        let patches = old_map
            .diff(&new_map)
            .patches
            .into_iter()
            .map(|v| -> VecElementPatch<T> {
                match v {
                    MapElementPatch::KeyOrValueChanged { key_patch, value_patch } => {
                        let (old_index, new_index) = match key_patch {
                            PatchWithValue::Equal(idx) => (idx.0, idx.0),
                            PatchWithValue::Patch(NeverMaximalPatch(ScalarChanged::Diff {
                                old,
                                new,
                            })) => (old, new),
                            _ => panic!(),
                        };

                        VecElementPatch::MovedOrChanged { old_index, new_index, patch: value_patch }
                    }
                    MapElementPatch::Removed { old_key, value } => {
                        VecElementPatch::Removed { old_index: old_key.0, value }
                    }
                    MapElementPatch::Added { new_key, value } => {
                        VecElementPatch::Added { new_index: new_key.0, value }
                    }
                }
            })
            .collect();

        VecPatch { patches, old_elements: self.len(), new_elements: new.len() }
    }
}

#[derive(Debug, Clone)]
pub enum PatchWithValue<T, P> {
    Equal(T),
    Patch(P),
}

impl<T: Diff + Clone> PatchWithValue<T, T::Patch> {
    fn score(&self) -> PatchScore {
        match self {
            Self::Equal(_) => 0,
            Self::Patch(p) => p.score(),
        }
    }

    // fn is_equal(&self) -> bool { matches!(self, Self::Equal(_)) }

    fn is_maximal(&self) -> bool {
        match self {
            Self::Equal(_) => false,
            Self::Patch(p) => p.is_maximal(),
        }
    }

    fn from_patch(patch: T::Patch, value: T) -> Self {
        if patch.is_equal() {
            Self::Equal(value)
        } else {
            Self::Patch(patch)
        }
    }
}

#[derive(Clone, Debug)]
pub enum MapElementPatch<K: Diff, V: Diff> {
    KeyOrValueChanged { key_patch: PatchWithValue<K, K::Patch>, value_patch: V::Patch },
    Removed { old_key: K, value: V },
    Added { new_key: K, value: V },
}

#[derive(Clone, Debug)]
pub struct MapPatch<K: Diff, V: Diff>
where
    K::Patch: Debug,
    V::Patch: Debug,
{
    patches: Vec<MapElementPatch<K, V>>,
    old_elements: usize,
    new_elements: usize,
}

impl<K: Diff + Clone, V: Diff + Clone> Patch for MapPatch<K, V>
where
    K::Patch: Debug,
    V::Patch: Debug,
{
    fn score(&self) -> PatchScore {
        let mut score = 0;
        for patch in &self.patches {
            match patch {
                MapElementPatch::KeyOrValueChanged { key_patch, value_patch } => {
                    score += key_patch.score() + value_patch.score()
                }
                MapElementPatch::Removed { .. } => score += 1,
                MapElementPatch::Added { .. } => score += 1,
            }
        }

        score
    }

    fn is_equal(&self) -> bool { self.patches.len() == 0 }

    fn is_maximal(&self) -> bool {
        let mut old_removed = 0;
        let mut new_added = 0;

        for patch in &self.patches {
            match patch {
                MapElementPatch::KeyOrValueChanged { key_patch, value_patch } => {
                    assert!(!value_patch.is_maximal() || !key_patch.is_maximal());
                    return false
                }
                MapElementPatch::Removed { .. } => old_removed += 1,
                MapElementPatch::Added { .. } => new_added += 1,
            }
        }

        (old_removed == self.old_elements)
            && (new_added == self.new_elements)
            && (!self.patches.len() == 0)
    }

    fn unchanged() -> Self { MapPatch { patches: vec![], old_elements: 0, new_elements: 0 } }
}

impl<K: Diff + Clone + Hash + Eq, V: Diff + Clone> Diff for HashMap<K, V>
where
    K::Patch: Debug,
    K: Debug,
    V::Patch: Debug,
    V: Debug,
{
    type Patch = MapPatch<K, V>;

    fn diff(&self, new: &Self) -> Self::Patch {
        let num_old = dbg!(self.len());
        let num_new = dbg!(new.len());

        let mut scores =
            Matrix::new(num_old, num_new, (K::Patch::unchanged(), V::Patch::unchanged()));

        // don't rely on iteration order stability (even if we don't modify the HashMap,
        // supposedly go randomizes iteration order every time)
        let old_keys = self.keys().collect::<Vec<_>>();
        let new_keys = new.keys().collect::<Vec<_>>();

        for (i, old_k) in old_keys.iter().enumerate() {
            let old_v = &self[old_k];
            for (j, new_k) in new_keys.iter().enumerate() {
                let new_v = &new[new_k];
                dbg!(old_k);
                dbg!(new_k);
                dbg!(old_v);
                dbg!(new_v);
                scores[&(i, j)] = dbg!((old_k.diff(new_k), old_v.diff(new_v)));
            }
        }

        let mut deleted_list = vec![];
        // if for a old element the diff is maximal for each new element, it was deleted
        for i in 0..num_old {
            'outer_del: loop {
                for j in 0..num_new {
                    if !scores[&(i, j)].is_maximal() {
                        break 'outer_del
                    }
                }

                deleted_list.push(i);
                break 'outer_del
            }
        }

        let mut inserted_list = vec![];
        // if for a new element the diff is maximal for each old element, it was
        // inserted
        for j in 0..num_new {
            'outer_ins: loop {
                for i in 0..num_old {
                    if !scores[&(i, j)].is_maximal() {
                        break 'outer_ins
                    }
                }

                inserted_list.push(j);
                break 'outer_ins
            }
        }

        let mut only_moved_value_list_from = vec![];
        let mut only_moved_value_list_to = vec![];
        for i in 0..num_old {
            'outer_value_moved: loop {
                let mut equal_count = 0;
                let mut equal_idx = 0;

                for j in 0..num_new {
                    if scores[&(i, j)].1.is_equal() {
                        equal_count += 1;
                        equal_idx = j;
                    } else if !scores[&(i, j)].1.is_maximal() {
                        break 'outer_value_moved
                    }
                }

                if equal_count == 1 {
                    let mut equal_count_new = 0;
                    for ii in 0..num_old {
                        if scores[&(ii, equal_idx)].1.is_equal() {
                            equal_count_new += 1;
                        } else if !scores[&(ii, equal_idx)].1.is_maximal() {
                            break 'outer_value_moved
                        }
                    }

                    if equal_count_new == 1 {
                        only_moved_value_list_from.push(i);
                        only_moved_value_list_to.push(equal_idx);
                    }
                }

                break 'outer_value_moved
            }
        }

        let mut only_moved_key_list_from = vec![];
        let mut only_moved_key_list_to = vec![];
        for i in 0..num_old {
            'outer_key_moved: loop {
                let mut equal_count = 0;
                let mut equal_idx = 0;

                for j in 0..num_new {
                    if scores[&(i, j)].0.is_equal() {
                        equal_count += 1;
                        equal_idx = j;
                    } else if !scores[&(i, j)].0.is_maximal() {
                        break 'outer_key_moved
                    }
                }

                if equal_count == 1 {
                    let mut equal_count_new = 0;
                    for ii in 0..num_old {
                        if scores[&(ii, equal_idx)].0.is_equal() {
                            equal_count_new += 1;
                        } else if !scores[&(ii, equal_idx)].0.is_maximal() {
                            break 'outer_key_moved
                        }
                    }

                    if equal_count_new == 1 {
                        if !only_moved_value_list_from.contains(&i)
                            && !only_moved_value_list_to.contains(&equal_idx)
                        {
                            only_moved_key_list_from.push(i);
                            only_moved_key_list_to.push(equal_idx);
                        }
                    }
                }

                break 'outer_key_moved
            }
        }

        let old_indices = (0..num_old)
            .filter(|i| {
                !deleted_list.contains(i)
                    && !only_moved_key_list_from.contains(i)
                    && !only_moved_value_list_from.contains(i)
            })
            .collect::<Vec<_>>();

        let new_indices = (0..num_new)
            .filter(|i| {
                !inserted_list.contains(i)
                    && !only_moved_key_list_to.contains(i)
                    && !only_moved_value_list_to.contains(i)
            })
            .collect::<Vec<_>>();

        let rows_are_old = old_indices.len() <= new_indices.len();
        dbg!(rows_are_old);
        let rows = if rows_are_old { &old_indices } else { &new_indices };
        let columns = if rows_are_old { &new_indices } else { &old_indices };

        let mut scores_for_optimization = Matrix::new(rows.len(), columns.len(), 0isize);

        for i in 0..old_indices.len() {
            for j in 0..new_indices.len() {
                let (row, column) = if rows_are_old { (i, j) } else { (j, i) };
                scores_for_optimization[&(row, column)] =
                    scores[&(old_indices[i], new_indices[j])].score() as isize;
            }
        }

        let pairings =
            dbg!(pathfinding::kuhn_munkres::kuhn_munkres_min(&dbg!(scores_for_optimization)));
        let mut patches = vec![];

        dbg!(&deleted_list);
        dbg!(&inserted_list);
        dbg!(&old_indices);
        dbg!(&new_indices);
        dbg!(&only_moved_key_list_from);
        dbg!(&only_moved_key_list_to);
        dbg!(&only_moved_value_list_from);
        dbg!(&only_moved_value_list_to);
        dbg!(&old_keys);
        dbg!(&new_keys);
        dbg!(&rows);
        dbg!(&columns);

        for i in &deleted_list {
            patches.push(MapElementPatch::Removed {
                old_key: old_keys[*i].clone(),
                value: self[old_keys[*i]].clone(),
            });
        }

        for i in &inserted_list {
            patches.push(MapElementPatch::Added {
                new_key: new_keys[*i].clone(),
                value: new[new_keys[*i]].clone(),
            });
        }

        for (from, to) in only_moved_key_list_from.iter().zip(&only_moved_key_list_to) {
            let key_patch = scores[&(*from, *to)].0.clone();
            let value_patch = scores[&(*from, *to)].1.clone();
            if !key_patch.is_equal() || !value_patch.is_equal() {
                patches.push(MapElementPatch::KeyOrValueChanged {
                    key_patch: PatchWithValue::from_patch(key_patch, old_keys[*from].clone()),
                    value_patch,
                });
            }
        }

        for (from, to) in only_moved_value_list_from.iter().zip(&only_moved_value_list_to) {
            let key_patch = scores[&(*from, *to)].0.clone();
            let value_patch = scores[&(*from, *to)].1.clone();
            if !key_patch.is_equal() || !value_patch.is_equal() {
                patches.push(MapElementPatch::KeyOrValueChanged {
                    key_patch: PatchWithValue::from_patch(key_patch, old_keys[*from].clone()),
                    value_patch,
                });
            }
        }

        for (i, a) in rows.iter().enumerate() {
            let b = &pairings.1[i];

            let (from, to) = dbg!(if rows_are_old { (a, b) } else { (b, a) });
            patches.push(MapElementPatch::KeyOrValueChanged {
                key_patch: PatchWithValue::from_patch(
                    scores[&(*from, new_indices[*to])].0.clone(),
                    old_keys[*from].clone(),
                ),
                value_patch: scores[&(*from, new_indices[*to])].1.clone(),
            })
        }

        if columns.len() > rows.len() {
            for col_index in columns {
                if !pairings.1.contains(col_index) {
                    if rows_are_old {
                        let key = new_keys[*col_index];
                        patches.push(MapElementPatch::Added {
                            new_key: key.clone(),
                            value: new[key].clone(),
                        });
                    } else {
                        let key = old_keys[*col_index];
                        patches.push(MapElementPatch::Removed {
                            old_key: key.clone(),
                            value: self[key].clone(),
                        });
                    }
                }
            }
        }


        MapPatch { patches, old_elements: self.len(), new_elements: new.len() }
    }
}
