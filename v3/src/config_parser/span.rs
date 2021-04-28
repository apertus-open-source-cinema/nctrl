use std::ops::{Deref, DerefMut};

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct Spanned<T> {
    start: usize,
    end: usize,
    value: T,
}

impl<T> Spanned<T> {
    pub fn new(start: usize, end: usize, value: T) -> Self { Spanned { start, end, value } }

    pub fn map<TO>(self, mapping_function: impl FnOnce(T) -> TO) -> Spanned<TO> {
        Spanned { start: self.start, end: self.end, value: mapping_function(self.value) }
    }

    pub fn try_map<TO, E>(
        self,
        mapping_function: impl FnOnce(T) -> Result<TO, E>,
    ) -> Result<Spanned<TO>, E> {
        let Spanned { start, end, value } = self;
        mapping_function(value).map(|v| Spanned { start, end, value: v })
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target { &self.value }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target { &mut self.value }
}
