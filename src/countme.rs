use core::marker::PhantomData;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Count<T: 'static> {
    marker: PhantomData<fn(T)>,
}

impl<T: 'static> Count<T> {
    #[inline]
    pub(crate) const fn new() -> Self {
        Self { marker: PhantomData }
    }
}
