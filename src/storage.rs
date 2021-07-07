use std::marker::PhantomData;

pub(crate) trait Handle {
    // When an object gives out handles, the value returned by
    // index for those handles must be contiguous, non-repeating
    // and starting at 0
    fn index(&self) -> usize;
}

#[derive(Clone, Debug)]
pub(crate) struct Storage<I: Handle, T> {
    s: Vec<Option<T>>,
    phantom: PhantomData<I>,
}

impl<I: Handle, T> Storage<I, T> {
    pub(crate) fn new() -> Self {
        Self{
            s: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub(crate) fn get(&self, h: &I) -> Option<&T> {
        match self.s.get(h.index()) {
            None => None,
            Some(x) => x.as_ref(),
        }
    }

    pub(crate) fn get_mut(&mut self, h: &I) -> &mut Option<T> {
        if h.index() >= self.s.len() {
            self.s.resize_with(h.index() + 1, || None);
        }

        unsafe {
            self.s.get_unchecked_mut(h.index())
        }
    }

    pub(crate) fn reserve(&mut self, additional: usize) {
        self.s.reserve(additional)
    }
}
