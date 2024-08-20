pub trait HasLen {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl HasLen for &str {
    fn len(&self) -> usize {
        str::len(self)
    }
}

impl<T> HasLen for &[T] {
    fn len(&self) -> usize {
        <[T]>::len(self)
    }
}

impl<T> HasLen for Vec<T> {
    fn len(&self) -> usize {
        Vec::len(self)
    }
}
