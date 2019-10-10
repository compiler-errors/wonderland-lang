pub trait Len {
    fn len(&self) -> usize;
}

impl<T> Len for Vec<T> {
    fn len(&self) -> usize {
        Vec::len(self)
    }
}

impl<T> Len for &Vec<T> {
    fn len(&self) -> usize {
        Vec::len(self)
    }
}

impl<T> Len for [T] {
    fn len(&self) -> usize {
        <[T]>::len(self)
    }
}

impl<T> Len for &[T] {
    fn len(&self) -> usize {
        <[T]>::len(self)
    }
}
