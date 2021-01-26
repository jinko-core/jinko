use std::collections::VecDeque;

#[doc(hidden)]
pub struct Queue<T> {
    data: VecDeque<T>,
}

impl<T> Queue<T> {
    pub fn new() -> Queue<T> {
        Queue {
            data: VecDeque::new(),
        }
    }

    pub fn push(&mut self, elt: T) {
        self.data.push_back(elt)
    }
}

impl<T> IntoIterator for Queue<T> {
    type Item = T;
    type IntoIter = std::collections::vec_deque::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.into_iter()
    }
}
