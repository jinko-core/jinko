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

    pub fn pop(&mut self) -> Option<T> {
        self.data.pop_front()
    }
}
