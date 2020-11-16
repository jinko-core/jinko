use std::collections::LinkedList;

#[doc(hidden)]
pub struct Stack<T> {
    data: LinkedList<T>,
}

impl<T> Stack<T> {
    pub fn new() -> Stack<T> {
        Stack {
            data: LinkedList::new(),
        }
    }

    pub fn push(&mut self, elt: T) {
        self.data.push_front(elt)
    }

    pub fn pop(&mut self) -> Option<T> {
        self.data.pop_front()
    }

    pub fn peek(&self) -> Option<&T> {
        self.data.front()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}
