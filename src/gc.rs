//! The Garbage Collector keeps track of all instances ever created during execution of
//! a jinko program. Its main function is to release the instances which are not
//! referenced anymore.
//! For now, the garbage collector is a simple reference counter.

use crate::ObjectInstance;
use std::collections::HashMap;

struct RefCount(ObjectInstance, usize);

impl RefCount {
    pub fn new(instance: ObjectInstance) -> RefCount {
        // Initialize the reference counter at one
        RefCount(instance, 1)
    }

    pub fn _increment(self) -> RefCount {
        RefCount(self.0, self.1 + 1)
    }

    pub fn _decrement(self) -> RefCount {
        match self.1 {
            0 => unreachable!("Logic error: Trying to decrement RefCount of zero"),
            _ => RefCount(self.0, self.1 - 1),
        }
    }

    pub fn _is_live(&self) -> bool {
        self.1 != 0
    }

    pub fn instance_mut(&mut self) -> &mut ObjectInstance {
        &mut self.0
    }
}

#[derive(Default)]
pub struct Gc {
    instances: HashMap<String, RefCount>,
}

impl<'gc> Gc {
    pub fn new_instance(&'gc mut self, instance: ObjectInstance) -> &'gc mut ObjectInstance {
        // FIXME: Should we really create a copy here?
        let hash_copy = instance.hash();

        // FIXME: Should we check something here?
        self.instances
            .insert(instance.hash(), RefCount::new(instance));

        // We can unwrap safely here since we just inserted said instance
        self.instances.get_mut(&hash_copy).unwrap().instance_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::TypeDec;

    #[test]
    fn t_refcount_new() {
        let i = ObjectInstance::empty();
        let rc = RefCount::new(i);

        assert_eq!(rc.1, 1);
        assert!(rc._is_live())
    }

    #[test]
    fn t_refcount_inc_twice() {
        let i = ObjectInstance::empty();
        let rc = RefCount::new(i);

        let rc = rc._increment()._increment();

        assert_eq!(rc.1, 3);
    }

    #[test]
    fn t_refcount_inc_dec() {
        let i = ObjectInstance::empty();
        let rc = RefCount::new(i);

        let rc = rc._increment()._decrement()._decrement();

        assert_eq!(rc.1, 0);
        assert!(!rc._is_live())
    }

    #[test]
    fn t_gc_add_instance() {
        let mut gc = Gc::default();
        let i = ObjectInstance::empty();
        let hash = i.hash();
        let custom_type = TypeDec::new("CustomType".to_string(), vec![]);

        let i = gc.new_instance(i);

        i.set_ty(Some(custom_type.clone()));

        assert_eq!(gc.instances.get(&hash).unwrap().0.ty(), Some(&custom_type))
    }
}
