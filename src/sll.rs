//! Sorted Linked List

use std::{cell::Cell, cmp::Ordering, ptr};

use crate::utility_types::Delta;

pub(crate) unsafe trait Elem {
    fn prev(&self) -> &Cell<*const Self>;
    fn next(&self) -> &Cell<*const Self>;
    fn key(&self) -> &Cell<u32>;
}

#[cold]
pub(crate) fn init<E: Elem>(head: Option<&Cell<*const E>>, elem: &E) -> Result<(), *const E> {
    elem.prev().set(elem);
    elem.next().set(elem);
    if let Some(head) = head {
        link(head, elem)
    } else {
        Ok(())
    }
}

#[cold]
pub(crate) fn unlink<E: Elem>(head: &Cell<*const E>, elem: &E) {
    debug_assert!(!head.get().is_null(), "invalid linked list head");

    let elem_ptr: *const E = elem;

    let prev = elem.prev().replace(elem_ptr);
    let next = elem.next().replace(elem_ptr);
    unsafe {
        debug_assert_eq!((*prev).next().get(), elem_ptr, "invalid linked list links");
        debug_assert_eq!((*next).prev().get(), elem_ptr, "invalid linked list links");
        (*prev).next().set(next);
        (*next).prev().set(prev);
    }

    if head.get() == elem_ptr {
        head.set(if next == elem_ptr { ptr::null() } else { next })
    }
}

#[cold]
pub(crate) fn link<E: Elem>(head: &Cell<*const E>, elem: &E) -> Result<(), *const E> {
    let elem_ptr: *const E = elem;

    unsafe {
        let old_head = head.get();
        // Case 1: empty head, replace it.
        if old_head.is_null() {
            head.set(elem_ptr);
            return Ok(());
        }

        // Case 2: we are smaller than the head, replace it.
        if elem.key() < (*old_head).key() {
            let prev = (*old_head).prev().replace(elem_ptr);
            (*prev).next().set(elem_ptr);
            elem.next().set(old_head);
            elem.prev().set(prev);
            head.set(elem_ptr);
            return Ok(());
        }

        // Case 3: loop *backward* until we find insertion place. Because of
        // Case 2, we can't loop beyond the head.
        let mut curr = (*old_head).prev().get();
        loop {
            match (*curr).key().cmp(elem.key()) {
                Ordering::Less => {
                    let next = (*curr).next().replace(elem_ptr);
                    (*next).prev().set(elem_ptr);
                    elem.prev().set(curr);
                    elem.next().set(next);
                    return Ok(());
                }
                Ordering::Equal => return Err(curr),
                Ordering::Greater => curr = (*curr).prev().get(),
            }
        }
    }
}

pub(crate) fn adjust<E: Elem>(elem: &E, from: u32, by: Delta<u32>) {
    let elem_ptr: *const E = elem;

    unsafe {
        let mut curr = elem_ptr;
        loop {
            let mut key = (*curr).key().get();
            if key >= from {
                key += by;
                (*curr).key().set(key);
            }
            curr = (*curr).next().get();
            if curr == elem_ptr {
                break;
            }
        }
    }
}
