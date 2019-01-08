use std::{cell::UnsafeCell, mem};

use parking_lot::{Once, ONCE_INIT};

#[derive(Debug)]
enum State<U, V> {
    Uninit(U),
    Inited(V),
    InProgress,
}

/// A `SwapCell` is a set-once version of `RefCell`, whihc gives you plain `&T`
/// back.
#[derive(Debug)]
pub(crate) struct SwapCell<U, V> {
    once: Once,
    state: UnsafeCell<State<U, V>>,
}

impl<U, V> SwapCell<U, V> {
    pub(crate) fn new(seed: U) -> SwapCell<U, V> {
        SwapCell {
            once: ONCE_INIT,
            state: UnsafeCell::new(State::Uninit(seed)),
        }
    }

    pub(crate) fn get_or_init(&self, f: impl FnOnce(U) -> V) -> &V {
        self.once.call_once(|| {
            let seed = match unsafe { self.replace_state(State::InProgress) } {
                State::Uninit(seed) => seed,
                _ => unreachable!(),
            };
            let value = f(seed);
            match unsafe { self.replace_state(State::Inited(value)) } {
                State::InProgress => (),
                _ => unreachable!(),
            }
        });
        match unsafe { &*self.state.get() } {
            State::Inited(value) => value,
            _ => unreachable!(),
        }
    }

    unsafe fn replace_state(&self, new_state: State<U, V>) -> State<U, V> {
        mem::replace(&mut *self.state.get(), new_state)
    }
}

unsafe impl<U: Sync + Send, V: Sync + Send> Sync for SwapCell<U, V> {}
unsafe impl<U: Send, V: Send> Send for SwapCell<U, V> {}
