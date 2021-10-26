use std::{ptr, mem::MaybeUninit, default::Default};

pub struct Chunk<T> {
    data: MaybeUninit<T>,
    next: Option<ptr::NonNull<Chunk<T>>>,
}

impl<T> Default for Chunk<T> {
    fn default() -> Self {
        Self { data: MaybeUninit::uninit(), next: None }
    }
}

impl<T> AsRef<T> for Chunk<T> {
    fn as_ref(&self) -> &T {
        unsafe { self.data.assume_init_ref() }
    }
}

impl<T> AsMut<T> for Chunk<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { self.data.assume_init_mut() }
    }
}

pub struct Pool<T> {
    data: Box<[Chunk<T>]>,
    free_chunk: Option<ptr::NonNull<Chunk<T>>>,
    capacity: usize,
    num_chunks: usize,
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Self::new_with_capacity(256)
    }
}

impl<T> Pool<T> {
    pub fn new_with_capacity(capacity: usize) -> Self {
        debug_assert!(capacity > 0);
        let mut vec = Vec::with_capacity(capacity);
        for idx in 0..capacity {
            vec.push(Chunk::default());
            if idx > 0 {
                unsafe {
                    vec[idx - 1].next = Some(ptr::NonNull::new_unchecked(&mut vec[idx]));
                }
            }
        }

        Self {
            free_chunk: unsafe { Some(ptr::NonNull::new_unchecked(&mut vec[0])) },
            data: vec.into_boxed_slice(),
            capacity,
            num_chunks: 0,
        }
    }

    pub fn allocate(&mut self, item: T) -> Result<ptr::NonNull<Chunk<T>>, &'static str> {
        if !self.can_allocate() {
            return Err("Pool is empty");
        }

        unsafe {
            self.free_chunk.unwrap().as_mut().data.write(item);
        }

        self.num_chunks += 1;
        let result = self.free_chunk.unwrap();

        unsafe {
            self.free_chunk = self.free_chunk.unwrap().as_ref().next;
        }

        Ok(result)
    }

    pub fn deallocate(&mut self, mut item: ptr::NonNull<Chunk<T>>) {
        unsafe {
            ptr::drop_in_place(item.as_mut().data.as_mut_ptr());
            item.as_mut().next = self.free_chunk;
        }
        self.free_chunk = Some(item);
        self.num_chunks -= 1;
    }

    pub fn is_empty(&self) -> bool {
        self.num_chunks == 0
    }

    pub fn can_allocate(&self) -> bool {
        self.num_chunks < self.capacity
    }
}

mod tests {
    use super::*;

    struct TestStruct {
        id: usize,
    }

    impl Drop for TestStruct {
        fn drop(&mut self) {
            println!("{} dropped", self.id);
        }
    }

    #[test]
    fn test_pool() {
        let mut pool = Pool::default();
        let mut allocated = Vec::new();
        for id in 0..10 {
            allocated.push(pool.allocate(TestStruct { id }).unwrap());
        }

        for id in 0..10 {
            unsafe {
                assert_eq!(allocated[id].as_ref().as_ref().id, id);
            }
        }

        assert!(!pool.is_empty());

        for it in allocated.iter() {
            pool.deallocate(*it);
        }

        assert!(pool.is_empty());
    }
}
