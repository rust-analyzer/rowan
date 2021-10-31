use std::{ptr, mem::MaybeUninit, default::Default};

struct Chunk<T> {
    data: MaybeUninit<T>,
    next: Option<ptr::NonNull<Chunk<T>>>,
}

impl<T> Default for Chunk<T> {
    fn default() -> Self {
        Self { data: MaybeUninit::uninit(), next: None }
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

fn ptr_to_chunk<T>(item: ptr::NonNull<T>) -> ptr::NonNull<Chunk<T>> {
    unsafe {
        let base = MaybeUninit::<Chunk<T>>::uninit();
        let base_ptr = base.as_ptr();
        let data_addr = ptr::addr_of!((*base_ptr).data);
        let data_offset = (data_addr as usize) - (base_ptr as usize);
        ptr::NonNull::new_unchecked(((item.as_ptr() as usize) - data_offset) as *mut Chunk<T>)
    }
}

fn chunk_to_ptr<T>(mut chunk: ptr::NonNull<Chunk<T>>) -> ptr::NonNull<T> {
    unsafe {
        ptr::NonNull::new_unchecked(chunk.as_mut().data.as_mut_ptr())
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

    pub fn allocate(&mut self, item: T) -> Result<ptr::NonNull<T>, &'static str> {
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

        Ok(chunk_to_ptr(result))
    }

    pub fn deallocate(&mut self, mut item: ptr::NonNull<T>) {
        let mut chunk_ptr = ptr_to_chunk(item);

        unsafe {
            ptr::drop_in_place(item.as_mut());
            chunk_ptr.as_mut().next = self.free_chunk;
        }
        self.free_chunk = Some(chunk_ptr);
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
                assert_eq!(allocated[id].as_ref().id, id);
            }
        }

        assert!(!pool.is_empty());

        for it in allocated.iter() {
            pool.deallocate(*it);
        }

        assert!(pool.is_empty());
    }

    #[test]
    fn test_chunk_ptr_conversion() {
        let mut chunk: Chunk<TestStruct> = Chunk::default();
        chunk.data.write(TestStruct { id: 0 });
        let chunk_ptr = unsafe { ptr::NonNull::new_unchecked(&mut chunk) };
        let test_struct = chunk_to_ptr(chunk_ptr);
        unsafe {
            assert_eq!(test_struct.as_ref().id, 0);
            assert_eq!(ptr_to_chunk(test_struct), chunk_ptr);
        }
    }
}
