use crate::arc::HeaderSlice;
use std::{
    alloc::{self, Layout},
    default::Default,
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ptr,
};

use memoffset::offset_of;

struct Chunk<T> {
    data: MaybeUninit<T>,
    next: Option<ptr::NonNull<Chunk<T>>>,
    page: PagePtr<T>,
}

#[inline]
fn ptr_to_chunk<T>(item: ptr::NonNull<T>) -> ptr::NonNull<Chunk<T>> {
    let data_offset = offset_of!(Chunk<T>, data);
    unsafe {
        ptr::NonNull::new_unchecked(((item.as_ptr() as usize) - data_offset) as *mut Chunk<T>)
    }
}

#[inline]
fn chunk_to_ptr<T>(mut chunk: ptr::NonNull<Chunk<T>>) -> ptr::NonNull<T> {
    unsafe { ptr::NonNull::new_unchecked(chunk.as_mut().data.as_mut_ptr()) }
}

impl<T> Default for Chunk<T> {
    fn default() -> Self {
        Self {
            data: MaybeUninit::uninit(),
            next: None,
            page: PagePtr { ptr: ptr::NonNull::dangling() },
        }
    }
}

struct PageHeader<T> {
    prev_page: Option<PagePtr<T>>,
    next_page: Option<PagePtr<T>>,
    free_chunk: Option<ptr::NonNull<Chunk<T>>>,
    num_chunks: usize,
    _p: PhantomData<T>,
}

type Page<T> = HeaderSlice<PageHeader<T>, [Chunk<T>; 0]>;

struct PagePtr<T> {
    ptr: ptr::NonNull<Page<T>>,
}

impl<T> Clone for PagePtr<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr.clone() }
    }
}

impl<T> Copy for PagePtr<T> {}

impl<T> PagePtr<T> {
    #[inline]
    fn next_page(&self) -> Option<Self> {
        unsafe { self.ptr.as_ref().header.next_page }
    }

    #[inline]
    fn prev_page(&self) -> Option<Self> {
        unsafe { self.ptr.as_ref().header.prev_page }
    }

    #[inline]
    fn set_next_page(&mut self, next: Option<Self>) {
        unsafe {
            self.ptr.as_mut().header.next_page = next;
        }
    }

    #[inline]
    fn set_prev_page(&mut self, prev: Option<Self>) {
        unsafe {
            self.ptr.as_mut().header.prev_page = prev;
        }
    }

    #[inline]
    fn link_next_page(&mut self, next: Option<Self>) {
        self.set_next_page(next);
        if let Some(mut ptr) = next {
            ptr.set_prev_page(Some(self.clone()));
        }
    }

    #[inline]
    fn link_prev_page(&mut self, prev: Option<Self>) {
        self.set_prev_page(prev);
        if let Some(mut ptr) = prev {
            ptr.set_next_page(Some(self.clone()));
        }
    }

    fn num_chunks(&self) -> usize {
        unsafe { self.ptr.as_ref().header.num_chunks }
    }

    fn remove_from_list(&self) {
        if let Some(mut prev) = self.prev_page() {
            prev.set_next_page(self.next_page());
        }

        if let Some(mut next) = self.next_page() {
            next.set_prev_page(self.prev_page());
        }
    }

    fn free(&self) {
        self.remove_from_list();
        unsafe {
            let _to_free = Box::from_raw(self.ptr.as_ptr());
        }
    }

    fn new(capacity: usize, prev_page: Option<PagePtr<T>>, next_page: Option<PagePtr<T>>) -> Self {
        // Implementation mostly based on arc.rs
        assert!(capacity > 0);

        // Find size of the HeaderSlice
        let slice_offset = offset_of!(Page<T>, slice);
        let slice_size = mem::size_of::<Chunk<T>>().checked_mul(capacity).expect("size overflow");
        let usable_size = slice_offset.checked_add(slice_size).expect("size overflows");

        // Round size up to alignment
        let align = mem::align_of::<Page<T>>();
        let size = usable_size.wrapping_add(align - 1) & !(align - 1);
        assert!(size >= usable_size, "size overflows");

        let layout = Layout::from_size_align(size, align).expect("invalid layout");

        unsafe {
            let buffer = alloc::alloc(layout);
            if buffer.is_null() {
                alloc::handle_alloc_error(layout);
            }

            let ptr = buffer as *mut Page<T>;
            let result = Self { ptr: ptr::NonNull::new_unchecked(ptr) };

            let mut current = ptr::addr_of_mut!((*ptr).slice) as *mut Chunk<T>;

            let header = PageHeader {
                prev_page,
                next_page,
                free_chunk: Some(ptr::NonNull::new_unchecked(current)),
                num_chunks: 0,
                _p: PhantomData,
            };

            ptr::write(ptr::addr_of_mut!((*ptr).header), header);
            for idx in 0..capacity {
                let chunk = Chunk {
                    data: MaybeUninit::uninit(),
                    next: if idx == capacity - 1 {
                        None
                    } else {
                        Some(ptr::NonNull::new_unchecked(current.offset(1)))
                    },
                    page: result,
                };
                ptr::write(current, chunk);
                current = current.offset(1);
            }

            result
        }
    }

    #[inline]
    fn allocate(&mut self, item: T) -> ptr::NonNull<T> {
        let header = unsafe { &mut self.ptr.as_mut().header };
        unsafe {
            header.free_chunk.unwrap().as_mut().data.write(item);
        }

        header.num_chunks += 1;
        let result = header.free_chunk.unwrap();
        header.free_chunk = unsafe { header.free_chunk.unwrap().as_ref().next };

        chunk_to_ptr(result)
    }
}

pub struct Pool<T> {
    free_pages: Option<PagePtr<T>>,
    full_pages: Option<PagePtr<T>>,

    num_empty_pages: usize,

    max_empty_pages: usize,

    page_capacity: usize,
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Self::new(1024, 4)
    }
}

impl<T> Drop for Pool<T> {
    fn drop(&mut self) {
        while let Some(ptr) = self.free_pages {
            self.free_pages = ptr.next_page();
            ptr.free();
        }

        while let Some(ptr) = self.full_pages {
            self.full_pages = ptr.next_page();
            ptr.free();
        }
    }
}

impl<T> Pool<T> {
    pub fn new(page_capacity: usize, max_empty_pages: usize) -> Self {
        debug_assert!(page_capacity > 0);
        debug_assert!(max_empty_pages > 0);

        Self {
            free_pages: None,
            full_pages: None,
            num_empty_pages: 0,
            max_empty_pages,
            page_capacity,
        }
    }

    #[inline]
    pub fn allocate(&mut self, item: T) -> ptr::NonNull<T> {
        if self.free_pages.is_none() {
            self.free_pages = Some(PagePtr::new(self.page_capacity, None, None));
            self.num_empty_pages += 1;
        }

        let mut free_page = self.free_pages.unwrap();
        if free_page.num_chunks() == 0 {
            self.num_empty_pages -= 1;
        }

        let result = free_page.allocate(item);
        if self.page_capacity == free_page.num_chunks() {
            self.free_pages = free_page.next_page();
            free_page.remove_from_list();
            free_page.link_next_page(self.full_pages);
            free_page.set_prev_page(None);
            self.full_pages = Some(free_page);
        }

        result
    }

    #[inline]
    pub fn deallocate(&mut self, mut item: ptr::NonNull<T>) {
        let mut chunk_ptr = ptr_to_chunk(item);
        let mut page = unsafe { chunk_ptr.as_ref().page };
        let was_full_page = unsafe { page.ptr.as_ref().header.num_chunks == self.page_capacity };
        let empty_page = unsafe { page.ptr.as_ref().header.num_chunks - 1 == 0 };

        unsafe {
            ptr::drop_in_place(item.as_mut());
            chunk_ptr.as_mut().next = page.ptr.as_ref().header.free_chunk;

            page.ptr.as_mut().header.free_chunk = Some(chunk_ptr);
            page.ptr.as_mut().header.num_chunks -= 1;

            if was_full_page {
                if page.prev_page().is_none() {
                    // head of list
                    self.full_pages = page.next_page();
                }
                page.remove_from_list();
                page.link_next_page(self.free_pages);
                page.set_prev_page(None);
                self.free_pages = Some(page);
            } else if empty_page && self.max_empty_pages <= self.num_empty_pages {
                if page.prev_page().is_none() {
                    // head of list
                    self.free_pages = page.next_page();
                }
                page.free();
            } else if empty_page {
                self.num_empty_pages += 1;
            }
        }
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
        let mut pool = Pool::new(2, 10);
        let mut allocated = Vec::new();
        for id in 0..100 {
            allocated.push(pool.allocate(TestStruct { id }));
        }

        for id in 0..100 {
            unsafe {
                assert_eq!(allocated[id].as_ref().id, id);
            }
        }

        for it in allocated.iter() {
            pool.deallocate(*it);
        }
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
