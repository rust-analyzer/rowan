use std::{ptr, sync::Arc};

use crate::{swap_cell::SwapCell, GreenNode, SyntaxNode, TextUnit, TreePtr, Types};

#[derive(Debug)]
pub(crate) struct SyntaxRoot<T: Types> {
    pub(crate) root_node: SyntaxNode<T>,
    pub(crate) data: T::RootData,
}

#[derive(Debug)]
pub(crate) struct ParentData<T: Types> {
    parent: ptr::NonNull<SyntaxNode<T>>,
    pub(crate) start_offset: TextUnit,
    index_in_parent: u32,
}

impl<T: Types> TreePtr<T> {
    pub(crate) fn new(node: &SyntaxNode<T>) -> TreePtr<T> {
        let root: Arc<SyntaxRoot<T>> = unsafe { Arc::from_raw(node.root) };
        std::mem::forget(Arc::clone(&root));
        std::mem::forget(root);
        TreePtr {
            inner: node as *const SyntaxNode<T>,
        }
    }
}

unsafe impl<T: Types> Send for TreePtr<T>
where
    T::RootData: Send,
    T::Kind: Send,
{
}

unsafe impl<T: Types> Sync for TreePtr<T>
where
    T::RootData: Sync,
    T::Kind: Sync,
{
}

impl<T: Types> std::ops::Deref for TreePtr<T> {
    type Target = SyntaxNode<T>;
    fn deref(&self) -> &SyntaxNode<T> {
        unsafe { &*self.inner }
    }
}

impl<T: Types> Clone for TreePtr<T> {
    fn clone(&self) -> TreePtr<T> {
        TreePtr::new(&*self)
    }
}

impl<T: Types> SyntaxNode<T> {
    pub(crate) fn new_root(green: GreenNode<T>, data: T::RootData) -> TreePtr<T> {
        let root = Arc::new(SyntaxRoot {
            root_node: SyntaxNode::new_impl(ptr::null(), None, green),
            data,
        });
        let red_node = &root.root_node as *const SyntaxNode<T>;
        let root_ptr: *const SyntaxRoot<T> = Arc::into_raw(root);
        unsafe {
            (*(red_node as *mut SyntaxNode<T>)).root = root_ptr;
        }
        TreePtr { inner: red_node }
    }

    fn new_child(
        root: *const SyntaxRoot<T>,
        parent: ptr::NonNull<SyntaxNode<T>>,
        green: GreenNode<T>,
        start_offset: TextUnit,
        index_in_parent: usize,
    ) -> SyntaxNode<T> {
        let parent_data = ParentData {
            parent,
            start_offset,
            index_in_parent: index_in_parent as u32,
        };
        SyntaxNode::new_impl(root, Some(parent_data), green)
    }

    fn new_impl(
        root: *const SyntaxRoot<T>,
        parent: Option<ParentData<T>>,
        green: GreenNode<T>,
    ) -> SyntaxNode<T> {
        let mut start_offset = parent
            .as_ref()
            .map(|it| it.start_offset)
            .unwrap_or(0.into());
        let children = green
            .children()
            .iter()
            .map(|child| {
                let off = start_offset;
                start_offset += child.text_len();
                SwapCell::new(off)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        SyntaxNode {
            root,
            parent,
            green,
            children,
        }
    }

    pub(crate) fn root(&self) -> &SyntaxRoot<T> {
        unsafe { &*self.root }
    }

    pub(crate) fn parent_impl(&self) -> Option<&SyntaxNode<T>> {
        let ptr = self.parent.as_ref()?.parent;
        Some(unsafe { &*ptr.as_ptr() })
    }
    pub(crate) fn start_offset(&self) -> TextUnit {
        match &self.parent {
            None => 0.into(),
            Some(p) => p.start_offset,
        }
    }

    pub(crate) fn n_children(&self) -> usize {
        self.green.children().len()
    }

    pub(crate) fn get_child(&self, idx: usize) -> Option<&SyntaxNode<T>> {
        if idx >= self.n_children() {
            return None;
        }
        let child = self.children[idx].get_or_init(|start_offset| {
            let green_children = self.green.children();
            SyntaxNode::new_child(
                self.root,
                ptr::NonNull::from(self),
                green_children[idx].clone(),
                start_offset,
                idx,
            )
        });
        Some(child)
    }

    pub(crate) fn index_in_parent(&self) -> Option<usize> {
        Some(self.parent.as_ref()?.index_in_parent as usize)
    }
}
