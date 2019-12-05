use {
    crate::{
        green::GreenElementBorrow,
        syntax::node::{NodeInner, NodeKind},
    },
    rc_box::RcBox,
    std::{cell::RefCell, convert::TryInto, rc::Rc},
};

pub(super) struct FreeList {
    first: Option<RcBox<NodeInner>>,
    len: usize,
}

const FREE_LIST_LEN: usize = 128;

impl FreeList {
    pub(super) fn push(&mut self, node: Rc<NodeInner>) {
        if self.len >= FREE_LIST_LEN {
            return;
        }
        if let Ok(it) = node.try_into() {
            self.push_(it);
        };
    }

    fn push_(&mut self, mut node: RcBox<NodeInner>) {
        node.kind = NodeKind::Free(self.first.take());
        self.first = Some(node);
        self.len += 1;
    }

    pub(super) fn pop(&mut self) -> Option<RcBox<NodeInner>> {
        let mut node = self.first.take()?;
        self.len -= 1;
        self.first = match &mut node.kind {
            NodeKind::Free(next) => next.take(),
            _ => unreachable!(),
        };
        Some(node)
    }

    pub(super) fn new() -> FreeList {
        let mut list = FreeList { first: None, len: 0 };
        for _ in 0..FREE_LIST_LEN {
            unsafe {
                list.push_(RcBox::new(NodeInner {
                    kind: NodeKind::Free(None),
                    green: GreenElementBorrow::dangling(),
                }));
            }
        }
        list
    }

    pub(super) fn with<R>(f: impl FnOnce(&mut FreeList) -> R) -> R {
        thread_local! {
            static INSTANCE: RefCell<FreeList> = RefCell::new(FreeList::new());
        }
        INSTANCE.with(|it| f(&mut it.borrow_mut()))
    }
}
