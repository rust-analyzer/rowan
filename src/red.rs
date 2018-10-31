use parking_lot::RwLock;
use crate::{roots::RedPtr, GreenNode, TextUnit, Types};

#[derive(Debug)]
pub(crate) struct RedNode<T: Types> {
    green: GreenNode<T>,
    parent: Option<ParentData<T>>,
    children: RwLock<Box<[RedChild<T>]>>,
}

#[derive(Debug)]
enum RedChild<T: Types> {
    Zigot(TextUnit),
    Child(RedNode<T>),
}

impl<T: Types> RedChild<T> {
    fn set(&mut self, node: RedNode<T>) -> &RedNode<T> {
        match self {
            RedChild::Child(node) => node,
            RedChild::Zigot(_) => {
                *self = RedChild::Child(node);
                match self {
                    RedChild::Child(node) => node,
                    RedChild::Zigot(_) => unreachable!(),
                }
            }
        }
    }
}

#[derive(Debug)]
struct ParentData<T: Types> {
    parent: RedPtr<T>,
    start_offset: TextUnit,
    index_in_parent: usize,
}

impl<T: Types> RedNode<T> {
    pub fn new_root(green: GreenNode<T>) -> RedNode<T> {
        RedNode::new(green, None)
    }

    fn new_child(
        green: GreenNode<T>,
        parent: RedPtr<T>,
        start_offset: TextUnit,
        index_in_parent: usize,
    ) -> RedNode<T> {
        let parent_data = ParentData {
            parent,
            start_offset,
            index_in_parent,
        };
        RedNode::new(green, Some(parent_data))
    }

    fn new(green: GreenNode<T>, parent: Option<ParentData<T>>) -> RedNode<T> {
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
                RedChild::Zigot(off)
            }).collect::<Vec<_>>()
            .into_boxed_slice();
        RedNode {
            green,
            parent,
            children: RwLock::new(children),
        }
    }

    pub(crate) fn green(&self) -> &GreenNode<T> {
        &self.green
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

    pub(crate) fn get_child(&self, idx: usize) -> Option<RedPtr<T>> {
        if idx >= self.n_children() {
            return None;
        }
        let start_offset = match &self.children.read()[idx] {
            RedChild::Child(child) => return Some(RedPtr::new(child)),
            RedChild::Zigot(start_offset) => *start_offset,
        };
        let green_children = self.green.children();
        let child = RedNode::new_child(
            green_children[idx].clone(),
            RedPtr::new(self),
            start_offset,
            idx,
        );
        let mut children = self.children.write();
        let child = children[idx].set(child);
        Some(RedPtr::new(child))
    }

    pub(crate) fn parent(&self) -> Option<RedPtr<T>> {
        Some(self.parent.as_ref()?.parent)
    }
    pub(crate) fn index_in_parent(&self) -> Option<usize> {
        Some(self.parent.as_ref()?.index_in_parent)
    }
}
