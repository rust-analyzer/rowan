use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::Range,
    sync::Arc,
    iter::repeat,
};

use {
    red::RedNode,
    roots::{OwnedRoot, RedPtr, RefRoot, SyntaxRoot, TreeRoot},
    GreenNode, SmolStr, TextRange, Types,
};

/// An immutable lazy constructed syntax tree with
/// offsets and parent pointers.
///
/// The design is close to
/// https://github.com/apple/swift/tree/bc3189a2d265bf7728ea0cfeb55f032bfe5beaf1/lib/Syntax
///
/// `SyntaxNode` exists in two flavors:
///   * owned (R = OwnedRoot<T>)
///   * borrowed (R = RefRoot<'a, T>)
///
/// Borrowed `SyntaxNode` is `Copy`, but is parametrized over a lifetime,
/// with a corresponding ergonomics hit.
///
/// Owned `SyntaxNode` is `Clone` (using `Arc::clone` under the hood) and
/// is not parametrized over a lifetime. Note that because of the parent
/// links `SyntaxNode` keeps all of its ancestors alive, and not only descendants,
/// so keep an eye on memory leaks.
///
/// Methods like `parent` or `children` preserve the flavor (borrowed or owned)
/// of nodes, but you can switch between them at any time using `.borrowed()`
/// and `.owned()` methods. As a rule of thumb, when *processing* nodes, use
/// borrowed version to avoid excessive Arc traffic, and, when *storing* nodes
/// in data structures, use owned variant, to avoid dealing with lifetimes.
///
/// `SyntaxNode` have object identity equality and hash semantics.
pub struct SyntaxNode<T: Types, R: TreeRoot<T>> {
    pub(crate) root: R,
    // Guaranteed to not dangle, because `root` holds a
    // strong reference to red's ancestor
    red: RedPtr<T>,
}

// unsafe impl<T: Types, R: TreeRoot<T>> Send for SyntaxNode<T, R> {}
// unsafe impl<T: Types, R: TreeRoot<T>> Sync for SyntaxNode<T, R> {}

impl<T, R1, R2> PartialEq<SyntaxNode<T, R1>> for SyntaxNode<T, R2>
where
    T: Types,
    R1: TreeRoot<T>,
    R2: TreeRoot<T>,
{
    fn eq(&self, other: &SyntaxNode<T, R1>) -> bool {
        self.red == other.red
    }
}

impl<T: Types, R: TreeRoot<T>> Eq for SyntaxNode<T, R> {}
impl<T: Types, R: TreeRoot<T>> Hash for SyntaxNode<T, R> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.red.hash(state)
    }
}

impl<T: Types, R: TreeRoot<T> + Clone> Clone for SyntaxNode<T, R> {
    fn clone(&self) -> SyntaxNode<T, R> {
        SyntaxNode {
            root: self.root.clone(),
            red: self.red,
        }
    }
}

impl<T: Types, R: TreeRoot<T> + Copy> Copy for SyntaxNode<T, R> {}

impl<T: Types> SyntaxNode<T, OwnedRoot<T>> {
    /// Creates a new `SyntaxNode`.
    pub fn new(green: GreenNode<T>, data: T::RootData) -> Self {
        let root = SyntaxRoot {
            red: RedNode::new_root(green),
            data,
        };
        let root = OwnedRoot(Arc::new(root));
        let red = RedPtr::new(&root.syntax_root().red);
        SyntaxNode { root, red }
    }
}

/// `WalkeEvent` describes tree walking process.
#[derive(Debug, Copy, Clone)]
pub enum WalkEvent<T> {
    /// Fired before traversing the node.
    Enter(T),
    /// Fired after the node is traversed.
    Leave(T),
}

impl<'a, T: Types> SyntaxNode<T, RefRoot<'a, T>> {
    /// Text of this node if it is a leaf.
    // Only for `RefRoot` to extend lifetime to `'a`.
    pub fn leaf_text(self) -> Option<&'a SmolStr> {
        let red = unsafe { self.red.get(self.root.syntax_root()) };
        red.green().leaf_text()
    }

    /// Traverse the subtree rooted at the current node in preorder.
    pub fn preorder(self) -> impl Iterator<Item = WalkEvent<SyntaxNode<T, RefRoot<'a, T>>>> {
        generate(Some(WalkEvent::Enter(self)), move |pos| {
            let next = match *pos {
                WalkEvent::Enter(node) => match node.first_child() {
                    Some(child) => WalkEvent::Enter(child),
                    None => WalkEvent::Leave(node),
                },
                WalkEvent::Leave(node) => {
                    if node == self {
                        return None;
                    }
                    match node.next_sibling() {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(node.parent().unwrap()),
                    }
                }
            };
            Some(next)
        })
    }
}

impl<T: Types, R: TreeRoot<T>> SyntaxNode<T, R> {
    /// Switch this node to borrowed flavor.
    pub fn borrowed<'a>(&'a self) -> SyntaxNode<T, RefRoot<'a, T>> {
        SyntaxNode {
            root: self.root.borrowed(),
            red: self.red,
        }
    }
    /// Switch this node to owned flavor.
    pub fn owned(&self) -> SyntaxNode<T, OwnedRoot<T>> {
        SyntaxNode {
            root: self.root.owned(),
            red: self.red,
        }
    }
    /// Get root data.
    pub fn root_data(&self) -> &T::RootData {
        &self.root.syntax_root().data
    }
    /// Get kind of this node.
    pub fn kind(&self) -> T::Kind {
        self.red().green().kind()
    }
    /// Get text range, covered by this node.
    pub fn range(&self) -> TextRange {
        let red = self.red();
        TextRange::offset_len(red.start_offset(), red.green().text_len())
    }
    /// Get the parent node.
    pub fn parent(&self) -> Option<SyntaxNode<T, R>> {
        let parent = self.red().parent()?;
        Some(SyntaxNode {
            root: self.root.clone(),
            red: parent,
        })
    }
    /// Get iterator over children.
    pub fn children(&self) -> SyntaxNodeChildren<T, R> {
        SyntaxNodeChildren {
            parent: self.clone(),
            iter: (0..self.red().n_children()),
        }
    }
    /// Get first child.
    pub fn first_child(&self) -> Option<SyntaxNode<T, R>> {
        let red = self.red().get_child(0)?;
        Some(SyntaxNode {
            root: self.root.clone(),
            red,
        })
    }
    /// Get last child.
    pub fn last_child(&self) -> Option<SyntaxNode<T, R>> {
        let n = self.red().n_children();
        let n = n.checked_sub(1)?;
        let red = self.red().get_child(n)?;
        Some(SyntaxNode {
            root: self.root.clone(),
            red,
        })
    }
    /// Get next sibling.
    pub fn next_sibling(&self) -> Option<SyntaxNode<T, R>> {
        let red = self.red();
        let parent = self.parent()?;
        let next_sibling_idx = red.index_in_parent()? + 1;
        let sibling_red = parent.red().get_child(next_sibling_idx)?;
        Some(SyntaxNode {
            root: self.root.clone(),
            red: sibling_red,
        })
    }
    /// Get previous sibling.
    pub fn prev_sibling(&self) -> Option<SyntaxNode<T, R>> {
        let red = self.red();
        let parent = self.parent()?;
        let prev_sibling_idx = red.index_in_parent()?.checked_sub(1)?;
        let sibling_red = parent.red().get_child(prev_sibling_idx)?;
        Some(SyntaxNode {
            root: self.root.clone(),
            red: sibling_red,
        })
    }
    /// Returns `true` if this node is a leaf node.
    pub fn is_leaf(&self) -> bool {
        self.red().green().leaf_text().is_some()
    }
    /// Returns a green tree, equal to the green tree this node
    /// belongs two, except with this node substitute. The complexity
    /// of operation is proportional to the depth of the tree
    /// TODO: naming is unfortunate, the return value is not *current*
    /// node, it is the new root node.
    pub fn replace_with(&self, green: GreenNode<T>) -> GreenNode<T> {
        assert_eq!(self.kind(), green.kind());
        match self.parent() {
            None => green,
            Some(parent) => {
                let children: Vec<_> = parent
                    .children()
                    .map(|child| {
                        if child == *self {
                            green.clone()
                        } else {
                            child.red().green().clone()
                        }
                    }).collect();
                let new_parent = GreenNode::new_branch(parent.kind(), children.into_boxed_slice());
                parent.replace_with(new_parent)
            }
        }
    }
    fn red(&self) -> &RedNode<T> {
        unsafe { self.red.get(self.root.syntax_root()) }
    }
}

impl<T: Types, R: TreeRoot<T>> fmt::Debug for SyntaxNode<T, R> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}@{:?}", self.kind(), self.range())
    }
}

/// Iterator over node's children.
#[derive(Debug)]
pub struct SyntaxNodeChildren<T: Types, R: TreeRoot<T>> {
    parent: SyntaxNode<T, R>,
    iter: Range<usize>,
}

impl<T: Types, R: TreeRoot<T>> Iterator for SyntaxNodeChildren<T, R> {
    type Item = SyntaxNode<T, R>;

    fn next(&mut self) -> Option<SyntaxNode<T, R>> {
        self.iter.next().map(|i| {
            let red = self.parent.red();
            SyntaxNode {
                root: self.parent.root.clone(),
                red: red.get_child(i).unwrap(),
            }
        })
    }
}


fn generate<'a, T: 'a, F: Fn(&T) -> Option<T> + 'a>(seed: Option<T>, step: F) -> impl Iterator<Item = T> + 'a {
    repeat(())
        .scan(seed, move |state, ()| {
            state.take().map(|curr| {
                *state = step(&curr);
                curr
            })
        })
}
