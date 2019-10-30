use memoffset::offset_of;
use static_assertions::*;
use std::{
    alloc::{alloc, dealloc, handle_alloc_error, Layout},
    borrow::{Borrow, ToOwned},
    cell::RefCell,
    convert::TryInto,
    fmt,
    hash::{Hash, Hasher},
    isize,
    marker::PhantomData,
    mem,
    mem::size_of,
    ops::Deref,
    ptr,
    sync::atomic::{self, AtomicUsize, Ordering},
    sync::Arc,
};

use crate::{cursor::SyntaxKind, NodeOrToken, SmolStr, TextUnit};

// GreenNode uses a custom layout with align(u64).
//
// head: GreenNodeHead {
//   text_len: TextUnit (u32)
//   kind: SyntaxKind (u16)
//   child_count: u16
// }
// [0usize ArcGreenNode OR 1usize GreenToken]

const NODE_TAG: u64 = 0;
const TOKEN_TAG: u64 = 1;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct GreenNodeHead {
    text_len: TextUnit,
    kind: SyntaxKind,
    child_count: u16,
}

assert_eq_align!(GreenNodeHead, u32);
assert_eq_size!(GreenNodeHead, u64);

/// # Safety
///
/// This struct is not actually sized.
/// This struct is only sound behind a reference or `ArcGreenNode`.
#[repr(C)] // defined layout
pub struct GreenNode {
    head: GreenNodeHead,
    /// trailing VLA
    tail: [u64; 0], // FUTURE: make `GreenNode` actually `?Sized` with `extern type`
                    // NB: will make the current version of offset_of! not work, sadly
}

assert_eq_align!(GreenNode, u64);

impl fmt::Debug for GreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct DebugChildren<'a>(&'a GreenNode);

        impl fmt::Debug for DebugChildren<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut debug = f.debug_list();
                for child in self.0.children() {
                    debug.entry(&child);
                }
                debug.finish()
            }
        }

        f.debug_struct("GreenNode")
            .field("head", &self.head)
            .field("children", &DebugChildren(self))
            .finish()
    }
}

impl ToOwned for GreenNode {
    type Owned = ArcGreenNode;
    fn to_owned(&self) -> ArcGreenNode {
        let ptr = self as *const GreenNode;
        #[allow(clippy::unneeded_field_pattern)] // offset_of!
        unsafe {
            // Safe because GreenNode always lives behind a ArcGreenNode
            let ptr = ptr
                .cast::<u8>()
                .sub(offset_of!(ArcGreenNodeInner, node))
                .cast::<ArcGreenNodeInner>();
            let arc = ArcGreenNode { raw: ptr::NonNull::new_unchecked(ptr as *mut _) };
            // Increase the ref count by one
            mem::forget(arc.clone());
            arc
        }
    }
}

impl Eq for GreenNode {}
impl PartialEq for GreenNode {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.children().eq(other.children())
    }
}

impl Hash for GreenNode {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.head.hash(state);
        self.children().for_each(|child| child.hash(state));
    }
}

impl Drop for GreenNode {
    fn drop(&mut self) {
        for child in self.children_mut() {
            match child {
                NodeOrToken::Token(token) => unsafe { ptr::drop_in_place(token) },
                NodeOrToken::Node(node) => unsafe { ptr::drop_in_place(node) },
            }
        }
    }
}

impl GreenNode {
    fn layout_with(node_count: usize, token_count: usize) -> Layout {
        let size = mem::size_of::<GreenNodeHead>()
            + (node_count * (mem::size_of::<u64>() + mem::size_of::<u64>()))
            + (token_count * (mem::size_of::<u64>() + mem::size_of::<GreenToken>()))
            + mem::size_of::<u64>();
        let align = mem::align_of::<GreenNode>();
        Layout::from_size_align(size, align).unwrap()
    }

    fn layout(&self) -> Layout {
        let (nodes, tokens) = self.children().fold((0, 0), |(nodes, tokens), el| match el {
            NodeOrToken::Node(_) => (nodes + 1, tokens),
            NodeOrToken::Token(_) => (nodes, tokens + 1),
        });
        GreenNode::layout_with(nodes, tokens)
    }
}

#[repr(C)]
struct ArcGreenNodeInner {
    count: AtomicUsize,
    node: GreenNode,
}

assert_eq_align!(ArcGreenNodeInner, u64);

impl ArcGreenNodeInner {
    fn layout(&self) -> Layout {
        let node_layout = self.node.layout();
        Layout::from_size_align(
            node_layout.size() + mem::size_of::<AtomicUsize>(),
            node_layout.align(),
        )
        .unwrap()
    }
}

pub struct ArcGreenNode {
    raw: ptr::NonNull<ArcGreenNodeInner>,
}

assert_eq_align!(ArcGreenNode, usize);
assert_eq_size!(ArcGreenNode, usize);

unsafe impl Send for ArcGreenNode {}
unsafe impl Sync for ArcGreenNode {}

impl Deref for ArcGreenNode {
    type Target = GreenNode;
    fn deref(&self) -> &GreenNode {
        &unsafe { self.raw.as_ref() }.node
    }
}

impl fmt::Debug for ArcGreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

impl Clone for ArcGreenNode {
    fn clone(&self) -> Self {
        // Relaxed is what the stdlib uses: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L925-L935>
        let old_size = unsafe { self.raw.as_ref() }.count.fetch_add(1, Ordering::Relaxed);
        // But protect against runaway clone/forget: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L938-L946>
        if old_size > isize::MAX as usize {
            std::process::abort();
        }
        ArcGreenNode { raw: self.raw }
    }
}

impl Eq for ArcGreenNode {}
impl PartialEq for ArcGreenNode {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl Borrow<GreenNode> for ArcGreenNode {
    fn borrow(&self) -> &GreenNode {
        self.deref()
    }
}

impl Hash for ArcGreenNode {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.deref().hash(state)
    }
}

impl Drop for ArcGreenNode {
    #[inline]
    fn drop(&mut self) {
        // Lower the reference count: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L1196-L1198>
        if unsafe { self.raw.as_ref() }.count.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }

        // Synchronize with other threads: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L1203-L1230>
        atomic::fence(Ordering::Acquire);

        // Don't inline slow path:
        unsafe fn drop_real(this: &mut ArcGreenNode) {
            let layout = this.raw.as_ref().layout();
            ptr::drop_in_place(&mut this.raw.as_mut().node);
            dealloc(this.raw.as_ptr().cast(), layout)
        }

        unsafe { drop_real(self) }
    }
}

impl GreenNode {
    /// Creates new Node.
    #[allow(clippy::new_ret_no_self, clippy::boxed_local)]
    pub fn new(kind: SyntaxKind, children: Vec<OwnedGreenElement>) -> ArcGreenNode {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        let head =
            GreenNodeHead { kind, text_len, child_count: children.len().try_into().unwrap() };

        let node_count = children.iter().filter(|el| el.as_node().is_some()).count();
        let token_count = children.iter().filter(|el| el.as_token().is_some()).count();
        let layout = GreenNode::layout_with(node_count, token_count);

        #[allow(clippy::unneeded_field_pattern)] // offset_of
        unsafe {
            let arc_inner = ptr::NonNull::new(alloc(layout))
                .unwrap_or_else(|| handle_alloc_error(layout))
                .cast::<ArcGreenNodeInner>();
            ptr::write(
                arc_inner
                    .as_ptr()
                    .cast::<u8>()
                    .add(offset_of!(ArcGreenNodeInner, count))
                    .cast::<AtomicUsize>(),
                AtomicUsize::new(1),
            );

            let node = ptr::NonNull::new_unchecked(
                arc_inner
                    .as_ptr()
                    .cast::<u8>()
                    .add(offset_of!(ArcGreenNodeInner, node))
                    .cast::<GreenNode>(),
            );
            ptr::write(
                node.as_ptr().cast::<u8>().add(offset_of!(GreenNode, head)).cast::<GreenNodeHead>(),
                head,
            );

            let mut vla = node.as_ptr().cast::<u8>().add(offset_of!(GreenNode, head)).cast::<u64>();
            for el in children {
                match el {
                    NodeOrToken::Node(node) => {
                        ptr::write(vla, NODE_TAG);
                        let this_node = vla.offset(1).cast::<ArcGreenNode>();
                        ptr::write(this_node, node);
                        vla = this_node.offset(1).cast::<u64>();
                    }
                    NodeOrToken::Token(token) => {
                        ptr::write(vla, TOKEN_TAG);
                        let this_token = vla.offset(1).cast::<GreenToken>();
                        ptr::write(this_token, token);
                        vla = this_token.offset(1).cast::<u64>();
                    }
                }
            }

            ArcGreenNode { raw: arc_inner }
        }
    }

    /// Kind of this node.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.head.kind
    }

    /// Length of the text, covered by this node.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        self.head.text_len
    }

    /// How many children does this node have?
    pub fn child_count(&self) -> usize {
        self.head.child_count as usize
    }

    /// Children of this node.
    #[inline]
    pub fn children(&self) -> GreenChildren<'_> {
        GreenChildren {
            next: self.tail.as_ptr(),
            remaining: self.child_count(),
            marker: PhantomData,
        }
    }

    fn children_mut(&mut self) -> GreenChildrenMut<'_> {
        GreenChildrenMut {
            next: self.tail.as_mut_ptr(),
            remaining: self.child_count(),
            marker: PhantomData,
        }
    }
}

macro_rules! green_child_next {
    ($self:ident) => {};
}

#[derive(Debug, Clone)]
pub struct GreenChildren<'a> {
    next: *const u64,
    remaining: usize,
    marker: PhantomData<&'a GreenNode>,
}

pub struct GreenChildrenMut<'a> {
    next: *mut u64,
    remaining: usize,
    marker: PhantomData<&'a mut GreenNode>,
}

unsafe impl Send for GreenChildren<'_> {}
unsafe impl Sync for GreenChildren<'_> {}
unsafe impl Send for GreenChildrenMut<'_> {}
unsafe impl Sync for GreenChildrenMut<'_> {}

impl<'a> Iterator for GreenChildren<'a> {
    type Item = GreenElementRef<'a>;

    fn next(&mut self) -> Option<GreenElementRef<'a>> {
        if self.remaining > 0 {
            self.remaining -= 1;
            unsafe {
                match *self.next {
                    NODE_TAG => {
                        let node_ptr = self.next.offset(1).cast::<ArcGreenNode>();
                        self.next = node_ptr.offset(1).cast::<u64>();
                        Some((&**node_ptr).into())
                    }
                    TOKEN_TAG => {
                        let token_ptr = self.next.offset(1).cast::<GreenToken>();
                        self.next = token_ptr.offset(1).cast::<u64>();
                        Some((&*token_ptr).into())
                    }
                    _ => unreachable!(),
                }
            }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'a> Iterator for GreenChildrenMut<'a> {
    type Item = GreenElementRefMut<'a>;

    fn next(&mut self) -> Option<GreenElementRefMut<'a>> {
        if self.remaining > 0 {
            self.remaining -= 1;
            unsafe {
                match *self.next {
                    NODE_TAG => {
                        let node_ptr = self.next.offset(1).cast::<ArcGreenNode>();
                        self.next = node_ptr.offset(1).cast::<u64>();
                        Some((&mut *node_ptr).into())
                    }
                    TOKEN_TAG => {
                        let token_ptr = self.next.offset(1).cast::<GreenToken>();
                        self.next = token_ptr.offset(1).cast::<u64>();
                        Some((&mut *token_ptr).into())
                    }
                    _ => unreachable!(),
                }
            }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for GreenChildren<'_> {
    fn len(&self) -> usize {
        self.remaining
    }
}

impl ExactSizeIterator for GreenChildrenMut<'_> {
    fn len(&self) -> usize {
        self.remaining
    }
}

/// Leaf node in the immutable tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    kind: SyntaxKind,
    text: SmolStr,
}

impl GreenToken {
    /// Creates new Token.
    #[inline]
    pub fn new(kind: SyntaxKind, text: SmolStr) -> GreenToken {
        GreenToken { kind, text }
    }
    /// Kind of this Token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }
    /// Text of this Token.
    #[inline]
    pub fn text(&self) -> &SmolStr {
        &self.text
    }
    /// Text of this Token.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        TextUnit::from_usize(self.text.len())
    }
}

pub(crate) type OwnedGreenElement = NodeOrToken<ArcGreenNode, GreenToken>;
pub(crate) type GreenElementRef<'a> = NodeOrToken<&'a GreenNode, &'a GreenToken>;
pub(crate) type GreenElementRefMut<'a> = NodeOrToken<&'a mut ArcGreenNode, &'a mut GreenToken>;

impl From<ArcGreenNode> for OwnedGreenElement {
    #[inline]
    fn from(node: ArcGreenNode) -> OwnedGreenElement {
        NodeOrToken::Node(node)
    }
}

impl<'a> From<&'a GreenNode> for GreenElementRef<'a> {
    #[inline]
    fn from(node: &'a GreenNode) -> GreenElementRef<'a> {
        NodeOrToken::Node(node)
    }
}

impl<'a> From<&'a mut ArcGreenNode> for GreenElementRefMut<'a> {
    #[inline]
    fn from(node: &'a mut ArcGreenNode) -> GreenElementRefMut<'a> {
        NodeOrToken::Node(node)
    }
}

impl From<GreenToken> for OwnedGreenElement {
    #[inline]
    fn from(token: GreenToken) -> OwnedGreenElement {
        NodeOrToken::Token(token)
    }
}

impl<'a> From<&'a GreenToken> for GreenElementRef<'a> {
    #[inline]
    fn from(token: &'a GreenToken) -> GreenElementRef<'a> {
        NodeOrToken::Token(token)
    }
}

impl<'a> From<&'a mut GreenToken> for GreenElementRefMut<'a> {
    #[inline]
    fn from(token: &'a mut GreenToken) -> GreenElementRefMut<'a> {
        NodeOrToken::Token(token)
    }
}

impl GreenElementRef<'_> {
    pub(crate) fn to_owned(self) -> OwnedGreenElement {
        match self {
            NodeOrToken::Node(node) => NodeOrToken::Node(node.to_owned()),
            NodeOrToken::Token(token) => NodeOrToken::Token(token.clone()),
        }
    }
}

macro_rules! green_element_methods {
    () => {
        /// Returns kind of this element.
        #[inline]
        pub fn kind(&self) -> SyntaxKind {
            match self {
                NodeOrToken::Node(it) => it.kind(),
                NodeOrToken::Token(it) => it.kind(),
            }
        }
        /// Returns length of the text covered by this element.
        #[inline]
        pub fn text_len(&self) -> TextUnit {
            match self {
                NodeOrToken::Node(it) => it.text_len(),
                NodeOrToken::Token(it) => it.text_len(),
            }
        }
    };
}

impl OwnedGreenElement {
    green_element_methods!();
}

impl GreenElementRef<'_> {
    green_element_methods!();
}

impl GreenElementRefMut<'_> {
    green_element_methods!();
}

/// A checkpoint for maybe wrapping a node. See `GreenNodeBuilder::checkpoint` for details.
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(usize);

/// A builder for a green tree.
#[derive(Default, Debug)]
pub struct GreenNodeBuilder {
    cache: rustc_hash::FxHashSet<ArcGreenNode>,
    parents: Vec<(SyntaxKind, usize)>,
    children: Vec<OwnedGreenElement>,
}

impl GreenNodeBuilder {
    /// Creates new builder.
    #[inline]
    pub fn new() -> GreenNodeBuilder {
        GreenNodeBuilder::default()
    }
    /// Adds new token to the current branch.
    #[inline]
    pub fn token(&mut self, kind: SyntaxKind, text: SmolStr) {
        let token = GreenToken { kind, text };
        self.children.push(token.into());
    }
    /// Start new node and make it current.
    #[inline]
    pub fn start_node(&mut self, kind: SyntaxKind) {
        let len = self.children.len();
        self.parents.push((kind, len));
    }
    /// Finish current branch and restore previous
    /// branch as current.
    #[inline]
    pub fn finish_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let children: Vec<_> = self.children.drain(first_child..).collect();
        let mut node = GreenNode::new(kind, children);
        // Green nodes are fully immutable, so it's ok to deduplicate them.
        // This is the same optimization that Roslyn does
        // https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees
        //
        // For example, all `#[inline]` in this file share the same green node!
        // For `libsyntax/parse/parser.rs`, measurements show that deduping saves
        // 17% of the memory for green nodes!
        // Future work: make hashing faster by avoiding rehashing of subtrees.
        if node.child_count() <= 3 {
            match self.cache.get(&node) {
                Some(existing) => node = existing.clone(),
                None => assert!(self.cache.insert(node.clone())),
            }
        }
        self.children.push(node.into());
    }
    /// Prepare for maybe wrapping the next node.
    /// The way wrapping works is that you first of all get a checkpoint,
    /// then you place all tokens you want to wrap, and then *maybe* call
    /// `start_node_at`.
    /// Example:
    /// ```rust
    /// # use rowan::{GreenNodeBuilder, cursor::SyntaxKind};
    /// # const PLUS: SyntaxKind = SyntaxKind(0);
    /// # const OPERATION: SyntaxKind = SyntaxKind(1);
    /// # struct Parser;
    /// # impl Parser {
    /// #     fn peek(&self) -> Option<SyntaxKind> { None }
    /// #     fn parse_expr(&mut self) {}
    /// # }
    /// # let mut builder = GreenNodeBuilder::new();
    /// # let mut parser = Parser;
    /// let checkpoint = builder.checkpoint();
    /// parser.parse_expr();
    /// if parser.peek() == Some(PLUS) {
    ///   // 1 + 2 = Add(1, 2)
    ///   builder.start_node_at(checkpoint, OPERATION);
    ///   parser.parse_expr();
    ///   builder.finish_node();
    /// }
    /// ```
    #[inline]
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.children.len())
    }
    /// Wrap the previous branch marked by `checkpoint` in a new branch and
    /// make it current.
    #[inline]
    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        let Checkpoint(checkpoint) = checkpoint;
        assert!(
            checkpoint <= self.children.len(),
            "checkpoint no longer valid, was finish_node called early?"
        );

        if let Some(&(_, first_child)) = self.parents.last() {
            assert!(
                checkpoint >= first_child,
                "checkpoint no longer valid, was an unmatched start_node_at called?"
            );
        }

        self.parents.push((kind, checkpoint));
    }
    /// Complete tree building. Make sure that
    /// `start_node_at` and `finish_node` calls
    /// are paired!
    #[inline]
    pub fn finish(mut self) -> ArcGreenNode {
        assert_eq!(self.children.len(), 1);
        match self.children.pop().unwrap() {
            NodeOrToken::Node(node) => node,
            NodeOrToken::Token(_) => panic!(),
        }
    }
}
