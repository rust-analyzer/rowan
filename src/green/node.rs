use {
    super::*,
    crate::{cursor::SyntaxKind, TextUnit},
    std::{
        borrow::Borrow,
        fmt, hash, mem,
        ops::Deref,
        ptr, slice,
        sync::{atomic::AtomicU64, Arc},
    },
    thin_dst::{ThinArc, ThinData},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(super) struct GreenNodeHead {
    kind: SyntaxKind,
    text_len: TextUnit,
}

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
#[repr(transparent)]
pub struct GreenNode {
    data: ThinData<GreenNodeHead, GreenElement>,
}

impl fmt::Debug for GreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GreenNode")
            .field("head", &self.data.head)
            .field("children", &self.children())
            .finish()
    }
}

impl Eq for GreenNode {}
impl PartialEq for GreenNode {
    fn eq(&self, other: &GreenNode) -> bool {
        self.data.head == other.data.head && self.children().eq(other.children())
    }
}

impl hash::Hash for GreenNode {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.data.head.hash(state);
        self.children().hash(state);
    }
}

impl GreenNode {
    /// A dummy `GreenNode` for use as a placeholder.
    ///
    /// # Safety
    ///
    /// You _can not_ upgrade this reference to `ArcGreenNode`.
    /// It is a fully valid `GreenNode` otherwise.
    pub(crate) unsafe fn dummy() -> &'static GreenNode {
        #[rustfmt::skip]
        static HEAD: [AtomicU64; 8] = [ // 64 bytes should be enough
            AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0),
            AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0),
        ];
        // produce a zeroed GreenNodeHead with no children
        let this =
            &*(slice::from_raw_parts(HEAD.as_ptr(), 0) as *const [AtomicU64] as *const GreenNode);
        debug_assert!(mem::size_of_val(this) <= mem::size_of_val(&HEAD));
        this
    }

    pub(crate) fn dangling() -> ptr::NonNull<GreenNode> {
        unsafe { GreenNode::dummy().into() }
    }

    /// Creates new Node.
    #[inline]
    pub fn new<I>(kind: SyntaxKind, children: I) -> Arc<GreenNode>
    where
        I: IntoIterator<Item = GreenElement>,
        I::IntoIter: ExactSizeIterator,
    {
        let mut text_len: TextUnit = 0.into();
        let children = children.into_iter().inspect(|it| text_len += it.text_len());
        let res = ThinArc::new(GreenNodeHead { kind, text_len: 0.into() }, children);
        let mut res: Arc<ThinData<GreenNodeHead, GreenElement>> = res.into();
        Arc::get_mut(&mut res).unwrap().head.text_len = text_len;
        ArcGreenNode::into_fat(ArcGreenNode::from_raw(res))
    }

    /// Kind of this node.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.data.head.kind
    }

    /// Length of the text, covered by this node.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        self.data.head.text_len
    }

    /// Children of this node.
    #[inline]
    pub fn children(&self) -> &[GreenElement] {
        &self.data.slice
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct ArcGreenNode {
    pub(super) raw: ThinArc<GreenNodeHead, GreenElement>,
}

impl ArcGreenNode {
    pub(super) fn from_thin(this: ThinArc<GreenNodeHead, GreenElement>) -> ArcGreenNode {
        ArcGreenNode { raw: this }
    }

    pub(super) fn from_raw(this: Arc<ThinData<GreenNodeHead, GreenElement>>) -> ArcGreenNode {
        Self::from_thin(this.into())
    }

    pub(super) fn from_fat(this: Arc<GreenNode>) -> ArcGreenNode {
        unsafe { ArcGreenNode::from_raw(mem::transmute(this)) }
    }

    pub(super) fn into_fat(this: ArcGreenNode) -> Arc<GreenNode> {
        let arc: Arc<ThinData<GreenNodeHead, GreenElement>> = this.raw.into();
        unsafe { mem::transmute(arc) }
    }
}

impl Deref for ArcGreenNode {
    type Target = GreenNode;
    fn deref(&self) -> &GreenNode {
        unsafe {
            &*((&*self.raw) as *const ThinData<GreenNodeHead, GreenElement> as *const GreenNode)
        }
    }
}

impl Borrow<GreenNode> for ArcGreenNode {
    fn borrow(&self) -> &GreenNode {
        &**self
    }
}

impl AsRef<GreenNode> for ArcGreenNode {
    fn as_ref(&self) -> &GreenNode {
        &**self
    }
}

impl ToOwned for GreenNode {
    type Owned = Arc<GreenNode>;

    fn to_owned(&self) -> Arc<GreenNode> {
        unsafe {
            // This is safe because all `GreenNode`s live behind an `Arc`.
            let arc = Arc::from_raw(self as *const GreenNode);
            // NB: The real owning `Arc` cannot be dropped here because it is borrowed.
            // Don't forget to increase the reference count!
            mem::forget(arc.clone());
            arc
        }
    }
}
