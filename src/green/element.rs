use {
    crate::{
        GreenNode, GreenToken,
        NodeOrToken,
    },
    erasable::{ErasablePtr, ErasedPtr},
    rc_borrow::ArcBorrow,
    rc_box::ArcBox,
    std::{
        fmt, hash,
        mem::{self, ManuallyDrop},
        ptr,
        sync::Arc,
    },
    text_unit::TextUnit,
};

/// An atomically reference counted pointer to either [`GreenNode`] or [`GreenToken`].
///
/// This is stored as a single erased pointer with the tag stored in the low
/// alignment bit: `0` for `GreenNode` and `1` for `GreenToken`.
pub(crate) struct GreenElement {
    raw: ErasedPtr,
}

unsafe fn erase_lt<T: ?Sized>(this: ArcBorrow<'_, T>) -> ArcBorrow<'static, T> {
    mem::transmute(this)
}

impl GreenElement {
    pub(crate) fn is_node(&self) -> bool {
        self.raw.as_ptr() as usize & 1 == 0
    }

    pub(crate) fn as_node(&self) -> Option<ArcBorrow<'_, GreenNode>> {
        if self.is_node() {
            unsafe {
                let arc: ManuallyDrop<Arc<GreenNode>> =
                    ManuallyDrop::new(ErasablePtr::unerase(self.raw));
                Some(erase_lt(ArcBorrow::from(&*arc)))
            }
        } else {
            None
        }
    }

    pub(crate) fn into_node(self) -> Option<Arc<GreenNode>> {
        if self.is_node() {
            unsafe {
                let arc: Arc<GreenNode> = ErasablePtr::unerase(ManuallyDrop::new(self).raw);
                Some(arc)
            }
        } else {
            None
        }
    }

    pub(crate) fn is_token(&self) -> bool {
        self.raw.as_ptr() as usize & 1 == 1
    }

    pub(crate) fn as_token(&self) -> Option<ArcBorrow<'_, GreenToken>> {
        if self.is_token() {
            unsafe {
                let raw = self.raw.as_ptr() as usize & !1;
                let raw = ptr::NonNull::new_unchecked(raw as *mut _);
                let arc: &Arc<GreenToken> = &ManuallyDrop::new(ErasablePtr::unerase(raw));
                Some(erase_lt(ArcBorrow::from(arc)))
            }
        } else {
            None
        }
    }

    pub(crate) fn into_token(self) -> Option<Arc<GreenToken>> {
        if self.is_token() {
            unsafe {
                let raw = ManuallyDrop::new(self).raw.as_ptr() as usize & !1;
                let raw = ptr::NonNull::new_unchecked(raw as *mut _);
                Some(ErasablePtr::unerase(raw))
            }
        } else {
            None
        }
    }
}

impl Drop for GreenElement {
    fn drop(&mut self) {
        if self.is_node() {
            unsafe { ptr::read(self).into_node().unwrap() };
        } else {
            unsafe { ptr::read(self).into_token().unwrap() };
        }
    }
}

impl fmt::Debug for GreenElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        NodeOrToken::<&_, &_>::from(self).fmt(f)
    }
}

impl Clone for GreenElement {
    fn clone(&self) -> Self {
        if self.is_node() {
            ArcBorrow::upgrade(self.as_node().unwrap()).into()
        } else {
            ArcBorrow::upgrade(self.as_token().unwrap()).into()
        }
    }
}

impl Eq for GreenElement {}
impl PartialEq for GreenElement {
    fn eq(&self, other: &Self) -> bool {
        self.as_node() == other.as_node() && self.as_token() == other.as_token()
    }
}

impl hash::Hash for GreenElement {
    fn hash<H>(&self, state: &mut H)
    where
        H: hash::Hasher,
    {
        if self.is_node() {
            self.as_node().unwrap().hash(state)
        } else {
            self.as_token().unwrap().hash(state)
        }
    }
}

impl From<Arc<GreenNode>> for GreenElement {
    fn from(node: Arc<GreenNode>) -> Self {
        GreenElement { raw: ErasablePtr::erase(node) }
    }
}

impl From<Arc<GreenToken>> for GreenElement {
    fn from(node: Arc<GreenToken>) -> Self {
        let ptr = ErasablePtr::erase(node).as_ptr() as usize | 1;
        unsafe { GreenElement { raw: ptr::NonNull::new_unchecked(ptr as *mut _) } }
    }
}

impl From<ArcBox<GreenNode>> for GreenElement {
    fn from(node: ArcBox<GreenNode>) -> Self {
        GreenElement::from(Arc::<GreenNode>::from(node))
    }
}

impl From<ArcBox<GreenToken>> for GreenElement {
    fn from(token: ArcBox<GreenToken>) -> Self {
        GreenElement::from(Arc::<GreenToken>::from(token))
    }
}

impl From<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>> for GreenElement {
    fn from(el: NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>) -> Self {
        el.map(Into::into, Into::into).flatten()
    }
}

impl From<GreenElement> for NodeOrToken<Arc<GreenNode>, Arc<GreenToken>> {
    fn from(el: GreenElement) -> Self {
        if el.is_node() {
            el.into_node().unwrap().into()
        } else {
            el.into_token().unwrap().into()
        }
    }
}

impl<'a> From<&'a GreenElement>
    for NodeOrToken<ArcBorrow<'a, GreenNode>, ArcBorrow<'a, GreenToken>>
{
    fn from(el: &'a GreenElement) -> Self {
        if el.is_node() {
            NodeOrToken::Node(el.as_node().unwrap())
        } else {
            NodeOrToken::Token(el.as_token().unwrap())
        }
    }
}

impl<'a> From<&'a GreenElement> for NodeOrToken<&'a GreenNode, &'a GreenToken> {
    fn from(el: &'a GreenElement) -> Self {
        if el.is_node() {
            NodeOrToken::Node(ArcBorrow::downgrade(el.as_node().unwrap()))
        } else {
            NodeOrToken::Token(ArcBorrow::downgrade(el.as_token().unwrap()))
        }
    }
}

impl From<Arc<GreenNode>> for NodeOrToken<Arc<GreenNode>, Arc<GreenToken>> {
    fn from(node: Arc<GreenNode>) -> Self {
        NodeOrToken::Node(node)
    }
}

impl From<Arc<GreenToken>> for NodeOrToken<Arc<GreenNode>, Arc<GreenToken>> {
    fn from(token: Arc<GreenToken>) -> Self {
        NodeOrToken::Token(token)
    }
}

impl<'a> From<ArcBorrow<'a, GreenNode>>
    for NodeOrToken<ArcBorrow<'a, GreenNode>, ArcBorrow<'a, GreenToken>>
{
    fn from(node: ArcBorrow<'a, GreenNode>) -> Self {
        NodeOrToken::Node(node)
    }
}

impl<'a> From<ArcBorrow<'a, GreenToken>>
    for NodeOrToken<ArcBorrow<'a, GreenNode>, ArcBorrow<'a, GreenToken>>
{
    fn from(token: ArcBorrow<'a, GreenToken>) -> Self {
        NodeOrToken::Token(token)
    }
}

impl<'a> From<&'a GreenNode> for NodeOrToken<&'a GreenNode, &'a GreenToken> {
    fn from(node: &'a GreenNode) -> Self {
        NodeOrToken::Node(node)
    }
}

impl<'a> From<&'a GreenToken> for NodeOrToken<&'a GreenNode, &'a GreenToken> {
    fn from(token: &'a GreenToken) -> Self {
        NodeOrToken::Token(token)
    }
}

impl NodeOrToken<&GreenNode, &GreenToken> {
    /// The length of the text of this element.
    pub fn text_len(self) -> TextUnit {
        self.map(|node| node.text_len, |token| TextUnit::of_str(&token.text)).flatten()
    }
}

impl NodeOrToken<Arc<GreenNode>, Arc<GreenToken>> {
    /// The length of the text of this element.
    pub fn text_len(&self) -> TextUnit {
        self.as_deref().text_len()
    }
}
