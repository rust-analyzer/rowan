use std::sync::Arc;

use super::*;
use crate::{cursor::SyntaxKind, NodeOrToken, TextUnit};
use std::{fmt, hash, mem, ptr};

/// An owned element in a green tree.
///
// FIXME: add some green tree docs here
pub struct GreenElement {
    /// This ptr is either `ArcGreenNode` or `Arc<GreenToken> | 1`.
    /// The low bit is available for a tag because
    /// these pointers are `usize`-aligned.
    raw: ptr::NonNull<()>,
}

unsafe impl Send for GreenElement {}
unsafe impl Sync for GreenElement {}

impl Drop for GreenElement {
    fn drop(&mut self) {
        unsafe {
            if self.is_node() {
                drop(ArcGreenNode::from_erased(self.raw.cast()))
            } else {
                let ptr = (self.raw.as_ptr() as usize & !1) as *const GreenToken;
                drop(Arc::from_raw(ptr))
            }
        }
    }
}

impl fmt::Debug for GreenElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match_element! { self => {
            el => el.fmt(f),
        }}
    }
}

impl Clone for GreenElement {
    fn clone(&self) -> GreenElement {
        match_element! { self => {
            el => el.to_owned().into(),
        }}
    }
}

impl Eq for GreenElement {}
impl PartialEq for GreenElement {
    fn eq(&self, other: &Self) -> bool {
        if self.is_node() {
            self.as_node() == other.as_node()
        } else {
            self.as_token() == other.as_token()
        }
    }
}

impl hash::Hash for GreenElement {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match_element! { self => {
            el => el.hash(state),
        }}
    }
}

impl From<Arc<GreenToken>> for GreenElement {
    #[inline]
    fn from(token: Arc<GreenToken>) -> GreenElement {
        let ptr = (Arc::into_raw(token) as usize | 1) as *mut ();
        unsafe { GreenElement { raw: ptr::NonNull::new_unchecked(ptr) } }
    }
}

impl From<GreenToken> for GreenElement {
    fn from(token: GreenToken) -> GreenElement {
        Arc::new(token).into()
    }
}

impl From<ArcGreenNode> for GreenElement {
    fn from(node: ArcGreenNode) -> GreenElement {
        GreenElement { raw: ArcGreenNode::into_raw(node).cast() }
    }
}

impl GreenElement {
    #[inline]
    pub fn is_node(&self) -> bool {
        self.raw.as_ptr() as usize & 1 == 0
    }

    #[inline]
    pub fn is_token(&self) -> bool {
        self.raw.as_ptr() as usize & 1 != 0
    }

    #[inline]
    pub fn as_node(&self) -> Option<&GreenNode> {
        if self.is_node() {
            Some(unsafe { &*ArcGreenNode::unerase(self.raw).as_ptr() })
        } else {
            None
        }
    }

    #[inline]
    pub fn unwrap_node(self) -> ArcGreenNode {
        if self.is_node() {
            unsafe {
                let ptr = ArcGreenNode::from_erased(self.raw);
                mem::forget(self);
                ptr
            }
        } else {
            panic!("called `unwrap_node` on a token")
        }
    }

    #[inline]
    pub fn as_token(&self) -> Option<&GreenToken> {
        if self.is_token() {
            let ptr = ((self.raw.as_ptr() as usize) & !1) as *const GreenToken;
            unsafe { Some(&*ptr) }
        } else {
            None
        }
    }

    #[inline]
    pub fn as_ref(&self) -> NodeOrToken<&GreenNode, &GreenToken> {
        match_element! { self => {
            Node(node) => NodeOrToken::Node(node),
            Token(token) => NodeOrToken::Token(token),
        }}
    }

    /// Returns kind of this element.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        match_element! { self => {
            el => el.kind(),
        }}
    }

    /// Returns length of the text covered by this element.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        match_element! { self => {
            el => el.text_len(),
        }}
    }
}
