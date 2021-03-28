use std::{ops, ptr};

use crate::{green::GreenTokenData, GreenToken};

/// This is basically `Arc<str>`.
/// But 2 words in size like `&str` and has no indirection.
// Think of it like GreenToken has been transformed to contain only &str.
// Then, using a field offset, the GreenToken is obtained back to drop.
// Invariant: Not Copy.
// TODO: impl Clone, From<_>, PartialEq<_>, etc
#[repr(transparent)]
pub struct TokenText {
    raw: ptr::NonNull<str>,
}

impl TokenText {
    pub fn new(token: &GreenTokenData) -> TokenText {
        let green = token.to_owned();
        let slice_ptr = green.text() as *const str as *mut str;
        let _leaked = GreenToken::into_raw(green);
        unsafe { Self { raw: ptr::NonNull::new_unchecked(slice_ptr) } }
    }
}

impl Drop for TokenText {
    fn drop(&mut self) {
        unsafe {
            let _ = GreenToken::from_raw_token_text(self.raw);
        }
    }
}

impl ops::Deref for TokenText {
    type Target = str;

    fn deref(&self) -> &str {
        unsafe { self.raw.as_ref() }
    }
}

#[cfg(test)]
mod tests {
    use crate::SyntaxKind;

    use super::*;

    #[test]
    fn smoke() {
        let tok = GreenToken::new(SyntaxKind(u16::MAX), "foo");
        let raw_str = tok.text() as *const str;
        assert_eq!(tok.strong_count(), 1);

        let token_text = TokenText::new(&tok);
        assert_eq!(token_text.raw.as_ptr() as *const str, raw_str);
        assert_eq!(&*token_text, "foo");
        assert_eq!(tok.strong_count(), 2);
        assert_eq!(tok.kind().0, u16::MAX);

        drop(token_text);
        assert_eq!(tok.strong_count(), 1);
        assert_eq!(tok.kind().0, u16::MAX);
        assert_eq!(tok.text(), "foo");
    }
}
