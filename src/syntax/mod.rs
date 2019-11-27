//! The "syntax tree" is a view of the green tree with parent pointers and text offsets.

mod text;
pub mod untyped;

mod free_list;

pub use self::text::Text;
use free_list::FreeList;
