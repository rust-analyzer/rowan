use serde::ser::{Serialize, SerializeMap, SerializeSeq, Serializer};

use crate::{
    api::{Language, SyntaxNode, SyntaxToken},
    NodeOrToken,
};

impl<L: Language> Serialize for SyntaxNode<L> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_map(Some(3))?;
        //FIXME: use https://github.com/serde-rs/serde/issues/1316 to avoid allocation.
        state.serialize_entry("kind", format!("{:?}", self.kind()).as_str())?;
        state.serialize_entry("text_range", &self.text_range())?;
        state.serialize_entry("children", &Children(self))?;
        state.end()
    }
}

impl<L: Language> Serialize for SyntaxToken<L> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_map(Some(3))?;
        //FIXME: use https://github.com/serde-rs/serde/issues/1316 to avoid allocation.
        state.serialize_entry("kind", format!("{:?}", self.kind()).as_str())?;
        state.serialize_entry("text_range", &self.text_range())?;
        state.serialize_entry("text", &self.text().as_str())?;
        state.end()
    }
}

struct Children<T>(T);

impl<L: Language> Serialize for Children<&'_ SyntaxNode<L>> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_seq(None)?;
        self.0.children_with_tokens().try_for_each(|element| match element {
            NodeOrToken::Node(it) => state.serialize_element(&it),
            NodeOrToken::Token(it) => state.serialize_element(&it),
        })?;
        state.end()
    }
}
