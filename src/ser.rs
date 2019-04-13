use crate::*;
use serde::{
    de::{Deserializer, Error, MapAccess, Visitor},
    ser::{SerializeMap, Serializer},
    Deserialize, Serialize,
};
use std::{fmt, marker::PhantomData};

///////////////////////////////////////////// ~ser~~~ /////////////////////////////////////////////

#[derive(Serialize)]
#[serde(untagged)]
#[serde(bound(serialize = "T::Kind: Serialize"))]
enum NodeContentSer<'a, T: Types> {
    Leaf(&'a str),
    Branch(&'a [GreenNode<T>]),
}

impl<T: Types> Serialize for GreenNode<T>
where
    T::Kind: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        if let Some(text) = self.leaf_text() {
            map.serialize_entry(&self.kind(), text.as_str())?;
        } else {
            map.serialize_entry(&self.kind(), &NodeContentSer::Branch(self.children()))?;
        }
        map.end()
    }
}

///////////////////////////////////////////// ~~~~de~ /////////////////////////////////////////////

#[derive(Deserialize)]
#[serde(untagged)]
#[serde(bound(deserialize = "T::Kind: Deserialize<'de>"))]
enum NodeContentDe<'a, T: Types> {
    Leaf(&'a str),
    Branch(Vec<GreenNode<T>>),
}

struct NodeVisitor<T: Types>(PhantomData<GreenNode<T>>);

impl<'de, T: Types> Visitor<'de> for NodeVisitor<T>
where
    T::Kind: Deserialize<'de>,
{
    type Value = GreenNode<T>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a singleton map from the type kind to node content")
    }

    fn visit_map<A>(self, mut map: A) -> Result<GreenNode<T>, A::Error>
    where
        A: MapAccess<'de>,
    {
        let kind: T::Kind = map
            .next_key()?
            .ok_or_else(|| A::Error::custom("unexpected empty map"))?;
        let value: NodeContentDe<T> = map.next_value()?;
        Ok(match value {
            NodeContentDe::Leaf(text) => GreenNode::new_leaf(kind, text.into()),
            NodeContentDe::Branch(children) => {
                GreenNode::new_branch(kind, children.into_boxed_slice())
            }
        })
    }
}

impl<'de, T: Types> Deserialize<'de> for GreenNode<T>
where
    T::Kind: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(NodeVisitor(PhantomData))
    }
}
