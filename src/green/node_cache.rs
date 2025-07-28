use hashbrown::{HashTable, hash_table::Entry};
use rustc_hash::FxHasher;
use std::hash::{Hash, Hasher};

use crate::{
    GreenNode, GreenNodeData, GreenToken, GreenTokenData, NodeOrToken, SyntaxKind,
    green::GreenElementRef,
};

use super::element::GreenElement;

/// Interner for GreenTokens and GreenNodes
// XXX: the impl is a bit tricky. As usual when writing interners, we want to
// store all values in one HashSet.
//
// However, hashing trees is fun: hash of the tree is recursively defined. We
// maintain an invariant -- if the tree is interned, then all of its children
// are interned as well.
//
// That means that computing the hash naively is wasteful -- we just *know*
// hashes of children, and we can re-use those.
//
// So here we use `HashTable` of hashbrown and provide the hashes manually,
// instead of going via a `Hash` impl. Our manual `Hash` and the
// `#[derive(Hash)]` are actually different! At some point we had a fun bug,
// where we accidentally mixed the two hashes, which made the cache much less
// efficient.
//
// `HashTable` prevents us to accidentally use the wrong hash!
#[derive(Default, Debug)]
pub struct NodeCache {
    nodes: HashTable<GreenNode>,
    tokens: HashTable<GreenToken>,
}

fn token_hash(token: &GreenTokenData) -> u64 {
    let mut h = FxHasher::default();
    token.kind().hash(&mut h);
    token.text().hash(&mut h);
    h.finish()
}

fn node_hash(node: &GreenNodeData) -> u64 {
    let mut h = FxHasher::default();
    node.kind().hash(&mut h);
    for child in node.children() {
        match child {
            NodeOrToken::Node(it) => node_hash(it),
            NodeOrToken::Token(it) => token_hash(it),
        }
        .hash(&mut h)
    }
    h.finish()
}

fn element_id(elem: GreenElementRef<'_>) -> *const () {
    match elem {
        NodeOrToken::Node(it) => it as *const GreenNodeData as *const (),
        NodeOrToken::Token(it) => it as *const GreenTokenData as *const (),
    }
}

impl NodeCache {
    pub(crate) fn node(
        &mut self,
        kind: SyntaxKind,
        children: &mut Vec<(u64, GreenElement)>,
        first_child: usize,
    ) -> (u64, GreenNode) {
        let build_node = move |children: &mut Vec<(u64, GreenElement)>| {
            GreenNode::new(kind, children.drain(first_child..).map(|(_, it)| it))
        };

        let children_ref = &children[first_child..];
        if children_ref.len() > 3 {
            let node = build_node(children);
            return (0, node);
        }

        let hash = {
            let mut h = FxHasher::default();
            kind.hash(&mut h);
            for &(hash, _) in children_ref {
                if hash == 0 {
                    let node = build_node(children);
                    return (0, node);
                }
                hash.hash(&mut h);
            }
            h.finish()
        };

        // Green nodes are fully immutable, so it's ok to deduplicate them.
        // This is the same optimization that Roslyn does
        // https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees
        //
        // For example, all `#[inline]` in this file share the same green node!
        // For `libsyntax/parse/parser.rs`, measurements show that deduping saves
        // 17% of the memory for green nodes!
        let entry = self.nodes.entry(
            hash,
            |node| {
                node.kind() == kind && node.children().len() == children_ref.len() && {
                    let lhs = node.children();
                    let rhs = children_ref.iter().map(|(_, it)| it.as_deref());

                    let lhs = lhs.map(element_id);
                    let rhs = rhs.map(element_id);

                    lhs.eq(rhs)
                }
            },
            |node| node_hash(node),
        );

        let node = match entry {
            Entry::Occupied(entry) => {
                drop(children.drain(first_child..));
                entry.get().clone()
            }
            Entry::Vacant(entry) => {
                let node = build_node(children);
                entry.insert(node.clone());
                node
            }
        };

        (hash, node)
    }

    pub(crate) fn token(&mut self, kind: SyntaxKind, text: &str) -> (u64, GreenToken) {
        let hash = {
            let mut h = FxHasher::default();
            kind.hash(&mut h);
            text.hash(&mut h);
            h.finish()
        };

        let entry = self.tokens.entry(
            hash,
            |token| token.kind() == kind && token.text() == text,
            |token| token_hash(token),
        );

        let token = match entry {
            Entry::Occupied(entry) => entry.get().clone(),
            Entry::Vacant(entry) => {
                let token = GreenToken::new(kind, text);
                entry.insert(token.clone());
                token
            }
        };

        (hash, token)
    }
}
