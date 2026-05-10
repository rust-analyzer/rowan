//! Regression test for a Stacked-Borrows violation triggered by the
//! `NodeCache` token map's rehash path under miri.
//!
//! Building enough distinct tokens forces hashbrown's `RawTable` to grow,
//! which calls the rehash closure -> `token_hash` -> `GreenTokenData::text`
//! -> `<HeaderSlice<H, [u8; 0]> as Deref>::deref`. Under Stacked Borrows,
//! the wider `&HeaderSlice<H, [u8]>` retag from a narrow `&HeaderSlice<H, [u8; 0]>`
//! input was rejected, producing UB.
//!
//! Run with: `MIRIFLAGS=-Zmiri-strict-provenance cargo +nightly miri test --test miri_node_cache_rehash`

use rowan::{GreenNodeBuilder, SyntaxKind};

#[test]
fn rehash_token_map_under_miri() {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind(0));
    // Push enough distinct tokens to force at least one hashbrown rehash.
    // Default capacity is small (~3); 200 distinct keys is plenty.
    for i in 0..200u32 {
        // Use 4-byte payload so the inline tail is non-trivial.
        let s = format!("t{:03}", i);
        builder.token(SyntaxKind(1), &s);
    }
    builder.finish_node();
    let _node = builder.finish();
    // Iterating the result also triggers the same SB violation through
    // `GreenNodeData::children()` -> `slice()` -> `HeaderSlice::deref`,
    // but that path is exercised by other tests; this test specifically
    // pins the rehash-during-token-insert path.
}
