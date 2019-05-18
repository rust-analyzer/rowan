/// Internal macro - Don't use! May change at any time disregarding
/// semver versioning.
#[macro_export]
#[doc(hidden)]
macro_rules! inner_syntax {
    (@const start $init:expr; $ident:ident $($remaining:tt)*) => {
        pub const $ident: $crate::SyntaxKind = $crate::SyntaxKind($init);
        inner_syntax!(@const start $init+1; $($remaining)*);
    };
    (@const start $init:expr;) => {};
}

/// Generate SyntaxKind constants with automatically defined unique
/// values and SyntaxResolver implementations. This is the recommended
/// way to create constants for different syntaxes.
/// ```rust
/// use rowan::syntax;
/// syntax! {
///     start 0;
///     resolver TokenResolver;
///     TOKEN_LEFT_PAREN
///     TOKEN_RIGHT_PAREN
///     // ...etc
/// }
///
/// assert_eq!(TOKEN_LEFT_PAREN.0, 0);
/// assert_eq!(TOKEN_RIGHT_PAREN.0, 1);
/// assert_eq!(TOKEN_LEFT_PAREN.name::<TokenResolver>(), "TOKEN_LEFT_PAREN");
/// assert_eq!(TOKEN_RIGHT_PAREN.name::<TokenResolver>(), "TOKEN_RIGHT_PAREN");
/// ```
#[macro_export]
macro_rules! syntax {
    (start $init:expr; resolver $resolver_name:ident; $($ident:ident)*) => {
        use $crate::inner_syntax;
        inner_syntax!(@const start $init; $($ident)*);

        struct $resolver_name;
        impl $crate::SyntaxResolver for $resolver_name {
            fn name(val: $crate::SyntaxKind) -> Option<&'static str> {
                match val {
                    // Depending on stringify! is bad, but how else can I
                    // get the identifier name?
                    $($ident => Some(stringify!($ident)),)*
                    _ => None
                }
            }
        }
    };
}
