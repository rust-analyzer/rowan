use xshell::{Shell, cmd};

#[test]
fn test_formatting() {
    let sh = Shell::new().unwrap();
    cmd!(sh, "cargo fmt --all -- --check").run().unwrap()
}
