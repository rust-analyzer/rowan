use std::{
    env,
    path::PathBuf,
    sync::atomic::{AtomicBool, Ordering},
    time::{Duration, Instant},
};

use anyhow::anyhow;
use xshell::{Shell, cmd};
pub type Result<T> = anyhow::Result<T>;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("error: {}", err);
        std::process::exit(1)
    }
}

pub struct Section {
    name: &'static str,
    start: Instant,
}

pub struct CargoToml {
    path: PathBuf,
    contents: String,
}

impl CargoToml {
    pub fn version(&self) -> Result<&str> {
        self.get("version")
    }

    fn get(&self, field: &str) -> Result<&str> {
        for line in self.contents.lines() {
            let words = line.split_ascii_whitespace().collect::<Vec<_>>();
            match words.as_slice() {
                [n, "=", v, ..] if n.trim() == field => {
                    assert!(v.starts_with('"') && v.ends_with('"'));
                    return Ok(&v[1..v.len() - 1]);
                }
                _ => (),
            }
        }
        Err(anyhow!("can't find `{}` in {}", field, self.path.display()))?
    }

    pub fn publish(&self, sh: &mut Shell) -> Result<()> {
        let token = env::var("CRATES_IO_TOKEN").unwrap_or("no token".to_string());
        let dry_run = dry_run();
        cmd!(sh, "cargo publish --token {token} {dry_run...}").run()?;
        Ok(())
    }

    pub fn publish_all(&self, dirs: &[&str], sh: &mut Shell) -> Result<()> {
        let token = env::var("CRATES_IO_TOKEN").unwrap_or("no token".to_string());
        if dry_run().is_none() {
            for &dir in dirs {
                for _ in 0..20 {
                    std::thread::sleep(Duration::from_secs(10));
                    if cmd!(
                        sh,
                        "cargo publish --manifest-path {dir}'/Cargo.toml' --token {token} --dry-run"
                    )
                    .run()
                    .is_ok()
                    {
                        break;
                    }
                }
                cmd!(sh, "cargo publish --manifest-path {dir}'/Cargo.toml' --token {token}")
                    .run()?;
            }
        }
        Ok(())
    }
}

fn dry_run() -> Option<&'static str> {
    let dry_run = DRY_RUN.load(Ordering::Relaxed);
    if dry_run { Some("--dry-run") } else { None }
}

pub fn section(name: &'static str) -> Section {
    Section::new(name)
}

pub fn cargo_toml() -> Result<CargoToml> {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR")?;
    let path = PathBuf::from(manifest_dir).join("Cargo.toml");
    let contents = std::fs::read_to_string(&path)?;
    Ok(CargoToml { path, contents })
}

static DRY_RUN: AtomicBool = AtomicBool::new(false);
pub fn set_dry_run(yes: bool) {
    DRY_RUN.store(yes, Ordering::Relaxed)
}

fn try_main() -> Result<()> {
    let mut sh = Shell::new()?;
    let subcommand = std::env::args().nth(1);
    match subcommand {
        Some(it) if it == "ci" => (),
        _ => {
            print_usage();
            Err(anyhow!("invalid arguments"))?
        }
    }

    let cargo_toml = cargo_toml()?;

    {
        let _s = section("TEST");
        for &release in &[None, Some("--release")] {
            for &tracing in &[&[][..], &["--features", "tracing"]] {
                for &force in &[&[][..], &["--features", "force"]] {
                    cmd!(
                        sh,
                        "cargo test {release...} {tracing...} {force...} --workspace -- --nocapture"
                    )
                    .run()?;
                }
            }
        }
    }

    let version = cargo_toml.version()?;
    let tag = format!("v{}", version);

    let dry_run = env::var("CI").is_err()
        || git::has_tag(&tag, &mut sh)?
        || git::current_branch(&mut sh)? != "master";
    set_dry_run(dry_run);

    {
        let _s = section("PUBLISH");
        cargo_toml.publish(&mut sh)?;
        git::tag(&tag, &mut sh)?;
        git::push_tags(&mut sh)?;
    }
    Ok(())
}

pub mod git {
    use xshell::{Shell, cmd};

    use super::{Result, dry_run};

    pub fn current_branch(sh: &mut Shell) -> Result<String> {
        let res = cmd!(sh, "git branch --show-current").read()?;
        Ok(res)
    }

    pub fn tag_list(sh: &mut Shell) -> Result<Vec<String>> {
        let tags = cmd!(sh, "git tag --list").read()?;
        let res = tags.lines().map(|it| it.trim().to_string()).collect();
        Ok(res)
    }

    pub fn has_tag(tag: &str, sh: &mut Shell) -> Result<bool> {
        let res = tag_list(sh)?.iter().any(|it| it == tag);
        Ok(res)
    }

    pub fn tag(tag: &str, sh: &mut Shell) -> Result<()> {
        if dry_run().is_some() {
            return Ok(());
        }
        cmd!(sh, "git tag {tag}").run()?;
        Ok(())
    }

    pub fn push_tags(sh: &mut Shell) -> Result<()> {
        // `git push --tags --dry-run` exists, but it will fail with permissions
        // error for forks.
        if dry_run().is_some() {
            return Ok(());
        }

        cmd!(sh, "git push --tags").run()?;
        Ok(())
    }
}

fn print_usage() {
    eprintln!(
        "\
Usage: cargo run -p xtask <SUBCOMMAND>

SUBCOMMANDS:
    ci
"
    )
}

impl Section {
    fn new(name: &'static str) -> Section {
        println!("::group::{}", name);
        let start = Instant::now();
        Section { name, start }
    }
}

impl Drop for Section {
    fn drop(&mut self) {
        eprintln!("{}: {:.2?}", self.name, self.start.elapsed());
        println!("::endgroup::");
    }
}
