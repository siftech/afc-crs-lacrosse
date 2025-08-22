use anyhow::{ensure, Result};
use contracts::{invariant, requires};
use std::{
    collections::BTreeMap,
    ffi::OsString,
    path::{Path, PathBuf},
};

/// A set of paths to use to compute rewrites.
#[derive(Debug)]
pub struct PathRewrites {
    paths: BTreeMap<PathBuf, PathBuf>,
}

impl PathRewrites {
    pub fn from_args(path_args: &[OsString]) -> Result<PathRewrites> {
        let mut paths = BTreeMap::new();
        for arg in path_args {
            let arg_bytes = arg.as_encoded_bytes();
            let (host, ctr) = if let Some(i) = arg_bytes.iter().position(|&b| b == b':') {
                // SAFETY: These bytes came from a valid OsString. Since we're guaranteed that the
                // encoding is a self-synchronizing superset of UTF-8, the colon has to have a code
                // unit boundary on both sides of it, so this must be legal.
                let (host, ctr) = unsafe {
                    (
                        OsString::from_encoded_bytes_unchecked(arg_bytes[..i].to_vec()),
                        OsString::from_encoded_bytes_unchecked(arg_bytes[i + 1..].to_vec()),
                    )
                };
                let host = PathBuf::from(host);
                let ctr = PathBuf::from(ctr);
                (
                    host.canonicalize().unwrap_or(host),
                    ctr.canonicalize().unwrap_or(ctr),
                )
            } else {
                let arg = PathBuf::from(arg);
                let arg = arg.canonicalize().unwrap_or(arg);
                (arg.clone(), arg)
            };
            ensure!(
                host.is_absolute(),
                "Failed to canonicalize {host:?}; try giving it as an absolute path"
            );
            ensure!(
                ctr.is_absolute(),
                "Failed to canonicalize {ctr:?}; try giving it as an absolute path"
            );
            paths.insert(host, ctr);
        }
        Ok(PathRewrites { paths })
    }

    /// Rewrites a path.
    #[requires(path.is_absolute())]
    pub fn rewrite(&self, path: PathBuf) -> PathBuf {
        let mut path = path;
        'outer: loop {
            for (host, ctr) in &self.paths {
                let ancestry = Ancestry::new(host, &path);
                // If the host path is strictly an ancestor of the path...
                if ancestry.lhs.as_os_str().is_empty() {
                    // Rewrite the path.
                    path = ctr.join(ancestry.rhs);
                    // Try replacing paths from scratch again.
                    continue 'outer;
                }
            }
            break path;
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct Ancestry<'a> {
    common_prefix: PathBuf,
    lhs: &'a Path,
    rhs: &'a Path,
}

#[invariant(self.common_prefix.is_absolute())]
#[invariant(self.lhs.is_relative())]
#[invariant(self.rhs.is_relative())]
impl<'a> Ancestry<'a> {
    /// Computes the common ancestor of the two paths.
    #[requires(lhs.is_absolute())]
    #[requires(rhs.is_absolute())]
    pub fn new(lhs: &'a Path, rhs: &'a Path) -> Ancestry<'a> {
        let mut lhs_components = lhs.components();
        let mut rhs_components = rhs.components();

        let mut common_prefix = PathBuf::new();
        let (lhs, rhs) = loop {
            // Clone the components, so we can "peek" them. This way, if we find a mismatch,
            // `Components::as_path()` returns the paths from the mismatch.
            match (lhs_components.clone().next(), rhs_components.clone().next()) {
                (Some(lhs_component), Some(rhs_component)) if lhs_component == rhs_component => {
                    common_prefix.push(lhs_component);

                    // Advance the "real" iterators.
                    lhs_components.next();
                    rhs_components.next();
                }
                _ => break (lhs_components.as_path(), rhs_components.as_path()),
            }
        };
        Ancestry {
            common_prefix,
            lhs,
            rhs,
        }
    }
}

#[test]
fn rewrites_test() {
    let rewrites = PathRewrites::from_args(&[
        OsString::from("/nix"),
        OsString::from("/home/dev/src/gaf:/code"),
    ])
    .unwrap();

    assert_eq!(
        rewrites.rewrite(PathBuf::from("/home/dev/src/gaf/src/main.rs")),
        PathBuf::from("/code/src/main.rs")
    );
}

#[test]
fn ancestry_test() {
    assert_eq!(
        Ancestry::new(Path::new("/a/foo/bar"), Path::new("/a/foo/baz/asdf")),
        Ancestry {
            common_prefix: PathBuf::from("/a/foo"),
            lhs: Path::new("bar"),
            rhs: Path::new("baz/asdf"),
        }
    );

    assert_eq!(
        Ancestry::new(Path::new("/b/foo"), Path::new("/b/foo/bar")),
        Ancestry {
            common_prefix: PathBuf::from("/b/foo"),
            lhs: Path::new(""),
            rhs: Path::new("bar"),
        }
    );
}
