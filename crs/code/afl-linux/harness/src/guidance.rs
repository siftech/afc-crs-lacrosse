use anyhow::{Context, Result};
use serde::Deserialize;
use std::{fs, ops::Range, path::Path};

/// A type that wraps up the guidance policy.
#[derive(Debug, Default, Deserialize)]
pub struct Guidance {
    interesting_ranges: Vec<Range<usize>>,
}

impl Guidance {
    /// Loads the guidance config from a file.
    pub fn load_from_file(path: &Path) -> Result<Guidance> {
        let bytes = fs::read(path).context("Failed to open guidance config file")?;
        serde_json::from_slice(&bytes).context("Failed to parse guidance config file")
    }

    /// Returns how much weight should be given to the given address.
    pub fn interestingness(&self, addr: usize) -> usize {
        let is_interesting = self
            .interesting_ranges
            .iter()
            .any(|range| range.contains(&addr));

        if is_interesting {
            0
        } else {
            10
        }
    }
}
