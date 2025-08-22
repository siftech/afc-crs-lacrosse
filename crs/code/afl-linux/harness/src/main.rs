use crate::{afl::fuzz_closure, guidance::Guidance, util::app};
use anyhow::{Context, Result};
use clap::Parser;
use std::path::PathBuf;

mod afl;
mod guidance;
mod harnesses;
mod kcov;
mod kmsg;
mod prng;
mod shm;
mod tap;
mod util;

fn main() -> Result<()> {
    app(|args: Args| {
        // Load the guidance config.
        let guidance = if let Some(guidance_config) = &args.guidance_config {
            Guidance::load_from_file(guidance_config).context("Failed to load guidance config")?
        } else {
            Guidance::default()
        };

        // Create the fuzzing harness.
        let mut harness = args
            .harness
            .make_harness()
            .context("Failed to make harness")?;

        // Run fuzzing jobs.
        fuzz_closure(&guidance, |mut coverage, buf| {
            harness
                .fuzz(buf, |entry| coverage.trace(entry))
                .expect("Harness failed")
        })
    })
}

/// The various harnesses for fuzzing the kernel with, in one binary. This gets run inside the
/// virtual machine, so that it can access KCOV information.
#[derive(Debug, Parser)]
struct Args {
    /// The path to the guidance config file.
    #[arg(long)]
    guidance_config: Option<PathBuf>,

    #[command(subcommand)]
    harness: harnesses::HarnessKind,
}
