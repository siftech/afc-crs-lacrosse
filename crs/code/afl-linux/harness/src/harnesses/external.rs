use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use std::{
    fs::{self, File},
    io::{self, Write as _},
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    process::{self, Command},
};

use crate::{afl::FuzzStatus, kcov::KCov, kmsg::Dmesg};

#[derive(Clone, Debug, Parser)]
pub struct ExternalArgs {
    /// The path to the harness to run.
    harness: PathBuf,

    /// The directory to store harness inputs in.
    ///
    /// At least one of the harnesses uses stat() to determine how many bytes to read, so we can't
    /// just pass stdin as the file to read from...
    #[clap(long, default_value = "/dev/shm")]
    tmpdir: PathBuf,
}

#[derive(Debug)]
pub struct ExternalHarness {
    harness: PathBuf,
    tmpdir: PathBuf,

    dmesg: Dmesg,
    kcov: KCov,
}

impl ExternalHarness {
    pub fn new(args: ExternalArgs) -> Result<ExternalHarness> {
        fs::metadata(&args.harness).with_context(|| {
            anyhow!(
                "Could not stat the given fuzzer harness ({:?})",
                args.harness
            )
        })?;
        fs::metadata(&args.tmpdir).with_context(|| {
            anyhow!(
                "Could not stat the given fuzzer input tmpdir ({:?})",
                args.tmpdir
            )
        })?;

        let dmesg = Dmesg::new().context("Failed to open dmesg")?;
        let kcov = KCov::new(1 << 16).context("Failed to set up KCov")?;

        Ok(ExternalHarness {
            harness: args.harness,
            tmpdir: args.tmpdir,

            dmesg,
            kcov,
        })
    }

    pub fn fuzz(&mut self, bytes: &[u8], mut trace: impl FnMut(usize)) -> Result<FuzzStatus> {
        self.kcov.with(|| {
            // Create the fuzzer harness input file.
            let mut input_file = mktemp_in(&self.tmpdir)
                .context("Failed to create temporary file for fuzzer input")?;
            input_file
                .write_all(bytes)
                .context("Failed to write to temporary file for fuzzer input")?;

            // Run the harness.
            let status = Command::new(&self.harness)
                .arg(input_file.path())
                .status()
                .context("Failed to run the external harness")?;
            if !status.success() {
                bail!("The external harness exited with {status:?}")
            }

            Ok(())
        })?;

        for entry in self.kcov.iter().copied() {
            trace(entry);
        }

        let mut status = FuzzStatus::Ok;
        while let Some(entry) = self.dmesg.read().context("Failed to read from dmesg")? {
            if entry.contains("Call Trace:") || entry.contains("Code:") {
                status = FuzzStatus::Crash;
            }
        }
        Ok(status)
    }
}

fn mktemp_in(dir: &Path) -> Result<TmpFile> {
    let pid = process::id();
    let mut i = 0;
    loop {
        let path = dir.join(format!("fuzz-in.{pid}.{i}.tmp"));
        match File::options().write(true).create_new(true).open(&path) {
            Ok(file) => return Ok(TmpFile(path, file)),
            Err(err) if err.kind() == io::ErrorKind::AlreadyExists => {
                i += 1;
                continue;
            }
            Err(err) => return Err(err).with_context(|| anyhow!("Failed to create {path:?}")),
        }
    }
}

/// A wrapper around `std::fs::File` that unlinks the file on `Drop`.
struct TmpFile(PathBuf, File);

impl TmpFile {
    pub fn path(&self) -> &Path {
        &self.0
    }
}

impl Deref for TmpFile {
    type Target = File;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl DerefMut for TmpFile {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl Drop for TmpFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(&self.0);
    }
}
