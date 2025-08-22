use crate::{afl::FuzzStatus, kcov::KCov, kmsg::Dmesg, tap::Tap};
use anyhow::{bail, Context, Result};
use clap::Parser;
use std::process::Command;

#[derive(Clone, Debug, Parser)]
pub struct TipcArgs;

#[derive(Debug)]
pub struct TipcHarness {
    dmesg: Dmesg,
    kcov: KCov,
    tap: Tap,
}

impl TipcHarness {
    pub fn new(_args: TipcArgs) -> Result<TipcHarness> {
        let tap = Tap::new().context("Failed to create tap device for TIPC harness")?;

        let status = Command::new("tipc")
            .arg("bearer")
            .arg("enable")
            .arg("media")
            .arg("eth")
            .arg("dev")
            .arg(tap.name())
            .status()
            .context("Failed to run the tipc CLI")?;
        if !status.success() {
            bail!("The tipc CLI exited non-zero when trying to add our tap device as a bearer");
        }

        let status = Command::new("ip")
            .arg("link")
            .arg("set")
            .arg("up")
            .arg(tap.name())
            .status()
            .context("Failed to run the ip CLI")?;
        if !status.success() {
            bail!("The ip CLI exited non-zero when trying to up our tap device");
        }

        let dmesg = Dmesg::new().context("Failed to open dmesg")?;
        let kcov = KCov::new(1 << 16).context("Failed to set up KCov")?;

        Ok(TipcHarness { dmesg, kcov, tap })
    }

    pub fn fuzz(&mut self, bytes: &[u8], mut trace: impl FnMut(usize)) -> Result<FuzzStatus> {
        self.kcov.with(|| self.tap.write(bytes))?;

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
