use crate::afl::FuzzStatus;
use anyhow::Result;
use clap::Parser;

#[derive(Clone, Debug, Parser)]
pub struct TestGuidanceArgs;

/// This harness simulates fuzzing a control-flow graph that is a 32-deep tree of `if`s. There's a
/// crash at the branch for 0x53494654.
#[derive(Debug)]
pub struct TestGuidanceHarness {}

impl TestGuidanceHarness {
    pub fn new(_args: TestGuidanceArgs) -> Result<TestGuidanceHarness> {
        Ok(TestGuidanceHarness {})
    }

    pub fn fuzz(&mut self, bytes: &[u8], mut trace: impl FnMut(usize)) -> Result<FuzzStatus> {
        let mut spot: u32 = 0;
        let bytes = bytes.iter().copied().take(2);
        for bit in bit_iter(bytes) {
            trace(spot as usize);
            spot <<= 1;
            if bit {
                spot |= 1;
            }
            if spot == 0x1234 {
                return Ok(FuzzStatus::Crash);
            }
        }
        Ok(FuzzStatus::Ok)
    }
}

fn bit_iter(bytes: impl Iterator<Item = u8>) -> impl Iterator<Item = bool> {
    bytes.flat_map(|byte| (0..8).map(move |i| (byte & (1 << i)) != 0))
}
