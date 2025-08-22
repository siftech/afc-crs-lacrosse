//! The various fuzzing harnesses.

use crate::afl::FuzzStatus;
use anyhow::Result;
use clap::Parser;
use paste::paste;

mod external;
mod test_guidance;
mod tipc;

macro_rules! defharnesses {
    ($($(#[$meta:meta])* $name:ident),* $(,)?) => {
        paste! {
            /// The kind of harness that should be used to fuzz.
            #[derive(Clone, Debug, Parser)]
            pub enum HarnessKind {
                $($(#[$meta])* [< $name:camel >]($name::[< $name:camel Args >])),*
            }

            impl HarnessKind {
                /// Creates a harness.
                pub fn make_harness(self) -> Result<Harness> {
                    match self {
                        $(HarnessKind::[< $name:camel >](args) => {
                            $name::[< $name:camel Harness >]::new(args)
                                .map(Harness::[< $name:camel >])
                        }),*
                    }
                }
            }

            #[derive(Debug)]
            pub enum Harness {
                $([< $name:camel >]($name::[< $name:camel Harness >])),*
            }

            impl Harness {
                /// Runs the command to be fuzzed.
                pub fn fuzz(&mut self, bytes: &[u8], trace: impl FnMut(usize)) -> Result<FuzzStatus> {
                    match self {
                        $(Harness::[< $name:camel >](harness) => {
                            harness.fuzz(bytes, trace)
                        }),*
                    }
                }
            }
        }
    };
}

defharnesses! {
    /// A harness backed by an external executable.
    external,

    /// A harness that exists to test the guidance feature.
    test_guidance,

    /// Creates a TAP device, adds it as a TIPC bearer, and fuzzes it with Ethernet packets.
    tipc
}
