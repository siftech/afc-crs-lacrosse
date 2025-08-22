//! An implementation of the AFL's persistent fuzzing.

use crate::{guidance::Guidance, prng::Prng, shm::Shm};
use anyhow::{bail, Context, Result};
use nix::{
    fcntl::{fcntl, FcntlArg},
    libc::SIGSEGV,
    unistd::Pid,
};
use std::{
    env,
    fs::File,
    hint::black_box,
    io::{stdin, Read, Write},
    ops::BitXor as _,
    os::fd::FromRawFd,
};

#[derive(Debug)]
pub struct Coverage<'a> {
    shm: &'a mut Shm,
    prev: u16,
    guidance: &'a Guidance,
    prng: Prng,
}

impl<'a> Coverage<'a> {
    pub fn trace(&mut self, addr: usize) {
        let next = Coverage::hash(addr);
        self.poke(self.prev ^ next);
        self.prev = next >> 1;

        for _ in 0..self.guidance.interestingness(addr) {
            let mut i_bytes = [0; 2];
            self.prng.generate_bytes(&mut i_bytes);
            self.poke(u16::from_le_bytes(i_bytes))
        }
    }

    /// Increments a particular entry in the coverage map.
    fn poke(&mut self, i: u16) {
        let count = &mut self.shm[i as usize];
        *count = count.wrapping_add(1);
    }

    fn hash(n: usize) -> u16 {
        // Slice and dice n into some number of u32s (depending on the width of usize).
        let n_bytes = n.to_le_bytes();
        let mut n_u32s = [0u32; (usize::BITS / u32::BITS) as usize];
        for (i, n) in n_u32s.iter_mut().enumerate() {
            // UNWRAP: This has the right number of bytes.
            let chunk = n_bytes[4 * i..4 * (i + 1)].try_into().unwrap();
            *n = u32::from_le_bytes(chunk);
        }

        // Do fxhash32 with each.
        let mut x: u32 = 0;
        for word in n_u32s {
            x = x.rotate_left(5).bitxor(word).wrapping_mul(0x27220a95);
        }

        // XOR the two halves of x against each other and return the result.
        let h = (x >> 16) as u16;
        let l = (x & 0xffff) as u16;
        h ^ l
    }
}

/// Runs the provided closure as the program to fuzz. This is essentially never what you want when
/// testing the program, but here we're testing the kernel.
pub fn fuzz_closure(
    guidance: &Guidance,
    mut body: impl FnMut(Coverage, &[u8]) -> FuzzStatus,
) -> Result<()> {
    // Connect to AFL.
    let mut forksrv = Forksrv::new().context("Failed to connect to AFL as the fork server")?;

    // Allocate a single buffer that we'll keep reusing for AFL's input.
    let mut buf = vec![0; 512 * 1024];

    // Fuzz forever.
    loop {
        // Wait for AFL to tell us that it's ready.
        let status = forksrv
            .read()
            .context("Failed to read readiness from AFL")?;
        if status != 0 {
            bail!("AFL readiness word was not 0: {status}");
        }

        // Tell AFL that we forked a child (we're lying). We would do it just before this if we
        // were actually trying to fork a child.
        forksrv
            .write(Pid::this().as_raw() as u32)
            .context("Failed to write PID to AFL")?;

        // Read the input from AFL.
        let len = stdin()
            .read(&mut buf)
            .context("Failed to read the testcase from AFL")?;
        let buf = &buf[..len];

        // Run the body with AFL's input.
        let status = body(
            Coverage {
                shm: &mut forksrv.afl_shm,
                prev: 0,
                guidance,
                prng: Prng::new(buf),
            },
            buf,
        );

        // Write the status back to AFL.
        let status = match status {
            FuzzStatus::Ok => 0,
            FuzzStatus::Crash => SIGSEGV as u32,
        };
        forksrv
            .write(status)
            .context("Failed to write status to AFL")?;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FuzzStatus {
    Ok,
    Crash,
}

/// The handles used by the AFL forkserver.
#[derive(Debug)]
struct Forksrv {
    afl_read: File,
    afl_write: File,
    afl_shm: Shm,
}

impl Forksrv {
    /// Prepares to act as the forkserver.
    fn new() -> Result<Forksrv> {
        // Find the ID of the XSI SHM AFL is passing us.
        //
        // We need this string to appear in the binary, _with_ its null terminator...
        let shm_id_var = black_box("__AFL_SHM_ID\0");
        let shm_id = env::var(&shm_id_var[..shm_id_var.len() - 1])
            .context("Failed to get AFL's SHM id -- are we running under afl?")?;
        let shm_id = shm_id.parse().context("Failed to parse AFL's SHM id")?;

        // Check that the file descriptors that connect to AFL exist.
        fcntl(198, FcntlArg::F_GETFD).context("Failed to find read end of the pipe to AFL")?;
        fcntl(199, FcntlArg::F_GETFD).context("Failed to find write end of the pipe to AFL")?;

        // Make handles to the file descriptors.
        //
        // SAFETY: We checked that this is AFL; assuming that we're not being tricked (...) these
        // should be pipes, which was can use as Files.
        let afl_read = unsafe { File::from_raw_fd(198) };
        let afl_write = unsafe { File::from_raw_fd(199) };

        // Open the shm.
        let afl_shm =
            Shm::attach(shm_id).context("Failed to attach to AFL's shared memory object")?;

        // Make the forkserver structure, so we can call methods on it.
        let mut forksrv = Forksrv {
            afl_read,
            afl_write,
            afl_shm,
        };

        // Write a zero to AFL as part of the handshake.
        forksrv
            .write(0)
            .context("Failed to write to AFL as part of the initialization handshake")?;

        Ok(forksrv)
    }

    /// Reads an integer from AFL.
    fn read(&mut self) -> Result<u32> {
        let mut buf = [0; 4];
        self.afl_read
            .read_exact(&mut buf)
            .context("Failed to read from AFL")?;
        Ok(u32::from_ne_bytes(buf))
    }

    /// Writes an integer to AFL.
    fn write(&mut self, n: u32) -> Result<()> {
        self.afl_write
            .write_all(&n.to_ne_bytes())
            .context("Failed to write to AFL")
    }
}
