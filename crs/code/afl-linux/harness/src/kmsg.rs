use anyhow::{anyhow, Context, Result};
use nix::fcntl::{fcntl, FcntlArg, OFlag};
use std::{
    fs::File,
    io::{self, Read, Seek, SeekFrom},
    os::fd::AsRawFd,
    str,
};

/// A handle to the `dmesg` buffer, used for kernel logs.
#[derive(Debug)]
pub struct Dmesg {
    file: File,
}

impl Dmesg {
    /// Opens the kernel dmesg device (`/dev/kmsg`).
    pub fn new() -> Result<Dmesg> {
        let path = "/dev/kmsg";
        let mut file = File::open(path).with_context(|| anyhow!("Failed to open {path:?}"))?;

        // Set the nonblocking flag.
        fcntl(
            file.as_raw_fd(),
            FcntlArg::F_SETFL(OFlag::O_CLOEXEC | OFlag::O_NONBLOCK),
        )
        .context("Failed to set file descriptor flags")?;

        // Seek to the end.
        file.seek(SeekFrom::End(0))
            .context("Failed to seek to end")?;

        Ok(Dmesg { file })
    }

    /// Tries to read from the `dmesg` buffer, returning `None` if no message is currently
    /// available.
    pub fn read(&mut self) -> Result<Option<String>> {
        let mut buf = vec![0; 4096];
        match self.file.read(&mut buf) {
            Ok(len) => {
                let str = str::from_utf8(&buf[..len]).context("A dmesg entry was not UTF-8?")?;
                Ok(Some(str.to_string()))
            }
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => Ok(None),
            Err(err) => Err(anyhow::Error::from(err).context("Failed to read dmesg entry")),
        }
    }
}
