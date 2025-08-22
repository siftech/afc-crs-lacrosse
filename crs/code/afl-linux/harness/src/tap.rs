use anyhow::{bail, Context, Result};
use nix::{
    ioctl_write_ptr_bad,
    libc::{ifreq, IFF_NO_PI, IFF_TAP},
    request_code_write,
};
use std::{
    fs::File,
    io::Write,
    mem::{size_of, transmute, MaybeUninit},
    os::{fd::AsRawFd, raw::c_int},
    str,
};

/// A high-level wrapper around a tap device.
#[derive(Debug)]
pub struct Tap {
    fd: File,
    name: String,
}

impl Tap {
    /// Creates a new tap device.
    pub fn new() -> Result<Tap> {
        // Open the tuntap device node.
        let fd = File::options()
            .read(true)
            .write(true)
            .open("/dev/net/tun")
            .context("Failed to open /dev/net/tun")?;

        // Create our request.
        //
        // SAFETY: ifreq is pure data.
        let mut ifreq: ifreq = unsafe { MaybeUninit::zeroed().assume_init() };
        ifreq.ifr_ifru.ifru_flags = (IFF_TAP | IFF_NO_PI) as i16;

        // Try to create the device.
        //
        // SAFETY: fd and ifreq are both valid, and this ioctl cannot otherwise compromise safety.
        unsafe { tun_set_iff(fd.as_raw_fd(), &ifreq) }.context("Failed to create tap device")?;

        // Try to get the device's name.
        let nul_index = ifreq
            .ifr_name
            .iter()
            .position(|&b| b == 0)
            .unwrap_or(ifreq.ifr_name.len());
        let name = str::from_utf8(bytes_as_unsigned(&ifreq.ifr_name[..nul_index]))
            .context("Newly created tap device had an illegal interface name")?
            .to_string();

        Ok(Tap { fd, name })
    }

    /// Returns the name of the tap device.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Writes a packet to the tap device.
    pub fn write(&mut self, packet: &[u8]) -> Result<()> {
        let len = self
            .fd
            .write(packet)
            .context("Failed to write packet to the tap device")?;
        if len != packet.len() {
            bail!(
                "Failed to write entire packet to the tap device (only {} of {} bytes)",
                len,
                packet.len()
            );
        }
        Ok(())
    }
}

ioctl_write_ptr_bad!(
    tun_set_iff,
    request_code_write!(b'T', 202, size_of::<c_int>()),
    ifreq
);

fn bytes_as_unsigned(bytes: &[i8]) -> &[u8] {
    unsafe { transmute(bytes) }
}
