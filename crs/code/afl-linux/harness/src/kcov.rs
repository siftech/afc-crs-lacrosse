use anyhow::{anyhow, Context, Result};
use nix::{
    ioctl_write_int_bad,
    libc::{c_int, ioctl},
    request_code_none, request_code_read,
    sys::mman::{mmap, munmap, MapFlags, ProtFlags},
};
use std::{
    fs::File,
    mem::size_of,
    num::NonZeroUsize,
    ops::Deref,
    os::fd::AsRawFd,
    ptr::NonNull,
    slice,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug)]
pub struct KCov {
    /// The `/sys/kernel/debug/kcov` file.
    fd: File,

    /// The pointer we mapped the trace buffer to.
    ptr: NonNull<usize>,

    /// The number of entries we configured the buffer to store.
    entries: usize,
}

impl KCov {
    /// Tries to set up kcov with a buffer that stores the given number of entries, but does not
    /// start collection.
    pub fn new(mut entries: usize) -> Result<KCov> {
        const PATH: &str = "/sys/kernel/debug/kcov";

        // Ensure there are at least two entries, as the API requires.
        entries = entries.max(2);

        // Open the kcov file.
        let fd = File::options()
            .read(true)
            .write(true)
            .open(PATH)
            .with_context(|| anyhow!("Failed to open {PATH:?}"))?;

        // Set up the trace.
        //
        // SAFETY: This ioctl is being used on the right kind of file descriptor.
        unsafe { kcov_init_trace(&fd, entries) }
            .context("Failed to perform KCOV_INIT_TRACE ioctl")?;

        // Map the trace buffer.
        //
        // UNWRAP: Both factors must be nonzero.
        let map_len = NonZeroUsize::new(entries * size_of::<usize>()).unwrap();
        // SAFETY: Each argument is valid, and we're not MAP_FIXEDing over anything.
        let ptr = unsafe {
            mmap(
                None,
                map_len,
                ProtFlags::PROT_READ | ProtFlags::PROT_WRITE,
                MapFlags::MAP_SHARED,
                &fd,
                0,
            )
            .context("Failed to mmap kcov buffer")?
        };

        // Construct and return the struct.
        let ptr = ptr.cast();
        Ok(KCov { fd, ptr, entries })
    }

    /// Starts collecting the trace into the trace buffer.
    pub fn enable(&mut self) -> Result<()> {
        unsafe { kcov_enable(self.fd.as_raw_fd(), 0) }
            .context("Failed to perform KCOV_ENABLE ioctl")?;
        Ok(())
    }

    /// Stops collecting the trace into the trace buffer.
    pub fn disable(&mut self) -> Result<()> {
        unsafe { kcov_disable(self.fd.as_raw_fd(), 0) }
            .context("Failed to perform KCOV_DISABLE ioctl")?;
        Ok(())
    }

    /// Clears the trace buffer.
    pub fn reset(&mut self) {
        // SAFETY: To construct this type, we must have allocated at least two entries, one for
        // this field and one actual datum.
        let len = unsafe { AtomicUsize::from_ptr(self.ptr.cast().as_mut()) };
        len.store(0, Ordering::Relaxed)
    }

    /// Runs a closure with a clear buffer and tracing turned on.
    pub fn with<T>(&mut self, body: impl FnOnce() -> Result<T>) -> Result<T> {
        self.enable().context("Failed to enable KCov tracing")?;
        self.reset();
        match body() {
            Ok(out) => {
                self.disable().context("Failed to disable KCov tracing")?;
                Ok(out)
            }
            Err(err) => {
                let _ = self.disable();
                Err(err)
            }
        }
    }
}

impl Drop for KCov {
    fn drop(&mut self) {
        // Unmap the trace buffer.
        //
        // SAFETY: We made this mapping when we constructed the type, and nothing's been allowed to
        // tamper with it.
        if let Err(err) = unsafe { munmap(self.ptr.cast(), self.entries * size_of::<usize>()) } {
            log::error!("Failed to munmap trace buffer: {err:?}");
        }
    }
}

impl Deref for KCov {
    type Target = [usize];

    fn deref(&self) -> &[usize] {
        let ptr = self.ptr.cast().as_ptr();
        // SAFETY: To construct this type, we must have allocated at least two entries, one for
        // this field and one actual datum.
        let len = unsafe { AtomicUsize::from_ptr(ptr) };
        let len = len.load(Ordering::Relaxed);
        // SAFETY: The kernel guarantees to only create as many entries as will fit, and we never
        // increase the number of entries marked as present.
        unsafe { slice::from_raw_parts(ptr.add(1), len) }
    }
}

/// Performs the KCOV_INIT_TRACE ioctl.
unsafe fn kcov_init_trace<Fd: AsRawFd>(fd: &Fd, data: usize) -> nix::Result<c_int> {
    unsafe {
        nix::errno::Errno::result(ioctl(
            fd.as_raw_fd(),
            request_code_read!('c', 1, size_of::<usize>()),
            data,
        ))
    }
}

ioctl_write_int_bad!(kcov_enable, request_code_none!('c', 100));
ioctl_write_int_bad!(kcov_disable, request_code_none!('c', 101));
