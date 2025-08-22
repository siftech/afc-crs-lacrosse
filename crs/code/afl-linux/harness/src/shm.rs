use anyhow::{anyhow, Result};
use nix::libc::{c_int, shmat, shmctl, shmdt, shmid_ds, IPC_STAT};
use std::{
    ffi::c_void,
    io,
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    ptr, slice,
};

/// An RAII wrapper for an XSI shared memory object.
#[derive(Debug)]
pub struct Shm {
    id: c_int,
    ptr: *mut c_void,
    len: usize,
}

impl Shm {
    /// Attaches to an XSI shared memory object, for reading and writing.
    pub fn attach(id: c_int) -> Result<Shm> {
        // SAFETY: The address and flags are valid, and the implementation is required to handle an
        // invalid id.
        let ptr = unsafe { shmat(id, ptr::null(), 0) };
        if (ptr as isize) == -1 {
            return Err(anyhow::Error::from(io::Error::last_os_error())
                .context(anyhow!("Failed to attach to XSI shared memory object {id}")));
        }

        // Construct the RAII object, so if the rest fails we still shmdt it.
        let mut shm = Shm { id, ptr, len: 0 };

        // Get the size of the shared memory object.
        //
        // SAFETY: This type is pure data.
        let mut stat: shmid_ds = unsafe { MaybeUninit::zeroed().assume_init() };
        if unsafe { shmctl(id, IPC_STAT, &mut stat) } != 0 {
            return Err(anyhow::Error::from(io::Error::last_os_error())
                .context(anyhow!("Failed to stat XSI shared memory object {id}")));
        }
        shm.len = stat.shm_segsz;

        Ok(shm)
    }
}

impl Deref for Shm {
    type Target = [u8];

    fn deref(&self) -> &[u8] {
        // SAFETY: ptr and len both came from the shm* libc calls and haven't been tampered with.
        unsafe { slice::from_raw_parts(self.ptr.cast(), self.len) }
    }
}

impl DerefMut for Shm {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: ptr and len both came from the shm* libc calls and haven't been tampered with.
        unsafe { slice::from_raw_parts_mut(self.ptr.cast(), self.len) }
    }
}

impl Drop for Shm {
    fn drop(&mut self) {
        // SAFETY: We got the pointer from shmat, and haven't let anything tamper with it.
        if unsafe { shmdt(self.ptr) } != 0 {
            log::error!(
                "Failed to shmdt XSI shared memory object {}: {:?}",
                self.id,
                io::Error::last_os_error()
            );
        }
    }
}
