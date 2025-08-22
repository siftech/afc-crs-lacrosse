#!/bin/sh
set -eux

# Install the packages we need.
apk add curl squashfs-tools

# Download the base.
curl -LO https://dl-cdn.alpinelinux.org/alpine/v3.19/releases/x86_64/alpine-minirootfs-3.19.1-x86_64.tar.gz
test "$(sha256sum alpine-minirootfs-3.19.1-x86_64.tar.gz)" = "185123ceb6e7d08f2449fff5543db206ffb79decd814608d399ad447e08fa29e  alpine-minirootfs-3.19.1-x86_64.tar.gz"

# Extract the base.
mkdir /tmp/rootfs
tar -C /tmp/rootfs -xzf alpine-minirootfs-3.19.1-x86_64.tar.gz

# Install some convenience packages.
apk add -p /tmp/rootfs -- afl++ bash cargo clang doas e2fsprogs gcc iproute2 iproute2-rdma libc-dev linux-headers lld llvm htop perl neovim rust strace tmux

# Write the doas config.
echo "permit nopass user" >> /tmp/rootfs/etc/doas.conf

# Create a user.
yes "" | chroot /tmp/rootfs adduser user

# Delete the root and user passwords.
chroot /tmp/rootfs passwd -d root
chroot /tmp/rootfs passwd -d user

# Make the code and here directories.
mkdir /tmp/rootfs/code /tmp/rootfs/here

# Add useful symlinks to /root and /home/user.
ln -s /code /tmp/rootfs/root/code
ln -s /here /tmp/rootfs/root/here
ln -s /code /tmp/rootfs/home/user/code
ln -s /here /tmp/rootfs/home/user/here

# Copy in our binaries.
install -Dt /tmp/rootfs/sbin          /code/init
install -Dt /tmp/rootfs/usr/local/bin /code/addr2line-filt.pl
ln -s /here/doit.sh /tmp/rootfs/usr/local/bin/doit.sh

# Make the filesystem.
cd /tmp/rootfs
test ! -f /code/rootfs.img || rm /code/rootfs.img
mksquashfs . /code/rootfs.img -comp zstd
