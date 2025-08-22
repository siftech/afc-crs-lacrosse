#!/bin/bash
set -euo pipefail

echo "Configuring inotify and file descriptor limits..."

PROGNAME=$(basename "$0")
warn()  { echo "$PROGNAME: ${@}" 1>&2; }
die()   { warn "${@}"; exit 1; }
dbug()   { test -z "${DEBUG:-}" || warn "${@}"; }

# Get script directory
thisdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && /bin/pwd )"
dbug "thisdir is $thisdir"

# 0. Copy sshd configs
sudo cp "$thisdir/azure-sshd-config" /etc/ssh/sshd_config
if [ -f "$thisdir/azure_sshd_override" ]; then
    echo "Creating directory /etc/systemd/system/ssh.service.d if it doesn't exist..."
    sudo mkdir -p /etc/systemd/system/ssh.service.d
    echo "Copying sshd override file..."
    sudo cp "$thisdir/azure_sshd_override" /etc/systemd/system/ssh.service.d/override.conf
fi
sudo systemctl daemon-reexec
sudo systemctl daemon-reload
sudo systemctl restart ssh

# 1. Set inotify sysctl limits persistently
echo "Setting sysctl inotify limits..."
sudo tee /etc/sysctl.d/99-inotify.conf > /dev/null <<EOF
fs.inotify.max_user_instances = 262144
fs.inotify.max_user_watches = 8388608
fs.inotify.max_queued_events = 1048576
fs.file-max = 4194304
EOF

# 2. Apply sysctl settings now
sudo sysctl --system

# 3. Append soft/hard nofile limits persistently
echo "Updating /etc/security/limits.conf..."
sudo tee -a /etc/security/limits.conf > /dev/null <<EOF

# Custom inotify and file descriptor limits
* soft nofile 1048576
* hard nofile 1048576
* soft nproc 2063729
* hard nproc 2063729
EOF

# 4. Ensure pam_limits.so is loaded
for file in /etc/pam.d/common-session /etc/pam.d/sshd; do
    if ! grep -q 'pam_limits.so' "$file"; then
        echo "Adding pam_limits.so to $file..."
        echo "session required pam_limits.so" | sudo tee -a "$file"
    fi
done

# 5. Set system-wide systemd NOFILE limit persistently
echo "Setting system-level systemd NOFILE limit..."
sudo mkdir -p /etc/systemd/system.conf.d
sudo tee /etc/systemd/system.conf.d/ulimit.conf > /dev/null <<EOF
[Manager]
DefaultLimitNOFILE=2063729
EOF

# 6. Set user-level systemd NOFILE limit persistently
echo "Setting user-level systemd NOFILE limit..."
mkdir -p ~/.config/systemd/user.conf.d
tee ~/.config/systemd/user.conf.d/99-nofile.conf > /dev/null <<EOF
[Manager]
DefaultLimitNOFILE=2063729
DefaultLimitNOFILESoft=2063729
EOF

# 7. Reload systemd configurations (system and user)
echo "Reloading systemd configuration..."
sudo systemctl daemon-reexec
sudo systemctl daemon-reload
systemctl --user daemon-reexec || true
systemctl --user daemon-reload || true

# 8. Set ulimit for current shell session immediately (soft and hard)
ulimit -Sn 1048576
ulimit -Hn 1048576
echo "Current shell limits for open files:"
echo "Soft limit: $(ulimit -Sn)"
echo "Hard limit: $(ulimit -Hn)"

# 9. Final message
echo -e "\nConfiguration complete."
ulimit -a

echo -e "\nPlease reboot later for all changes to fully apply system-wide."
