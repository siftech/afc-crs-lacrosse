#!/bin/bash
set -euo pipefail

echo "Configuring inotify and file descriptor limits..."

# 1. Set inotify sysctl limits
echo "Setting sysctl inotify limits..."
sudo tee /etc/sysctl.d/99-inotify.conf > /dev/null <<EOF
fs.inotify.max_user_instances = 262144
fs.inotify.max_user_watches = 8388608
fs.inotify.max_queued_events = 1048576
fs.file-max = 4194304
EOF

# 2. Apply sysctl settings
sudo sysctl --system

# 3. Set soft/hard file limits in limits.conf
echo "Updating /etc/security/limits.conf..."
sudo tee -a /etc/security/limits.conf > /dev/null <<EOF

# Custom inotify test limits
* soft nofile 1048576
* hard nofile 1048576
EOF

# 4. Ensure pam_limits.so is loaded
for file in /etc/pam.d/common-session /etc/pam.d/sshd; do
    if ! grep -q 'pam_limits.so' "$file"; then
        echo "Adding pam_limits.so to $file..."
        echo "session required pam_limits.so" | sudo tee -a "$file"
    fi
done

# 5. Set system-wide systemd manager limits
echo "Setting system-level systemd NOFILE limit..."
sudo mkdir -p /etc/systemd/system.conf.d
sudo tee /etc/systemd/system.conf.d/ulimit.conf > /dev/null <<EOF
[Manager]
DefaultLimitNOFILE=1048576
EOF

# 6. Set user-level systemd manager limits
echo "Setting user-level systemd NOFILE limit..."
mkdir -p ~/.config/systemd/user.conf.d
tee ~/.config/systemd/user.conf.d/99-nofile.conf > /dev/null <<EOF
[Manager]
DefaultLimitNOFILE=1048576
DefaultLimitNOFILESoft=1048576
EOF

# 7. Reload systemd config
echo "Reloading systemd configuration..."
sudo systemctl daemon-reexec
sudo systemctl daemon-reload

systemctl --user daemon-reexec
systemctl --user daemon-reload

# 8. Verify current user limits
echo "Current user soft/hard limits:"
ulimit -n
ulimit -a

# 9. Prompt for reboot
echo -e "\n All configuration steps completed."

read -p "Reboot now to apply all changes (y/n)? " -r
if [[ "$REPLY" =~ ^[Yy]$ ]]; then
    echo "Rebooting..."
    sudo reboot
else
    echo "Please reboot manually for changes to fully take effect."
fi
