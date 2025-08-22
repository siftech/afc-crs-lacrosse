#!/usr/bin/env bash

set -e
set -o pipefail
set -x  # Enables debug mode, showing each command before execution
exec > >(tee -a /var/log/setup-script.log) 2>&1  # Logs stdout and stderr

echo "Starting setup script!"

# Non-interactive setup
export DEBIAN_FRONTEND=noninteractive



# Wait for apt to be ready (handles lock files too)
until sudo apt update &> /dev/null; do
    echo "Waiting for apt to be ready..."
    sleep 10
done
echo "APT is ready"


# Install packages (the same as in lacrosse-0)
#docker-ce docker-buildx-plugin docker-ce-cli docker-ce-rootless-extras docker-compose-plugin python3-docker \
sudo -E apt-get update && sudo -E apt-get install -y apt-transport-https \
apparmor \
auditd autoconf automake base-files base-passwd \
bash bash-completion bc bison bolt bsd-mailx bsdutils build-essential bzip2 ca-certificates cadaver chrony \
clang clang-format clangd cloud-init cmake curl dash \
dbus-user-session \
debian-archive-keyring \
debian-keyring \
diffutils dnsutils \
dstat eatmydata emacs exim4 figlet file findutils fish flex freeipa-client fwupd-signed g++ gcc gdb gh git \
git-lfs git-svn gnupg gpg grep grub-pc gzip hostname htop init iotop iptables itop jq landscape-common \
less libacl1-dev libarchive13 libatasmart4 libblockdev-crypto2 libblockdev-fs2 libblockdev-loop2 \
libblockdev-part-err2 libblockdev-part2 libblockdev-swap2 libblockdev-utils2 libblockdev2 libbz2-dev libclang-dev \
libdbus-1-dev libdebconfclient0 libeatmydata1 libefiboot1 libefivar1 libffi-dev libflashrom1 libftdi1-2 libfwupd2 \
libfwupdplugin5 libgcab-1.0-0 libgccjit-11-dev libgnutls28-dev libgpgme11 libgudev-1.0-0 libgusb2 libjcat1 \
libjson-glib-1.0-0 libjson-glib-1.0-common liblzma-dev libmbim-glib4 libmbim-proxy libmm-glib0 libncurses-dev \
libncursesw5-dev libnspr4 libnss3 libparted-fs-resize0 libqmi-glib5 libqmi-proxy libreadline-dev libsmbios-c2 \
libsqlite3-dev libssl-dev libsystemd-dev libtcl8.6 libtool libtree-sitter-dev libtss2-esys-3.0.2-0 libtss2-mu0 \
libtss2-sys1 libtss2-tcti-cmd0 libtss2-tcti-device0 libtss2-tcti-mssim0 libtss2-tcti-swtpm0 libudisks2-0 \
libvolume-key1 libwrap0 libxml2-dev libxmlsec1-dev linux-virtual locales locales-all login lshw lsof make \
mercurial mosh mutt myrepos ncdu ncurses-base ncurses-bin ncurses-term openssh-client openssh-server openssh-sftp-server \
parallel perl-doc pipenv pipx pkg-config popularity-contest python-babel-localedata \
python-is-python3 python3 \
python3-apt python3-babel python3-certifi python3-dateparser python3-dev python3-distutils python3-docker python3-jinja2 \
python3-json-pointer python3-jsonpatch python3-jsonschema python3-lib2to3 python3-markupsafe python3-pip python3-poetry \
python3-pyrsistent python3-requests python3-serial python3-setuptools python3-tk python3-tz python3-urllib3 python3-venv \
python3-virtualenv python3.10 python3.10-dev python3.10-venv python3.11 python3.11-dev python3.11-venv qemu-guest-agent rlwrap \
rsync rsyslog sbsigntool screen secureboot-db shellcheck shim-signed \
slirp4netns ssh-import-id stow stress subversion sudo sysbench \
sysvinit-utils tcl tcl8.6 texinfo texlive tk-dev tmux tofrodos tpm-udev trash-cli tzdata ubuntu-minimal ubuntu-server \
ubuntu-standard unattended-upgrades unzip update-notifier-common usb-modeswitch usb-modeswitch-data vim wget \
xz-utils zip zlib1g-dev zsh zstd nfs-common

# for NetApp NFS storage
sudo mkdir -p /mnt/netapp
echo "NetApp ANF_IP: ${ANF_IP}"
# Add entry to /etc/fstab for persistent mount
#10.0.3.4:/netappnfs /mnt/netapp nfs rw,hard,rsize=262144,wsize=262144,vers=3,sec=sys,tcp,noatime,nconnect=8,exec 0 0
echo "${ANF_IP}:/netappnfs /mnt/netapp nfs \
    rw,hard,rsize=262144,wsize=262144,vers=3,sec=sys,tcp,noatime,nconnect=8,exec 0 0" >> /etc/fstab
# Mount all entries in fstab
mount -a
sudo chmod 777 /mnt/netapp
# Verify mount
df -h | grep netapp > /var/log/netapp_mount.log
echo "NetApp Files volume mounted at /mnt/netapp" >> /var/log/netapp_mount.log
#mkdir -p /mnt/netapp/crs_scratch
#mkdir -p /mnt/netapp/cp_root

# Env variables
export USERNAME="${USERNAME}"
export GIT_URL="${GIT_URL}"
export SSH_PRIVATE_KEY="${SSH_PRIVATE_KEY}"
export SSH_PUBLIC_KEY="${SSH_PUBLIC_KEY}"
export LACROSSE_GITHUB_PAT="${LACROSSE_GITHUB_PAT}"
export COMPETITION_API_KEY_ID="${COMPETITION_API_KEY_ID}"
export COMPETITION_API_KEY_TOKEN="${COMPETITION_API_KEY_TOKEN}"
export COMPETITION_API_TEAM_ID="${COMPETITION_API_TEAM_ID}"
export COMPETITION_API_TEAM_SECRET="${COMPETITION_API_TEAM_SECRET}"
export CRS_API_KEY_ID="${CRS_API_KEY_ID}"
export CRS_API_KEY_TOKEN="${CRS_API_KEY_TOKEN}"
export GEMINI_API_KEY="${GEMINI_API_KEY}"
export ANTHROPIC_API_KEY="${ANTHROPIC_API_KEY}"
export OPENAI_API_KEY="${OPENAI_API_KEY}"
export OTEL_EXPORTER_OTLP_HEADERS="${OTEL_EXPORTER_OTLP_HEADERS}"

# Persist variables
echo "export USERNAME=$USERNAME" >> /etc/environment
echo "export GIT_URL=$GIT_URL" >> /etc/environment
echo "export TAILSCALE_HOSTNAME=$TAILSCALE_HOSTNAME" >> /etc/environment

# Load variables immediately
source /etc/environment

# Create SSH Keys, use printf to account for newlines
echo "Private key is ${SSH_PRIVATE_KEY}"
echo "Public key is ${SSH_PUBLIC_KEY}"
echo "${SSH_PRIVATE_KEY}" > /home/$USERNAME/.ssh/id_azure
echo "${SSH_PUBLIC_KEY}" > /home/$USERNAME/.ssh/id_azure.pub

# Set permissions of ssh keys
chmod 600 /home/$USERNAME/.ssh/id_azure
chmod 664 /home/$USERNAME/.ssh/id_azure.pub
chown $USERNAME:$USERNAME /home/$USERNAME/.ssh/id_azure
chown $USERNAME:$USERNAME /home/$USERNAME/.ssh/id_azure.pub


# Git
echo "Cloning Git repository from $GIT_URL"
git clone $GIT_URL /home/$USERNAME/git-repo
# Ensure correct permissions
sudo -E chown -R $USERNAME:$USERNAME /home/$USERNAME/git-repo

# Docker rootless setup
sudo -E apt-get install -y make uidmap
# Adding to path only once, see line export PATH=...
# echo "export PATH=/home/$USERNAME/bin:\$PATH" >> /etc/environment
echo "export DOCKER_HOST=unix:///run/user/1000/docker.sock" >> /etc/environment
echo "export LOOPBACK=10.0.2.2" >> /etc/environment
echo "export HOST_USERID=1000" >> /etc/environment

# for accessing Azure Container Registry
echo "export SP_PASSWD=${SP_PASSWD}" >> /etc/environment
echo "export SP_APP_ID=${SP_APP_ID}" >> /etc/environment

# The environment values from them
echo "COMPETITION_API_KEY_ID=${COMPETITION_API_KEY_ID}" >> /etc/environment
echo "COMPETITION_API_KEY_TOKEN=${COMPETITION_API_KEY_TOKEN}" >> /etc/environment

echo "CRS_API_KEY_ID=${CRS_API_KEY_ID}" >> /etc/environment
echo "CRS_API_KEY_TOKEN=${CRS_API_KEY_TOKEN}" >> /etc/environment

# mapping to the current server.py
echo "COMPETITION_API_TEAM_ID=${COMPETITION_API_TEAM_ID}" >> /etc/environment
echo "COMPETITION_API_TEAM_SECRET=${COMPETITION_API_TEAM_SECRET}" >> /etc/environment
echo "COMPETITION_API_ENDPOINT=${COMPETITION_API_ENDPOINT}" >> /etc/environment
echo "COMPETITION_API_TEAM_ENDPOINT=${COMPETITION_API_TEAM_ENDPOINT}" >> /etc/environment

# Tailscale
echo "CRS_TAILSCALE_ENDPOINT=pixel-phantom-final.tail7e9b4c.ts.net" >> /etc/environment

# Add release tag
echo "CRS_LAX_RELEASE_TAG=v1.4" >> /etc/environment

# Needed env var for Lacrosse
# echo "export LACROSSE_HOME=/home/\$(id -un)/git-repo/crs" >> /etc/environment
# it becomes /home/root/git-repo/crs, updating path:
echo "export LACROSSE_HOME=${LACROSSE_HOME}" >> /etc/environment

# This might have incorrect path expansion. e.g. the default path for root instead of $USER
echo "export PATH=${LACROSSE_HOME}/code/tools:${LACROSSE_HOME}/code/prt:/home/$USERNAME/bin/:$PATH" >> /etc/environment

# Needed for rootless docker networking
echo "export DOCKERD_ROOTLESS_ROOTLESSKIT_DISABLE_HOST_LOOPBACK=false" >> /etc/environment
echo "export DOCKERD_ROOTLESS_ROOTLESSKIT_ALLOW_HOST_LOOPBACK=true" >> /etc/environment
echo "export DOCKERD_ROOTLESS_ROOTLESSKIT_NET=slirp4netns" >> /etc/environment
echo "export DOCKERD_ROOTLESS_ROOTLESSKIT_PORT_DRIVER=slirp4netns" >> /etc/environment
echo "export HOST_USERID=1000" >> /etc/environment

# Persist env variables
source /etc/environment


# Service will start on restart, not on log in
#loginctl enable-linger $USERNAME


su $USERNAME <<EOF
systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_DISABLE_HOST_LOOPBACK=false
systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_ALLOW_HOST_LOOPBACK=true
systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_NET=slirp4netns
systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_PORT_DRIVER=slirp4netns
curl -fsSL https://get.docker.com/rootless | sh

# ###########
# Typical docker rootless setup for user bashrc
# Needed for rootless Docker
# Setup personal docker rootless daemon
#export PATH=/home/$(id -un)/bin:$PATH
#systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_DISABLE_HOST_LOOPBACK=false
#systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_ALLOW_HOST_LOOPBACK=true
#systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_NET=slirp4netns
#systemctl --user set-environment DOCKERD_ROOTLESS_ROOTLESSKIT_PORT_DRIVER=slirp4netns
#command -v docker-rootless-init >/dev/null && \
#  eval "$(docker-rootless-init)" || :

export DOCKER_HOST=unix:///run/user/$(id -u)/docker.sock
#echo "export DOCKER_HOST=unix:///run/user/$(id -u)/docker.sock" >> /etc/environment
#systemctl --user start docker.service
export HOST_USERID=`id -u $USER`
#echo "export HOST_USERID=`id -u $USER`" >> /etc/environment
systemctl --user restart docker

# Persist env variables just because
source /etc/environment

echo "print env variables"
printenv
echo "PATH"
echo $PATH
echo "Now log in to Docker"
# Log in to Docker with service principal credentials and pull images
echo $LACROSSE_GITHUB_PAT | docker login ghcr.io \
    --username lax \
    --password-stdin
echo "Now pull images:"
docker pull ghcr.io/aixcc-finals/afc-crs-lacrosse/neo-fuzz-ccl:latest
docker pull ghcr.io/aixcc-finals/afc-crs-lacrosse/afc-crs-server:latest
docker pull docker:24-dind

# Tag images to expected name:tag
echo "Retagging images"
docker tag ghcr.io/aixcc-finals/afc-crs-lacrosse/neo-fuzz-ccl:latest ghcr.io/aixcc-sc/asc-crs-lacrosse/neo-fuzz-ccl:v1.5.1
docker tag ghcr.io/aixcc-finals/afc-crs-lacrosse/neo-fuzz-ccl:latest neo-fuzz-ccl:latest
docker tag ghcr.io/aixcc-finals/afc-crs-lacrosse/afc-crs-server:latest afc-crs-server:latest

#Install docker compose
echo "Installing docker compose"
DOCKER_CONFIG=$${DOCKER_CONFIG:-\$HOME/.docker}
mkdir -p \$DOCKER_CONFIG/cli-plugins
curl -SL https://github.com/docker/compose/releases/download/v2.33.0/docker-compose-linux-x86_64 -o \$DOCKER_CONFIG/cli-plugins/docker-compose
chmod +x \$DOCKER_CONFIG/cli-plugins/docker-compose

# Change to development branch in git-repo
echo "Checking out Git repo development branch:"
cd /home/$USERNAME/git-repo
git checkout development
git pull

# Install needed bin
echo "Install needed bins"
bash /home/$USERNAME/git-repo/crs/code/tools/install-lax-deps-to-home-bin

# Update kernel and userspace limits
echo "Update kernel and user limits"
bash /home/$USERNAME/git-repo/crs/code/tools/azure-configure-limits.sh

# Update keyring quotas
sudo cp /home/$USERNAME/git-repo/crs/code/tools/99-keyring.conf /etc/sysctl.d/99-keyring.conf
sudo sysctl --system

# Update TCP settings
sudo cp /home/$USERNAME/git-repo/crs/code/tools/99-high-performance-network.conf /etc/sysctl.d/99-high-performance-network.conf
sudo sysctl -p /etc/sysctl.d/99-high-performance-network.conf

# Test cubic and reuse settings, append test settings to the existing file
# Append the two test settings
echo "net.ipv4.tcp_tw_reuse = 2" | sudo tee -a /etc/sysctl.d/99-high-performance-network.conf
echo "net.ipv4.tcp_congestion_control = cubic" | sudo tee -a /etc/sysctl.d/99-high-performance-network.conf
sudo sysctl -p /etc/sysctl.d/99-high-performance-network.conf

#Update user level limits
sudo cp /home/$USERNAME/git-repo/crs/code/tools/sysctl.conf /etc/sysctl.conf
cp /home/$USERNAME/git-repo/crs/code/tools/override.conf /home/$USERNAME/.config/systemd/user/override.conf
sudo cp /home/$USERNAME/git-repo/crs/code/tools/limits.conf /etc/security/limits.conf

# Copy docker-rootless-init to bin location
sudo cp /home/$USERNAME/git-repo/crs/code/tools/docker-rootless-init /usr/local/bin/
sudo chmod +x /usr/local/bin/docker-rootless-init
rm -rf /home/$USERNAME/git-repo/crs/code/tools/docker-rootless-init

sudo cp /home/$USERNAME/git-repo/crs/code/tools/dockerd-rootless-wrapper.sh /usr/local/sbin/
sudo chmod +x /usr/local/sbin/dockerd-rootless-wrapper.sh

# Populate bashrc-template with Key Vault Values
sed -i \
  -e "s|<GitHub Repositiory Url for Lacrosse>|$GIT_URL|g" \
  -e "s|<GitHub Personal Access Token>|$LACROSSE_GITHUB_PAT|g" \
  -e "s|<Azure Service Principal Secret>|$SP_PASSWD|g" \
  -e "s|<Azure Service Principal App ID>|$SP_APP_ID|g" \
  -e "s|<AIxCC Competition API Key ID>|$COMPETITION_API_KEY_ID|g" \
  -e "s|<AIxCC Competition API Key Token>|$COMPETITION_API_KEY_TOKEN|g" \
  -e "s|<CRS API Key ID>|$CRS_API_KEY_ID|g" \
  -e "s|<CRS API Key Token>|$CRS_API_KEY_TOKEN|g" \
  -e "s|<OpenAI API Key>|$OPENAI_API_KEY|g" \
  -e "s|<Anthropic API Key>|$ANTHROPIC_API_KEY|g" \
  -e "s|<Google Gemini API Key>|$GEMINI_API_KEY|g" \
  -e "s|<Telemetry endpoint auth headers>|$OTEL_EXPORTER_OTLP_HEADERS|g" \
  /home/$USERNAME/git-repo/crs/code/tools/azure-bashrc-template

# Copy bashrc
cp /home/$USERNAME/git-repo/crs/code/tools/azure-bashrc-template /home/$USERNAME/.bashrc

## Install caddy
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
sudo apt update
sudo apt install caddy
# Allow caddy to proxy priviledged ports
sudo setcap 'cap_net_bind_service=+ep' /usr/bin/caddy

###########
EOF


# Create cluster-allocation
echo "hostname -d print"
hostname -d

#export AZURE_HOST=$(hostname -d)
#echo "AZURE_HOST is $AZURE_HOST"

#sed -i.bak "s/<AZURE_HOST>/$AZURE_HOST/g" /home/$USERNAME/git-repo/crs/code/tools/empty-pixel-phantom-cluster-allocations
sed -i.bak "s/<AZURE_HOST>/$(hostname -d)/g" /home/$USERNAME/git-repo/crs/code/tools/empty-pixel-phantom-cluster-allocations

cp /home/$USERNAME/git-repo/crs/code/tools/empty-pixel-phantom-cluster-allocations /tmp/cluster-allocation
chown $USERNAME:$USERNAME /tmp/cluster-allocation



# Create caddy service
cat <<EOF | sudo tee /etc/systemd/system/caddy.service
[Unit]
Description=api service
After=network.target

[Service]
Restart=always
ExecStart=/usr/bin/caddy reverse-proxy --from ${TAILSCALE_HOSTNAME}.tail7e9b4c.ts.net:443 --to ${TAILSCALE_HOSTNAME}.tail7e9b4c.ts.net:10080
ExecStop=/usr/bin/caddy stop

[Install]
WantedBy=default.target
EOF

# Enable services
sudo systemctl enable caddy.service
sudo systemctl start caddy.service


# Install Tailscale and login with pixel-phantom authorization
curl -fsSL https://tailscale.com/install.sh | sh
tailscale up -advertise-tags=tag:crs-pixel-phantom -hostname=${TAILSCALE_HOSTNAME} -authkey=${TAILSCALE_AUTH_KEY}

# Test Tailscale connectivity and enable service
curl https://echo.tail7e9b4c.ts.net
systemctl enable --now tailscaled

# ping the competition api 
curl -u "${COMPETITION_API_KEY_ID}:${COMPETITION_API_KEY_TOKEN}" https://api.tail7e9b4c.ts.net/v1/ping

# curl our crs for status 
#curl -u "${CRS_API_KEY_ID}:${CRS_API_KEY_TOKEN}"  https://pixel-phantom-final.tail7e9b4c.ts.net/status


#Create ssh-agent, add key, and start lax-never-neverending
su $USERNAME <<EOF
echo "1 Create ssh agent"
eval "\$(ssh-agent -s)"

echo "Add ssh key"
ssh-add /home/$USERNAME/.ssh/id_azure

echo "Create crs_scratch and cp_root dirs"
mkdir -p /mnt/netapp/crs_scratch
mkdir -p /mnt/netapp/cp_root

echo "Source .bashrc"
source /home/$USERNAME/.bashrc

echo "Run really_neverending"
cd /home/$USERNAME/git-repo/crs/code/tools
./finals-lax-will-neverend.sh
EOF

