# You should source this script in your shell's rc file (e.g. bashrc or zshrc).
# Currently, this is only designed to support bash and zsh; users of other
# shells should ensure it works identically before blindly trusting it.

# Enables shellcheck (https://github.com/koalaman/shellcheck/), a helpful
# shell linter.
# shellcheck shell=bash

# bash and zsh report the name of a sourced script differently; this idiom
# should be compatible with either.
export LACROSSE_HOME
LACROSSE_HOME="$(dirname "$(dirname "$(dirname "${BASH_SOURCE[0]:-"$0"}")")")"

# Get the NF-specific scripts, as well as prt.
export PATH="$LACROSSE_HOME/code/prt:$PATH"
export PATH="$LACROSSE_HOME/code/tools:$PATH"

# Check for necessary tools.
for bin in make perl svn; do
	if ! command -v $bin >/dev/null; then
		echo \`"${bin}' is missing, install it!" >&2
	fi
done

# Docker related environment variables, including environment variables for the
# docker service must be set up BEFORE starting (or restarting) the rootless
# docker service.
export DOCKER_HOST="unix:///run/user/$(id -u)/docker.sock"
# rootless Docker's networking is slightly different, we must specify LOOPBACK
export LOOPBACK="10.0.2.2"
systemctl --user set-environment \
    DOCKERD_ROOTLESS_ROOTLESSKIT_ALLOW_HOST_LOOPBACK=true
systemctl --user set-environment \
    DOCKERD_ROOTLESS_ROOTLESSKIT_DISABLE_HOST_LOOPBACK=false
systemctl --user set-environment \
    DOCKERD_ROOTLESS_ROOTLESSKIT_NET=slirp4netns
systemctl --user set-environment \
    DOCKERD_ROOTLESS_ROOTLESSKIT_PORT_DRIVER=slirp4netns

# Start the rootless docker service. This is idempotent.
#
# NOTE: If you use 'systemctl --user {import,set}-environment ...' later,
# then you must restart the docker service manually with:
# systemctl --user restart docker.service
# in order for it to take effect. Restarting will kill all running containers.
command -v docker-rootless-init >/dev/null && \
    eval "$(docker-rootless-init)" || :

# Poll to check docker is up so tools like Jenkins or stuff the user wants
# to run via ssh don't try to use it before it is ready and blow up.
while (! docker stats --no-stream > /dev/null 2>&1 ); do
  echo "Waiting for Docker to launch..."
  sleep 1
done

# Set some variables used by realuser.sh to ensure permissions are reasonable.
# These shouldn't be strictly necessary, but it should be harmless to set them
# here, and it's useful that they're set in some scenarios (e.g. dev-in-Docker
# with rootless Docker).
export HOST_USERNAME HOST_USERID HOST_GROUPNAME HOST_GROUPID
HOST_USERNAME="${HOST_USERNAME:-"$(id -un)"}"
HOST_USERID="${HOST_USERID:-"$(id -u)"}"
HOST_GROUPNAME="${HOST_GROUPNAME:-"$(id -gn)"}"
HOST_GROUPID="${HOST_GROUPID:-"$(id -g)"}"

# Similar logic (wrt setting it even though it should be detected) applies to
# DOCKER_TAG. On rooted Docker, this ensures that running code/docker/build.sh
# doesn't trample over other users' containers; on rootless Docker, it doesn't
# currently (2021-06-18) matter what it is as long as it is consistent.
export DOCKER_TAG="${DOCKER_TAG:-"${HOST_USERNAME}"}"

# Check that the user has some user-deconflicting variables set. These should
# be looked up in a table in the future, but for now we require that the user
# provides them. Some of these (e.g. DOCKER_SUBNET) may be obsolete with
# rootless, but some (e.g. CIRCA_BASEPORT) are not, and we still support rooted
# Docker for now.
if [[ -z ${CIRCA_BASEPORT+set} ]]; then
	echo "The CIRCA_BASEPORT variable is not set!" >&2
	echo "Check the NeoFuzz manual for more info" >&2
fi
if [[ -z ${DOCKER_SUBNET+set} ]]; then
	echo "The DOCKER_SUBNET variable is not set!" >&2
	echo "Check the NeoFuzz manual for more info" >&2
fi

# The CIRCA_BASENAME variable should be your username, per the manual.
export CIRCA_BASENAME="${CIRCA_BASENAME:-"${USER:-"$(id -un)"}"}"
