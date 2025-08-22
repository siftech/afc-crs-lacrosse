FROM ubuntu:22.04

# Need this for various things below to configure non-interactively.
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
	ca-certificates nix-bin nix-setup-systemd rsync && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*


# Get the packages we intend to mount into the other containers.
RUN mkdir -p /lacrosse
COPY crs/code/docker/nix-copy/ /lacrosse/nix/
# TODO: store optimization goes here
RUN nix-build --out-link /lacrosse/aflplusplus /lacrosse/nix/default.nix && \
    nix-collect-garbage

CMD ["/lacrosse/nix/container_main.sh"]
