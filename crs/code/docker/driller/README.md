# Driller Docker

The Dockerfile specifying a driller setup. Creates a docker image that
can be used for development on driller.

This docker image builds on the shellphish/mechaphish image (available
on dockerhub). It simply adds a script for starting driller and
removes the container entrypoint so that commands can be passed from
the commmand line when starting the container.

This image is designed to either be interactive, or used like a binary
that runs then dies.

## Usage

The <mounts> are described in a the Mounts section below.

To run interactively:

```
./attach-driller -T ${NEO_FUZZ_HOME}/code/tracer -D ${NEO_FUZZ_HOME}/code/driller -b ${NEO_FUZZ_HOME}/code/tools/program-generators/generated-programs/keyed_levels -o ${NEO_FUZZ_HOME}/code/docker/driller/output
```

Use `./attach-driller -h` for more information.


To run like a binary:

```
./driller -c 1  -o /home/matthew/projects/neo-fuzz/driller-output /home/matthew/projects/neo-fuzz/target-binaries/maze
```

Use `./driller -h` for more information.

## Dependencies

The only dependency is the docker image
mdehavensift/driller:20171001. This image is hosted on dockerhub, so
it will be pulled down automatically.

The pulled down driller image is a tagged version of the
shellphish/mechaphish image. This tagged version is used because none
of the shellphish or angr code/docker images are tagged. They are
automatically rebuilt when the github changes.

Using the latest version of the mechaphish image would eventually
cause a problem when some interface in the angr code changes (very
likely to happen). The tagged version provides stability so we can
edit the driller code without worrying about the angr code changing
out from under us.

## Building

This image can be built by running `make`. This does nothing but a
standard `docker build` command, but it may be useful in the future if
the build becomes more complex. The image will be named
driller:latest.

## Mounts

This docker image expects at least two different directories to be mounted.

- The directory with the target binary mounted to `/home/angr/bins/`
- The directory to which the output will be written mounted to `/home/angr/driller-results/`
- Optionally: Modified driller code directory mounted to `/home/angr/angr-dev/driller/`
