# NOTE you cannot use inline comments-- do not put a comment at end of line, you'll get a very obscure error.
FROM ubuntu:22.04

ARG CRS_CONTEXT_PREFIX=crs/code/docker/neo-fuzz-ccl

# Need this for various things below to configure non-interactively.
ENV DEBIAN_FRONTEND=noninteractive
# Clean up APT when done, in same command, so layer doesnt lock in the stuff
# 04-18-18 MAD install flex so matchmaker can build
# Install acl to give access to /var/run/docker.sock

RUN apt-get update && \
    apt-get install -y --no-install-recommends software-properties-common && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
	virtualenvwrapper python3 python3-dev python3-pip python3-pygraphviz python3-setuptools \
	build-essential git vim flex libfl-dev acl apt-utils dmsetup fakeroot kpartx file gpg-agent \
	tzdata libc6-dev-i386 libgraphviz-dev tcpdump libz-dev libpcap-dev libnet1-dev libnids-dev libtool \
	automake gdb gdb-multiarch patchelf patchutils \
	gnuplot graphviz gosu sudo wget curl ssh rsync unzip nmap ncat snmp \
	tree jq jc ca-certificates make nano cargo iputils-ping iproute2 netcat-openbsd \
        readline-common sbcl && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY ${CRS_CONTEXT_PREFIX}/slime-v2.30.tar.gz /
RUN (cd /usr/local/; tar zxf /slime-v2.30.tar.gz; ln -s slime-2.30 slime) && rm -f /slime-v2.30.tar.gz

COPY ${CRS_CONTEXT_PREFIX}/sly-master.zip /
RUN (cd /usr/local/; unzip /sly-master.zip; ln -s sly-master sly) && rm -f /sly-master.zip


# Install Python 3.12.3, pip, and other dependencies
RUN add-apt-repository ppa:deadsnakes/ppa && \
    apt-get update && \
    apt-get install -y python3.12 python3.12-venv python3.12-dev python3-pip && \
    if [ ! -e /usr/bin/python ]; then ln -s /usr/bin/python3.12 /usr/bin/python; fi && \
    if [ ! -e /usr/bin/pip ]; then ln -s /usr/bin/pip3 /usr/bin/pip; fi && \
    rm -rf /var/lib/apt/lists/*


# Install setuptools and wheel using get-pip.py to ensure compatibility
RUN wget https://bootstrap.pypa.io/get-pip.py && \
    python3.12 get-pip.py && \
    rm get-pip.py

# Upgrade pip and install langchain
RUN python3.12 -m pip install --no-cache --upgrade pip
COPY "${CRS_CONTEXT_PREFIX}/../../langchain/requirements.txt" /tmp/langchain-requirements.txt
RUN python3.12 -m pip install -r /tmp/langchain-requirements.txt
RUN python3.12 -m pip install xmltodict

# yq for yaml -> json translation
# https://github.com/mikefarah/yq
# (There's a yq package for ubuntu24.04, but it doesn't support all the args we're using.)
#RUN wget https://github.com/mikefarah/yq/releases/download/${VERSION}/${BINARY}.tar.gz -O - |\
#  tar xz && mv ${BINARY} /usr/bin/yq
RUN wget https://github.com/mikefarah/yq/releases/download/v4.43.1/yq_linux_amd64.tar.gz -O - | \
  tar xz && mv yq_linux_amd64 /usr/bin/yq

# get github CLI
RUN curl -sS https://webi.sh/gh | sh

# Install mise (optional dependency installer for crs-sandbox/.tool-versions)
RUN curl https://mise.run | sh


# Now install a ccl.  (Based on https://github.com/daewok/lisp-devel-docker)
#ADD ccl-1.11.5-linuxx86.tar.gz /usr/local/src
ADD ${CRS_CONTEXT_PREFIX}/ccl-1.12.2-linuxx86.tar.gz /usr/local/src
ENV CCL_DEFAULT_DIRECTORY /usr/local/src/ccl
#   This is how daewok/lisp-devel does it.
# Poss counter-intuitive, but I do prefer making ccl64 the default.
RUN ln -s /usr/local/src/ccl/scripts/ccl64 /usr/local/bin/ccl && \
    ln -s /usr/local/src/ccl/scripts/ccl /usr/local/bin/ccl32

#   ccl1.11.5 includes asdf 3.1.5, but we need asdf 3.3 for launch-program
# Let's just poke a newer asdf straight into its distro...
#COPY asdf-3.3.1.6.lisp /usr/local/src/ccl/tools/
#RUN (cd /usr/local/src/ccl/tools/; mv asdf.lisp asdf-3.1.5.lisp; ln -s asdf-3.3.1.6.lisp asdf.lisp)

# Ubuntu comes with /bin/sh linked to dash; that can cause issues b/c doesnt support expected redirection syntax.
# web suggested just linking bash to it...but.. that seems wrong given diff b/w bourne shell expected syntax, but whatever.
RUN ln -f /bin/bash /bin/sh

# Here's the realuser stuff we have from other projects, to make the user ID same as who runs it (on cgc-submit)

RUN groupadd sift && useradd -m -g sift realuser && adduser realuser sudo

# Above makes a /home/realuser dir but it will have the wrong uid, b/c later when
# we use the realuser/gosu magic to get the uid of the person running the docker, this uid will (probably) be diff.
# Easiest workaround is to just open up perms on the docker homedir, which doesnt matter/get-saved.
# Note password for 'sudo' in docker image is sift .

# FIXME: These chmods are wrong: only the directories should get -x perms.
# This makes all files executable.  
RUN chmod -R 777 /home/realuser /usr/local/slime-2.30 && \
        echo root:sift | chpasswd

RUN chmod -R 777 /home/realuser /usr/local/sly-master && \
        echo root:sift | chpasswd

# this porks caching, do it late
RUN pip3 install --upgrade pip
RUN pip3 install --no-cache-dir numpy scipy requests dool \
	pyyaml xlsxwriter twisted cryptography pyasn1 dpkt python-levenshtein numpy scipy terminaltables scikit-learn pymodbus==2.2.0.rc4 \
    openlit && \
    rm -rf /root/.cache

# get Docker; sudo apt-get docker does not give you the thing you want; this does.
#	 per https://stackoverflow.com/questions/30379381/docker-command-not-found-even-though-installed-with-apt-get
# Do this late b/c it defeats caching
RUN curl -sSL https://get.docker.com/ | sh

#   Some realuser stuff we used to get from neo-fuzz.
#RUN groupadd sift && useradd -m -g sift realuser && adduser realuser sudo
RUN chmod -R 777 /home/realuser && \
        echo root:sift | chpasswd
ENTRYPOINT ["/bin/sh", "/realuser.sh"]
CMD ["/bin/bash"]

COPY ${CRS_CONTEXT_PREFIX}/realuser.sh /
COPY ${CRS_CONTEXT_PREFIX}/bashrc /home/realuser/.bashrc
COPY ${CRS_CONTEXT_PREFIX}/bashrc /root/.bashrc

ADD ${CRS_CONTEXT_PREFIX}/jc-1.25.3-linux-x86_64.tar.gz /tmp
RUN cp /tmp/jc /usr/local/bin/jc

COPY crs/code/afl-linux/harness /lacrosse/code/afl-linux/harness
RUN cargo build --release --manifest-path /lacrosse/code/afl-linux/harness/Cargo.toml && \
    install -Dt /usr/local/bin /lacrosse/code/afl-linux/harness/target/release/afl-linux-harness

COPY crs/code/grab-a-func /lacrosse/code/grab-a-func
RUN cargo build --release --manifest-path /lacrosse/code/grab-a-func/Cargo.toml && \
    install -Dt /usr/local/bin /lacrosse/code/grab-a-func/target/release/grab-a-func

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    splitpatch && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# This is not ideal but we don't have a better option.
# This is how we make our code available when running from
# within the k8s cluster.
COPY crs /lacrosse
# RUN touch /lacrosse/.is_docker

# Install Python 3.10 separately
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        python3.10 \
    	python3.10-dev \
    	python3.10-distutils && \
    rm -rf /var/lib/apt/lists/*

RUN which python3.10

# Ensure Python 3.12 remains the default
RUN update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.12 2 && \
    update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.10 1

# Verify Python 3.10 installation
RUN python3.10 --version
RUN pip3.10 --version

# Install Poetry
RUN curl -sSL https://install.python-poetry.org | python3.10 -

# Add Poetry to PATH
ENV PATH="/root/.local/bin:$PATH"

# Install the dependencies
RUN cd /lacrosse/code/dspy && poetry env use python3.10 && poetry install --no-interaction

RUN apt-get update && \
    apt-get install -y --no-install-recommends colorized-logs
