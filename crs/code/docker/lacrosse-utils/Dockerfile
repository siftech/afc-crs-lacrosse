# NOTE you cannot use inline comments-- do not put a comment at end of line, you'll get a very obscure error.
FROM ubuntu:24.04

# Need this for various things below to configure non-interactively.
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
                    build-essential curl rsync \
                    tree git jq jc wget ca-certificates \
                    make ssh

# yq for yaml -> json translation
# https://github.com/mikefarah/yq
# (There's a yq package for ubuntu24.04, but it doesn't support all the args we're using.)
#RUN wget https://github.com/mikefarah/yq/releases/download/${VERSION}/${BINARY}.tar.gz -O - |\
#  tar xz && mv ${BINARY} /usr/bin/yq
RUN wget https://github.com/mikefarah/yq/releases/download/v4.43.1/yq_linux_amd64.tar.gz -O - | \
  tar xz && mv yq_linux_amd64 /usr/bin/yq

# get Docker; sudo apt-get docker does not give you the thing you want; this does.
#        per https://stackoverflow.com/questions/30379381/docker-command-not-found-even-though-installed-with-apt-get
# Do this late b/c it defeats caching
RUN curl -sSL https://get.docker.com/ | sh

# get github CLI
RUN curl -sS https://webi.sh/gh | sh

# Install Kompose
RUN curl -L https://github.com/kubernetes/kompose/releases/download/v1.33.0/kompose-linux-amd64 -o kompose
RUN chmod +x kompose
RUN mv ./kompose /usr/local/bin/kompose

# Install Kubernetes Kind
RUN [ $(uname -m) = x86_64 ] && curl -Lo ./kind https://kind.sigs.k8s.io/dl/v0.23.0/kind-linux-amd64
RUN chmod +x ./kind
RUN mv ./kind /usr/local/bin/kind

# Install mise (optional dependency installer for crs-sandbox/.tool-versions)
RUN curl https://mise.run | sh

RUN rm -rf /var/lib/apt/lists/*

################################ REALUSER STUFF ################################
# this is mainly included at SIFT for bashrc ending in echo that helps
# PRT detection of container being 'up'.  No longer sure how it affects rooted docker
################################################################################

RUN curl -L https://github.com/tianon/gosu/releases/download/1.10/gosu-amd64 \
	-o /usr/local/bin/gosu
RUN chmod +x /usr/local/bin/gosu
RUN groupadd sift && useradd -mg sift realuser
RUN chmod -R 777 /home/realuser && echo root:sift | chpasswd
COPY realuser.sh /
COPY bashrc /home/realuser/.bashrc
COPY bashrc /root/.bashrc
ENTRYPOINT ["/bin/sh", "/realuser.sh"]
