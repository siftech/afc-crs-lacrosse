FROM ubuntu:22.04

RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
      autoconf \
      automake \
      build-essential \
      cmake \
      curl \
      git \
      libmpfr-dev \
      libgmp-dev \
      libsqlite3-dev \
      make \
      ninja-build \
      opam \
      openjdk-11-jdk-headless \
      pkg-config \
      python3 \
      python3-pip \
      sqlite3 \
      zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/facebook/infer.git /tmp/infer \
    && cd /tmp/infer \
    && git checkout da475786768dd22f29cb13fe09a81f0e386a5381 \
    && ./build-infer.sh java -y \
    && make install \
    && rm -r /tmp/infer
  
ENV PATH /usr/local/lib/infer/facebook-clang-plugins/clang/install/bin:${PATH}

WORKDIR /workspace
COPY ./infer/.inferconfig /workspace/.inferconfig
