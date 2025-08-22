FROM ubuntu:bionic

######################################################################
#  
# First part of this dockerfile builds an Angr image.
# Second part adds Driller and SIFT's customizations.
######################################################################

######################################################################
# Begin angr dockerfile
######################################################################
#RUN dpkg --add-architecture i386
#	python3-dev python3-pip python3-wheel virtualenv python3-venv \
#        python3 python3-dev python3-setuptools python-is-python3 \
#	libreadline-dev libtool debootstrap debian-archive-keyring libglib2.0-dev \
#    	build-essential curl cmake ssh ca-certificates &&\
#    apt-get -y autoremove && \
#    rm -rf /var/lib/apt/lists/* && \
#    rm -rf /var/cache/apk/* && \
#    rm -rf /root/.cache
#
#

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    python3-dev python3-pip python3-wheel virtualenv python3-venv \
    build-essential libxml2-dev libxslt1-dev git libffi-dev cmake \
    libreadline-dev libtool debootstrap debian-archive-keyring libglib2.0-dev \
    libpixman-1-dev qtdeclarative5-dev binutils-multiarch nasm \
    libssl-dev \
    automake gdb gdb-multiarch binfmtc \
    gosu libz-dev libpcap-dev libnet1-dev libnids-dev \
    python3-pygraphviz python3-pip python3-dev \
    python3-setuptools build-essential libxml2-dev libxslt1-dev git libffi-dev \
    cmake libreadline-dev libtool debootstrap debian-archive-keyring \
    libglib2.0-dev libpixman-1-dev qtdeclarative5-dev binutils-multiarch nasm \
    libssl-dev libssl1.0.0 libssl1.1 openssl curl \
    less psmisc \ 
    sudo parallel && \
    apt-get remove -y emacs humanity-icon-theme && \
    apt-get -y autoremove && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /var/cache/apk/* && \
    rm -rf /root/.cache
#
# 20190731 MAD - Modifying this section from the angr dockerfile to add a script
# to rollback the repos to the known working date before installing to ensure
# new, breaking dependencies are not installed. The new version of claripy
# was pulling a new version of z3 that was breaking with the older version
# of angr we are using.
#COPY rollback-libs.sh /rollback-libs.sh
#COPY angr-targets-setup.py /angr-targets-setup.py

RUN useradd -s /bin/bash -m angr

# 20190805 MAD - Remove the binaries directory before this layer is finished
# Some of the binaries set off trojan scans
## 20200827 PMW - Use archived copy of angr-dev to prevent errors with cloning
#COPY angr-dev.tar.gz /home/angr
#RUN su - angr -c "tar -xzvf angr-dev.tar.gz"
#RUN su - angr -c "cd angr-dev && ./setup.sh -C -w -e angr && /rollback-libs.sh"
#RUN su - angr -c "cd angr-dev && ./setup.sh -w -e angr "
#RUN su - angr -c "cd angr-dev && ./setup.sh -w -p angr-pypy"
#RUN su - angr -c "rm -rf /home/angr/.cache && rm -rf /home/angr/angr-dev/binaries && rm -rf /home/angr/angr-dev/angr-doc/"
#
#RUN su - angr -c "echo 'workon angr' >> /home/angr/.bashrc"
#CMD su - angr

######################################################################
# End angr dockerfile
######################################################################

#USER root
#
#RUN pip3 install --upgrade pip && \
#    pip3 install --no-cache-dir \
#    wheel amqp==2.6.1 numpy terminaltables scipy python-levenshtein z3-solver==4.11.2.0 \
#    dpkt ana angr==8.20.7.27 cle==8.20.7.27 claripy IPython pyelftools==0.26 pyyaml nose flaky \
#    amqp==2.6.1 celery==4.4.7 configparser==5.0.0 GitPython==3.1.8 ipdb==0.13.3 \
#    isort==5.5.3 kombu==4.6.11 posix-ipc==1.0.4 prompt-toolkit==3.0.7 protobuf==3.19.4 \
#    pylink-square==0.8.0 tqdm==4.49.0 unicorn==1.0.2rc5 vine==1.3.0 zipp==3.2.0 && \
#    capstone==5.0.0 \
#    Cython==0.29.32 \
#    decorator==5.1.1 \
#    future==0.18.2 \
#    joblib==1.1.0 \
#    plumbum==1.7.2 \
#    psutil==5.9.2 \
#    pygdbmi==0.10.0.2 \ 
#    Pygments==2.13.0 \
#    pygobject==3.26.1 \  
#    pytz==2022.2.1 \ 
#    sklearn==0.0 \
#    threadpoolctl==3.1.0 \
#    wcwidth==0.2.5 \
#    rm -rf /root/.cache

#RUN curl -sSL https://get.docker.com/ | sh

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

#RUN groupadd sift \
#    && useradd -m -g sift realuser
#RUN chmod -R 777 /home/realuser && \
#        echo root:sift | chpasswd
#ENTRYPOINT ["/bin/sh", "/realuser.sh"]
#CMD ["/bin/bash"]


# get Docker; sudo apt-get docker does not give you the thing you want; this
# does.
# https://stackoverflow.com/questions/30379381/docker-command-not-found-even-though-installed-with-apt-get

# Copy new custom version of driller and tracer code
# and reset them. This installs newly developed packages.
# Installs new dependecies.
#COPY re-setup-py-libs.sh /re-setup-py-libs.sh
#COPY driller-code /home/angr/angr-dev/driller
#COPY tracer-code /home/angr/angr-dev/tracer
#
#USER root
#RUN /re-setup-py-libs.sh
## this replicates setup in Matt's mdehavensift/driller:20171001
#RUN echo 'angr ALL=NOPASSWD: ALL' >> /etc/sudoers.d/angr
#
#RUN . /home/angr/.virtualenvs/angr/bin/activate && \
#    python3 -m pip install --no-cache-dir /home/angr/angr-dev/wheels/shellphish_afl-1.2.1-py2.py3-none-manylinux1_x86_64.whl && \
#    rm -rf /root/.cache && \
#    rm -rf /home/angr/.cache
#
#RUN . /home/angr/.virtualenvs/angr/bin/activate && \
#    python3 -m pip install --no-cache-dir psutil tqdm==4.49.0 shellphish-qemu==0.9.8 z3-solver==4.5.1.0.post2 sortedcontainers==2.1.0 && \
#    rm -rf /root/.cache && \
#    rm -rf /home/angr/.cache
#
#USER root
#COPY core /proc/sys/kernel/core_pattern
