# NOTE you cannot use inline comments-- do not put a comment at end of line, you'll get a very obscure error.
FROM python:3.12.3

# Need this for various things below to configure non-interactively.
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      curl tree jq


# Make sure some of our packages are up to date.
#
RUN pip install --no-cache --upgrade pip
RUN pip install --no-cache --upgrade setuptools wheel "Cython<3" numpy
RUN pip install --no-cache --upgrade poetry
RUN poetry self add poetry-plugin-export

RUN echo doit8

# Update Poetry and its internal dependencies.
#
#RUN /home/python/.local/bin/poetry self update
RUN poetry self update
ADD pyproject.toml /build/pyproject.toml
ADD poetry.lock /build/poetry.lock

WORKDIR /build


# Install everything we need for our project.
RUN poetry export -o requirements.txt
RUN pip install -r requirements.txt

# FIXME: could potentially dump poetry at this point; we won't use it.


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
