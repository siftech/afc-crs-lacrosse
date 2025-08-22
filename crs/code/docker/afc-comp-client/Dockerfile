FROM python:3.12.3

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      curl tree jq

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

COPY requirements.txt /usr/src/app/

RUN pip3 install --no-cache-dir -r requirements.txt

COPY . /usr/src/app

EXPOSE 8080

# Here's the realuser stuff we have from other projects, to make the user ID same as who runs it (on cgc$
RUN groupadd sift && useradd -m -g sift realuser && adduser realuser sudo

RUN chmod -R 777 /home/realuser && \
        echo root:sift | chpasswd

# this stuff often changes, so do it near-last to leverage caching
COPY realuser.sh bashrc /
RUN ln -s -f /bashrc /root/.bashrc && ln -s -f /bashrc /home/realuser/.bashrc

# ENTRYPOINT ["python3"]
# CMD ["-m", "openapi_server"]