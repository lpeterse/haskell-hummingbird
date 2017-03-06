FROM        ubuntu:16.04
MAINTAINER	Lars Petersen <info@lars-petersen.net>

RUN locale-gen en_US.UTF-8
ENV         PATH="/root/.local/bin:${PATH}"
ENV         LANG en_US.UTF-8
ENV         LANGUAGE en_US:en
ENV         LC_ALL en_US.UTF-8

RUN         DEBIAN_FRONTEND=noninteractive apt-get update && \
            DEBIAN_FRONTEND=noninteractive apt-get install -y \
             build-essential \
             curl \
             libgmp-dev \
             git-core \
             libtinfo-dev && \
            mkdir -p ~/.local/bin && \
            curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack' && \
            chmod a+x ~/.local/bin/stack

COPY        . .

RUN         ln -s /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libtinfo.so
RUN         stack setup
RUN         stack install
RUN         stack test
