FROM        ubuntu:16.04
MAINTAINER  Lars Petersen <info@lars-petersen.net>

RUN         DEBIAN_FRONTEND=noninteractive apt-get update && \
            DEBIAN_FRONTEND=noninteractive apt-get install -y \
             locales \
             build-essential \
             curl \
             libgmp-dev \
             git-core \
             libtinfo-dev \
             debhelper \
             dh-make \
             quilt \
             fakeroot \
             lintian \
             git-core \
             zlib1g-dev && \
            locale-gen en_US.UTF-8 && \
            ln -s /lib/x86_64-linux-gnu/libtinfo.so.5 /lib/x86_64-linux-gnu/libtinfo.so

WORKDIR     /root
ENV         PATH="/root/.local/bin:${PATH}"
ENV         LANG en_US.UTF-8
ENV         LANGUAGE en_US:en
ENV         LC_ALL en_US.UTF-8

RUN         mkdir -p ~/.local/bin && \
            curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack' && \
            chmod a+x ~/.local/bin/stack && \
            stack update
COPY        stack.yaml .
RUN         stack setup --verbose 2>&1

COPY        hummingbird.cabal .
RUN         stack install --only-dependencies 2>&1

COPY        src src
COPY        app app
COPY        resources resources
COPY        LICENSE .
COPY        CHANGELOG.md .

RUN         stack install 2>&1

# Debian package assembly
COPY        debian debian
RUN         dpkg-buildpackage -b -us -uc && ls -la
RUN         dpkg-deb --info ../hummingbird_*.deb

# Install the package (testwise)

RUN         dpkg -i ../hummingbird_*.deb