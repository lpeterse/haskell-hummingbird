#!/bin/bash

set -e
set -x

SERVICE=hummingbird
USER=hummingbird
GROUP=hummingbird

DIR_BIN="/usr/sbin"
DIR_ETC="/etc/$SERVICE"
DIR_RUN="/var/run/$SERVICE"
DIR_HOME="/var/lib/$SERVICE"
DIR_SYSTEMD="/etc/systemd/system"

# Create user and group (if not exists)

if id "$USER" >/dev/null 2>&1; then
  true
else
  useradd --home-dir $DIR_HOME --no-create-home --system --shell /usr/bin/false --user-group $USER
fi

mkdir -p $DIR_RUN $DIR_ETC $DIR_HOME

chown root:root $DIR_ETC
chown $USER:$GROUP $DIR_HOME $DIR_RUN
chmod 755 $DIR_ETC
chmod 750 $DIR_HOME $DIR_RUN

# Copy systemd unit file

sudo cp hummingbird.service $DIR_SYSTEMD/hummingbird.service
sudo chown root:root $DIR_SYSTEMD/hummingbird.service
sudo chmod 644 $DIR_SYSTEMD/hummingbird.service
sudo systemctl daemon-reload

# Copy binary

cp .stack-work/dist/*/Cabal-*/build/hummingbird/hummingbird $DIR_BIN/$SERVICE
cp .stack-work/dist/*/Cabal-*/build/hummingbird-cli/hummingbird-cli $DIR_BIN/hummingbird-cli
chown root:root $DIR_BIN/$SERVICE
chmod 755 $DIR_BIN/$SERVICE

# Create default settings file (if not exists)

if [ ! -f DIR_ETC/settings.yml ]; then
  SETTINGS=$DIR_ETC/settings.yml
  cp settings.yml $SETTINGS
  chown root:root $SETTINGS
  chmod 755 $SETTINGS
fi
