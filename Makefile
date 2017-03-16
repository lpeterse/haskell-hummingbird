.PHONY: all build test install doc

all: build test install

build:
	stack build

build-docker:
	docker build --rm -t hummingbird .
	docker run --rm --entrypoint cat hummingbird /hummingbird_0.1.0.0-1_amd64.deb > hummingbird_0.1.0.0-1_amd64.deb
	docker rmi hummingbird

doc:
	stack haddock

test:
	stack test

install:
	sudo sh install.sh
