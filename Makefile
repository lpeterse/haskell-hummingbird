.PHONY: all build test install doc

all: build test install

build:
	stack build

build-docker:
	docker build --rm -t hummingbird .
	docker run --rm --entrypoint cat hummingbird  /root/.local/bin/hummingbird > hummingbird
	docker rmi hummingbird

doc:
	stack haddock

test:
	stack test

install:
	sudo sh install.sh
