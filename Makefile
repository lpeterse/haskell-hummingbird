.PHONY: all build test install doc

all: build test install

build:
	stack build

doc:
	stack haddock

test:
	stack test

install:
	sudo sh install.sh
