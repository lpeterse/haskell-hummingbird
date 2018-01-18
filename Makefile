.PHONY: all build test doc dist

VERSION           := $(shell grep -oP '^version:\s+\K([0-9].[0-9].[0-9].[0-9])' hummingbird.cabal)
PACKAGE           := hummingbird
DEBFILE           := ${PACKAGE}_${VERSION}-1_amd64.deb

all: build test doc

build:
	stack build

test:
	stack test

doc:
	stack haddock

dist:
	echo "${DEBFILE} unknown optional" > debian/files
	docker build --rm -t hummingbird .
	docker run --rm --entrypoint cat hummingbird /${DEBFILE} > ${DEBFILE}
	docker rmi hummingbird
