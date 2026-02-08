.PHONY: build clean test install

build:
	gerbil build

clean:
	gerbil clean

test:
	gerbil test tests/...

install:
	gerbil build
