.PHONY: all clean build test install uninstall

PACKAGE_NAME = arena

all: build

build:
	cd src/rust && cargo build --release
	mkdir -p native
	cp src/rust/target/release/libarena.* native/
	rm -f native/libarena.d

check: all
	racket examples/low-layer.rkt
	racket examples/high-layer.rkt

performance-check: all
	racket examples/performance-check.rkt

test: check

install: all
	raco pkg install --deps search-auto --name $(PACKAGE_NAME) --link $(PWD)

uninstall:
	raco pkg remove $(PACKAGE_NAME)

reinstall: uninstall install

setup: install
	raco setup $(PACKAGE_NAME)

dev-setup:
	cd $(PWD) && raco pkg install --deps search-auto --link

clean: clean-build
	rm -rf native/*
	rm -rf compiled/

clean-build:
	cd src/rust && cargo clean
	rm -f native/libarena.d
