.PHONY: all clean build build-win test install uninstall

PACKAGE_NAME = arena

ifeq ($(OS),Windows_NT)
    # MSYS2/MinGW
    ifneq (,$(findstring MINGW,$(shell uname -s)))
        WINDOWS_NATIVE = 0
    else
        WINDOWS_NATIVE = 1
    endif
else
    WINDOWS_NATIVE = 0
endif

all: build

build-win:
	cd src\rust && cargo build --release
	if not exist native mkdir native
	copy src\rust\target\release\arena.dll native\
	copy src\rust\target\release\arena.lib native\
	del /Q native\*.d native\*.exp native\*.pdb 2>nul || echo No files to delete.

build:
ifeq ($(WINDOWS_NATIVE),1)
	$(MAKE) build-win
else
	cd src/rust && cargo build --release
	mkdir -p native
	cp src/rust/target/release/libarena.* native/
	rm -f native/*.d native/*.exp native/*.pdb
endif

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
