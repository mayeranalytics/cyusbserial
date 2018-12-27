OS=$(shell uname | tr A-Z a-z)
ARCH=$(shell arch)
LD_LIBRARY_PATH=${PWD}/cypress/${OS}

all: build

run: all
ifeq ($(OS),darwin)
	DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH} stack exec cy-exe
else
	LD_LIBRARY_PATH=${LD_LIBRARY_PATH} stack --allow-different-user exec cy-exe
endif

run-prof: build-profile
ifeq ($(OS),darwin)
	DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH} stack exec -- cy-exe +RTS -p
else
	LD_LIBRARY_PATH=${LD_LIBRARY_PATH} stack exec --allow-different-user -- cy-exe +RTS -p
endif

build:
	stack build

build-profile:
	stack build --profile --executable-profiling --library-profiling

build-debug:
	stack build --no-strip --no-executable-stripping --no-library-stripping --ghc-options="-fPIC -fllvm -pgmlo opt -pgmlc llc -g"

test:
ifeq ($(OS),darwin)
	DYLD_LIBRARY_PATH=${LD_LIBRARY_PATH} stack test --extra-lib-dirs=${LD_LIBRARY_PATH}
else
	LD_LIBRARY_PATH=${LD_LIBRARY_PATH} stack test --allow-different-user --extra-lib-dirs=${LD_LIBRARY_PATH}
endif

clean:
	stack clean

code:
	stack build stylish-haskell hlint intero hoogle c2hs && \
	zsh -c -i "code ."

hoogle-server:
	stack hoogle -- server --local

haddock:
	stack haddock --open

tmp/%.hs: src/System/Cypress/%.chs
	c2hs -C"-Icypress/$(OS)" $< -o $@

.PHONY: build run test build-profile clean code hoogle-server haddock
