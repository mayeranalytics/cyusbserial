OS=$(shell uname | tr A-Z a-z)
ARCH=$(shell arch)
DIR=${PWD}/../cypress/${OS}

all: bin/cy1 bin/cy2 bin/read-config bin/chksum

bin/%.o:%.c
	gcc -c $< -I$(DIR) -o$@

bin/%: bin/%.o
	gcc $^ -L$(DIR) -Wl,-rpath,$(DIR) -lcyusbserial -lusb-1.0 -o$@

bin/read-config: bin/extras.o
bin/chksum: bin/extras.o
bin/extras.o: extras.h

run: bin/cy1
ifeq ($(OS),darwin)
	DYLD_LIBRARY_PATH=$(DIR) $<
else
	LD_LIBRARY_PATH=$(DIR) $<
endif

run2: bin/cy2
ifeq ($(OS),darwin)
	DYLD_LIBRARY_PATH=$(DIR) $<
else
	LD_LIBRARY_PATH=$(DIR) $<
endif

read-config: bin/read-config
ifeq ($(OS),darwin)
	DYLD_LIBRARY_PATH=$(DIR) $<
else
	LD_LIBRARY_PATH=$(DIR) $<
endif

clean:
	rm -rf bin/*

.PHONY: clean run
