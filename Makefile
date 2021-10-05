.PHONY: all
default: all

NODEJS=node
GO=go

ST_FILES=st/Kernel.st st/Exceptions.st \
				 st/Collection.st st/Collections-Array.st st/Collections-Unordered.st \
				 st/Strings.st st/SUnit.st
ST_TESTS=st/tests/Basics.st st/tests/Collections.st

mist: *.go parser/*.go
	$(GO) build

plain.bin: st/*.st mist
	./mist $(ST_FILES)
	mv st.bin plain.bin

testing.bin: st/*.st st/tests/*.st mist
	./mist $(ST_FILES) $(ST_TESTS)
	mv st.bin testing.bin

all: plain.bin

test: testing.bin

watch:
	while true; do \
		make $(WATCHMAKE); \
		inotifywait -qre close_write .; \
	done

clean: FORCE
	rm -f st.bin plain.bin testing.bin mist

FORCE:

