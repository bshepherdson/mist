.PHONY: all
default: all

NODEJS=node
GO=go

ST_FILES=st/Kernel.st st/Exceptions.st \
				 st/Collection.st st/Collections-Array.st st/Collections-Unordered.st \
				 st/SUnit.st
ST_TESTS=st/tests/Basics.st

all.js: js/*.js
	cat $(JS_FILES) > $@

mist: *.go parser/*.go
	$(GO) build

plain.json: st/*.st mist
	./mist $(ST_FILES)
	mv st.json plain.json

testing.json: st/*.st st/tests/*.st mist
	./mist $(ST_FILES) $(ST_TESTS)
	mv st.json testing.json

all: plain.json

run: all FORCE
	$(NODEJS) run.js

test: testing.json

watch:
	while true; do \
		make $(WATCHMAKE); \
		inotifywait -qre close_write .; \
	done

clean: FORCE
	rm -f all.js st.json mist

FORCE:

