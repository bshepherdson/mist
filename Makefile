.PHONY: all
default: all

NODEJS=node
GO=go

JS_FILES=kernel.js errors.js builtins.js vm.js bytecodes.js
JS_FILES_EXPANDED=$(foreach js,$(JS_FILES),js/$(js))

ST_FILES=Kernel.st Exceptions.st \
				 Collection.st Collections-Array.st Collections-Unordered.st \
				 SUnit.st Test.st
ST_FILES_EXPANDED=$(foreach st,$(ST_FILES),st/$(st))

all.js: js/*.js
	cat $(JS_FILES_EXPANDED) > $@

mist: *.go parser/*.go
	$(GO) build

st.json: st/*.st mist
	./mist $(ST_FILES_EXPANDED)

all: st.json

run: all FORCE
	$(NODEJS) run.js

watch:
	while true; do \
		make $(WATCHMAKE); \
		inotifywait -qre close_write .; \
	done

clean: FORCE
	rm -f all.js st.json mist

FORCE:

