.PHONY: all
default: all

ANTLR=antlr4
NODEJS=node
PYTHON3=python3
GO=go

ANTLR_FLAGS=-Dlanguage=Go
ANTLR_FLAGS_PYTHON=-Dlanguage=Python3 -no-listener -visitor

JS_FILES=kernel.js errors.js builtins.js vm.js bytecodes.js
JS_FILES_EXPANDED=$(foreach js,$(JS_FILES),js/$(js))

ST_FILES=Kernel.st Exceptions.st Collection.st Collections-Array.st Collections-Unordered.st \
	 SUnit.st
ST_FILES_EXPANDED=$(foreach st,$(ST_FILES),st/$(st))

PYTHON_FILES=main.py bytecodes.py visitor.py
GO_FILES=bytecodes.go main.go listener.go

parser/SmalltalkVisitor.py: parser/Smalltalk.g4
	$(ANTLR) $(ANTLR_FLAGS_PYTHON) parser/Smalltalk.g4

parser/smalltalk_listener.go: parser/Smalltalk.g4
	$(ANTLR) $(ANTLR_FLAGS) parser/Smalltalk.g4

all.js: js/*.js
	cat $(JS_FILES_EXPANDED) > $@

mist: $(GO_FILES) parser/smalltalk_listener.go
	$(GO) build

st.json: st/*.st mist
	./mist $(ST_FILES_EXPANDED)

all: all.js st.json

run: all FORCE
	$(NODEJS) run.js

watch:
	while true; do \
		make $(WATCHMAKE); \
		inotifywait -qre close_write .; \
	done

clean: FORCE
	rm -f all.js st.json parser/Smalltalk.{interp,tokens} mist parser/Smalltalk{Lexer,Parser,Visitor}.* parser/smalltalk_*.go

FORCE:

