.PHONY: all
default: all

ANTLR=antlr4
NODEJS=node
PYTHON3=python3

ANTLR_FLAGS=-Dlanguage=Python3 -no-listener -visitor

JS_FILES=kernel.js errors.js builtins.js vm.js bytecodes.js
JS_FILES_EXPANDED=$(foreach js,$(JS_FILES),js/$(js))

ST_FILES=Kernel.st
ST_FILES_EXPANDED=$(foreach st,$(ST_FILES),st/$(st))

PYTHON_FILES=main.py bytecodes.py visitor.py

parser/SmalltalkVisitor.py: parser/Smalltalk.g4
	$(ANTLR) $(ANTLR_FLAGS) parser/Smalltalk.g4

all.js: js/*.js
	cat $(JS_FILES_EXPANDED) > $@

st.json: st/*.st $(PYTHON_FILES) parser/SmalltalkVisitor.py
	$(PYTHON3) main.py $(ST_FILES_EXPANDED)

all: all.js st.json

run: all FORCE
	$(NODEJS) run.js

watch:
	while true; do \
		make $(WATCHMAKE); \
		inotifywait -qre close_write .; \
	done

clean: FORCE
	rm -f all.js st.json parser/Smalltalk.{interp,tokens} parser/Smalltalk{Lexer,Parser,Visitor}.*

FORCE:

