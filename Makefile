.PHONY: all
default: all

# These parser files are GNU Smalltalk files for the new, smarter parser.
PARSER_FILES=parser/ast.st parser/parser.st compiler/core.st \
						 compiler/visitor.st compiler/macros.st compiler/driver.st \
						 compiler/main.st
PARSER=gst $(PARSER_FILES) -a

ST_FILES=st/Kernel.st st/Exceptions.st \
				 st/Collection.st st/Collections-Array.st st/Collections-Unordered.st \
				 st/Streams.st st/Strings.st st/Collections-LinkedList.st \
				 st/Collections-Interval.st st/SUnit.st st/Fractions.st \
				 st/Process.st st/Canvas.st st/Morphic.st
ST_TESTS=st/tests/Basics.st st/tests/Collections.st st/tests/Dictionaries.st \
				 st/tests/SequenceableArrays.st st/tests/SequenceableOCs.st \
				 st/tests/OrderedCollections.st st/tests/SortedCollections.st \
				 st/tests/Fractions.st st/tests/Morphic.st st/tests/Processes.st

plain.bin: st/*.st st/tests/*.st $(PARSER_FILES)
	$(PARSER) plain.bin $(ST_FILES)

testing.bin: st/*.st st/tests/*.st $(PARSER_FILES)
	$(PARSER) testing.bin $(ST_FILES) $(ST_TESTS)

all: plain.bin

test: testing.bin

clean: FORCE
	rm -f plain.bin testing.bin

FORCE:

