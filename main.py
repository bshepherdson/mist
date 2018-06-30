import json
import sys
from antlr4 import *
from antlr4.InputStream import InputStream

from bytecodes import BytecodeStream
from parser.SmalltalkLexer import SmalltalkLexer
from parser.SmalltalkParser import SmalltalkParser
from visitor import MistVisitor

if __name__ == '__main__':
  input = FileStream(sys.argv[1])
  lexer = SmalltalkLexer(input)
  stream = CommonTokenStream(lexer)
  stream.fill()
  #print([ SmalltalkLexer.symbolicNames[t.type] for t in stream.getTokens(0, 1000) ])
  stream.reset()
  parser = SmalltalkParser(stream)

  tree = parser.script()

  visitor = MistVisitor()
  result = visitor.visit(tree)

  out = []
  for cls in result.values():
    out.append(cls.compile())

  print(json.dumps(out))
