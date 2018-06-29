import sys
from bytecodes import *
from parser.SmalltalkVisitor import SmalltalkVisitor

# Most of the legwork in the Smalltalk system is done at the level of message
# sends.
# Messages are sent to a receiver and have a symbol identifying them, and some
# argument expressions.
# Cascades are a series of messages all sent to the same receiver.

# The *Message rules are the ones that really do a message send, assuming their
# receiver is already on the stack.
# The only wrinkle there is when their receiver is the special word "super",
# which changes the next message sent to use a different bytecode.

# We can check this at the level of unarySend, binarySend, etc. - if the
# receiver the inner send/operand returns is Super, handle it accordingly.

KIND_GLOBAL = 'global'
KIND_TEMP = 'temp'
KIND_ARG = 'arg'
KIND_INST_VAR = 'instvar'

# Base class for all singular operands:
# - self, super, nil, true, false
# - variable names
# - blocks
# - dynamic arrays and dictionaries
# - number literals
# - character constants
# - literal arrays
# - string literals
# - symbols

class PBase:
  def compile(self, stream):
    raise Exception(
        "compile() not implemented in {}".format(type(self).__name__))

  def isSuper(self):
    return False

  def __str__(self):
    return self.__class__.__name__ + ": " + str(self.__dict__)

class POperand(PBase):
  pass


class PSuper(POperand):
  def isSuper(self):
    return True

  def compile(self, stream):
    raise Exception("Cannot compile() super directly")


class PSelf(POperand):
  def compile(self, s):
    s.add(BCPushSelf())

class PReference(POperand):
  def __init__(self, name, kind):
    self.name = name
    self.kind = kind

  def compile(self, s):
    if self.kind == KIND_GLOBAL:
      s.add(BCPushGlobal(self.name))
    elif self.kind == KIND_TEMP or self.kind == KIND_ARG:
      s.add(BCPushLocal(self.name))
    elif self.kind == KIND_INST_VAR:
      s.add(BCPushInstVar(self.name))
    else:
      raise Exception("Unknown reference type: " + self.kind)

class PBlock(POperand):
  # Takes an array of arg names (no colons) and an array of temp names.
  # BytecodeStream includes add() and the code field.
  def __init__(self, args=[], temps=[], code=[]):
    self.args = args
    self.temps = temps
    self.code = code

  def compile(self, s):
    # First, emit my own code into a side BytecodeStream.
    stream = BytecodeStream()
    for c in self.code:
      c.compile(stream)

    # Emit the start-block bytecode first.
    s.add(BCStartBlock(len(self.args), len(stream.contents)))
    s.contents.extend(stream.contents)

class PDynArray(POperand):
  def __init__(self):
    self.elements = [] # An array of the PBases in the array. They can be
                       # arbitrarily complex expressions, then we append.

  def push(self, el):
    self.elements.append(el)

  def compile(self, s):
    # We construct our Array first, then for each expression we dup it, compile
    # the expression, and call #add:.
    s.add(BCPushGlobal("Array"))
    s.add(BCSend("new", 1))

    for el in self.elements:
      s.add(BCDup())
      el.compile(s)
      s.add(BCSend("add:", 2))

    # Now the Array is still on the stack, which is what we want.


class PDynDict(POperand):
  def __init__(self):
    self.elements = []

  def push(self, key, val):
    self.elements.append((key, val))

  def compile(self, s):
    # Similar to arrays: construct the Dictionary, then dup, compile, #at:put:
    s.add(BCPushGlobal("Dictionary"))
    s.add(BCSend("new", 1))

    for key, val in self.elements:
      s.add(BCDup())
      key.compile(s)
      val.compile(s)
      s.add(BCSend("at:put:", 3))

    # We've left the dictionary on the stack, as intended.


class PLiteral(POperand):
  """Pushes a literal straight onto the stack: strings, numbers."""
  def __init__(self, value):
    self.value = value

  def compile(self, s):
    s.add(BCPushLiteral(self.value))

class PNumber(PLiteral):
  pass
class PString(PLiteral):
  pass

class PSymbol(PLiteral):
  def compile(self, s):
    super().compile(s)
    s.add(BCSend("asSymbol", 2))

class PChar(PLiteral):
  def compile(self, s):
    s.add(BCPushGlobal("Character"))
    super().compile(s)
    s.add(BCSend("fromString:", 2))



class PSend(PBase):
  def __init__(self, receiver, selector, args=[]):
    self.receiver = receiver
    self.selector = selector
    self.args = args

  def compile(self, s):
    # Compile the receiver first, then each argument, then the send.
    # If the receiver is super, do the special send.
    self.receiver.compile(s)
    for a in self.args:
      a.compile(s)

    if self.receiver.isSuper():
      s.add(BCSuperSend(self.selector, len(self.args) + 1))
    else:
      s.add(BCSend(self.selector, len(self.args) + 1))


class PCascade(PBase):
  def __init__(self, receiver, tails):
    """Tails is [(selector, [arg])]."""
    self.receiver = receiver
    self.tails = tails

  def compile(self, s):
    # Push the code for the receiver first.
    # Then we do "DROP DUP tail" for each tail, skipping the drop on the first
    # one and dup on the last.
    # That leaves the last result on the stack as the final value.
    self.receiver.compile(s)
    for ix, t in enumerate(self.tails):
      if ix > 0:
        s.add(BCDrop())

      if ix + 1 < len(self.tails):
        s.add(BCDup())

      for a in t[1]:
        a.compile(s)
      s.add(BCSend(t[0], len(t[1]) + 1))


class PAssignment(PBase):
  def __init__(self, name, kind, value):
    self.name = name
    self.kind = kind
    self.value = value

  def compile(self, s):
    # First compile the expression on the right.
    self.value.compile(s)
    # Then compile the store.
    if self.kind == KIND_TEMP:
      s.add(BCStoreLocal(self.name))
    else:
      s.add(BCStoreInstVar(self.name))


class PStatement(PBase):
  """A PStatement is an expression followed by a drop, unless the expression is
  a PAssignment (since it already consumes the value."""
  def __init__(self, expr):
    self.expr = expr
    self.noDrop = False

  def compile(self, s):
    self.expr.compile(s)
    if not self.noDrop and not isinstance(self.expr, PAssignment):
      s.add(BCDrop())


class PAnswer(PBase):
  """A PAnswer represents an answer. Needs to know whether it's inside a
  block."""
  def __init__(self, expr, inBlock):
    self.expr = expr
    self.inBlock = inBlock

  def compile(self, s):
    self.expr.compile(s) # The answer is on the stack now.
    if self.inBlock:
      s.add(BCAnswerBlock())
    else:
      s.add(BCAnswer())

class PSequence(PBase):
  def __init__(self, temps=[], code=[]):
    self.temps = temps
    self.code = code

  def compile(self, s):
    for c in self.code:
      c.compile(s)


class PMethod(PBase):
  def __init__(self, selector, args, temps, code):
    self.selector = selector
    self.args = args
    self.temps = temps
    self.code = code

  def compile(self, s):
    self.stream = BytecodeStream()
    for c in self.code:
      c.compile(self.stream)

    s.add(self)

  def emit(self):
    return {
        "method": self.selector,
        "args": self.args,
        "temps": self.temps,
        "code": self.stream.contents,
        }


def tokensToString(tokens):
  return "".join([t.getText() for t in tokens])


class MistVisitor(SmalltalkVisitor):
  def __init__(self):
    self.scope = Scope()
    self.blockDepth = 0

    # TODO: Add globals here - builtin classes?

  def popScope(self):
    self.scope = self.scope.parent

  def pushScope(self):
    self.scope = self.scope.sub()

  def error(self, token, msg):
    sym = token.getSymbol()
    filename = sym.getInputStream().fileName
    print("{:s} {:d}:{:d}  {:s}".format(filename, sym.line,
      sym.column, msg))
    sys.exit(1)


  # method : methodHeader sequence;
  # unaryHeader : unarySelector;                       -> (sel, [args])
  # binaryHeader : BINARY_SELECTOR ws? IDENTIFIER;     -> (sel, [args])
  # keywordHeader : (keywordHeaderPair ws_oneline?)+;  -> (sel, [args])
  # keywordHeaderPair : KEYWORD ws? IDENTIFIER;        -> (keyword, arg)
  # methodHeader : (keywordHeader | binaryHeader | unaryHeader) ws_oneline? NEWLINE;
  def visitUnaryHeader(self, ctx):
    sel = self.visit(ctx.unarySelector())
    return (sel, [])

  def visitBinaryHeader(self, ctx):
    sel = ctx.BINARY_SELECTOR().getText()
    arg = ctx.IDENTIFIER().getText()
    return (sel, [arg])

  def visitKeywordHeaderPair(self, ctx):
    return (ctx.KEYWORD().getText(), ctx.IDENTIFIER().getText())

  def visitKeywordHeader(self, ctx):
    pairs = ctx.keywordHeaderPair()
    keyword = ""
    args = []
    for p in pairs:
      p2 = self.visit(p)
      keyword += p2[0]
      args.append(p2[1])
    return (keyword, args)

  def visitMethodHeader(self, ctx):
    if ctx.keywordHeader() is not None:
      return self.visit(ctx.keywordHeader())
    elif ctx.binaryHeader() is not None:
      return self.visit(ctx.binaryHeader())
    elif ctx.unaryHeader() is not None:
      return self.visit(ctx.unaryHeader())

  def visitMethod(self, ctx):
    header = self.visit(ctx.methodHeader())
    self.pushScope()
    for arg in header[1]:
      self.scope.add(arg, KIND_ARG)
    seq = self.visit(ctx.sequence())
    self.popScope()
    return PMethod(header[0], header[1], seq.temps, seq.code)

  def visitScript(self, ctx):
    return self.visit(ctx.method())


  # sequence: temps ws? statements? | ws? statements;   -> ([temp_names], [stmt])
  def visitSequence(self, ctx):
    temps = []
    if ctx.temps() is not None:
      temps = self.visit(ctx.temps())

    # Add those temps into a new scope as locals.
    self.pushScope()
    for t in temps:
      self.scope.add(t, KIND_TEMP)

    statements = []
    if ctx.statementBlock() is not None:
      statements = self.visit(ctx.statementBlock())

    return PSequence(temps, statements)

  # temps: PIPE (ws? IDENTIFIER)+ ws? PIPE;   -> [string]
  def visitTemps(self, ctx):
    return [ t.getText() for t in ctx.IDENTIFIER() ]

  # statements: statement (PERIOD statements)?   [PStatement]
  def visitStatements(self, ctx):
    stmt = self.visit(ctx.statement())
    if ctx.statements() is None:
      return [stmt]
    stmts = self.visit(ctx.statements())
    stmts.insert(0, stmt)
    return stmts

  # statement: expression
  # A statement is an expression that is followed by a Drop.
  def visitStatement(self, ctx):
    return PStatement(self.visit(ctx.expression()))

  # answer: CARROT ws? expression ws? PERIOD?;   -> PAnswer
  def visitAnswer(self, ctx):
    return PAnswer(self.visit(ctx.expression()), self.blockDepth > 0)

  # StatementAnswer: answer ws?   -> [PAnswer]
  def visitStatementAnswer(self, ctx):
    return [self.visit(ctx.answer())]

  # StatementExpressionsAnswer: statements ws? PERIOD ws? answer
  def visitStatementExpressionsAnswer(self, ctx):
    stmts = self.visit(ctx.statements())
    answer = self.visit(ctx.answer())
    stmts.append(answer)
    return stmts

  # StatementExpressions: statements PERIOD? ws?
  def visitStatementExpressions(self, ctx):
    # Special case: if we're inside a block with no answer, set a flag on the
    # last statement to prevent it from adding the "drop".
    stmts = self.visit(ctx.statements())
    if self.blockDepth > 0:
      stmts[-1].noDrop = True
    return stmts

  # operand: literal | reference | subexpression
  def visitOperand(self, ctx):
    if ctx.literal() is not None:
      return self.visit(ctx.literal())
    elif ctx.reference() is not None:
      return self.visit(ctx.reference())
    elif ctx.subexpression() is not None:
      return self.visit(ctx.subexpression())

  # literal: runtimeLiteral | parsetimeLiteral   -> PThings
  def visitLiteral(self, ctx):
    if ctx.runtimeLiteral() is not None:
      return self.visit(ctx.runtimeLiteral())
    elif ctx.parsetimeLiteral() is not None:
      return self.visit(ctx.parsetimeLiteral())

  # runtimeLiteral: dynamicDictionary | dynamicArray | block -> PDynArray/PDynDict/PBlock
  def visitRuntimeLiteral(self, ctx):
    if ctx.dynamicDictionary() is not None:
      return self.visit(ctx.dynamicDictionary())
    elif ctx.dynamicArray() is not None:
      return self.visit(ctx.dynamicArray())
    elif ctx.block() is not None:
      return self.visit(ctx.block())

  # dynamicDictionary: DYNDICT_START ws? expressions? ws? DYNARR_END
  def visitDynamicDictionary(self, ctx):
    if ctx.expressions() is None:
      return PDynDict([])
    return PDynDict(self.visit(ctx.expressions()))

  # dynamicArray: DYNARRAY_START ws? expressions? ws? DYNARR_END
  def visitDynamicArray(self, ctx):
    if ctx.expressions() is None:
      return PDynArray([])
    return PDynArray(self.visit(ctx.expressions()))

  # expressions : expression (PERIOD expressions)?;
  def visitExpressions(self, ctx):
    expr = self.visit(ctx.expression())
    if ctx.expressions() is None:
      return [expr]
    exprs = self.visit(self.expressions())
    exprs.insert(0, expr)
    return exprs


  # block: BLOCK_START (blockParamList PIPE)? ws? sequence? BLOCK_END
  # blockParamList: (ws? BLOCK_PARAM)+;   -> [string]
  def visitBlock(self, ctx):
    self.blockDepth += 1
    if ctx.blockParamList() is not None:
      params = self.visit(ctx.blockParamList())
    else:
      params = []

    self.pushScope()
    for p in params:
      self.scope.add(p, KIND_ARG)

    seq = self.visit(ctx.sequence())
    self.blockDepth -= 1
    return PBlock(params, seq.temps, seq.code)

  def visitBlockParamList(self, ctx):
    return [ t.getText()[1:] for t in ctx.BLOCK_PARAM() ]

  # pseudoVariable: RESERVED_WORD
  def visitPseudoVariable(self, ctx):
    s = ctx.RESERVED_WORD().getText()
    if s == "self":
      return PSelf()
    elif s == "super":
      return PSuper()
    elif s == "nil":
      return PReference("null", KIND_GLOBAL)
    else:
      return PReference(s, KIND_GLOBAL)

  # parsetimeLiteral: pseudoVariable | number | charConstant | literalArray | string | symbol
  def visitParsetimeLiteral(self, ctx):
    if ctx.pseudoVariable() is not None:
      return self.visit(ctx.pseudoVariable())
    elif ctx.number() is not None:
      return PNumber(self.visit(ctx.number()))
    elif ctx.charConstant() is not None:
      return self.visit(ctx.charConstant())
    # TODO: elif ctx.literalArray
    elif ctx.stringLit() is not None:
      return PString(self.visit(ctx.stringLit()))
    elif ctx.symbol() is not None:
      return self.visit(ctx.symbol())

  # charConstant: CHARACTER_CONSTANT
  def visitCharConstant(self, ctx):
    return PChar(ctx.CHARACTER_CONSTANT().getText()[1:])

  # symbol: HASH bareSymbol
  # bareSymbol: (IDENTIFIER | BINARY_SELECTOR) | KEYWORD+ | stringLit -> string
  def visitBareSymbol(self, ctx):
    if ctx.IDENTIFIER() is not None:
      return PSymbol(ctx.IDENTIFIER().getText())
    elif ctx.BINARY_SELECTOR() is not None:
      return PSymbol(ctx.BINARY_SELECTOR().getText())
    elif ctx.stringLit() is not None:
      return PSymbol(self.visit(ctx.stringLit()))
    else:
      return PSymbol(tokensToString(ctx.KEYWORD()))

  def visitSymbol(self, ctx):
    return self.visit(ctx.bareSymbol())

  # stringLit: STRING
  def visitStringLit(self, ctx):
    return ctx.STRING().getText()[1:-1]

  # number: MINUS? (HEX | EXP | FLOAT | INTEGER)   -> number
  def visitNumber(self, ctx):
    if ctx.HEX() is not None:
      value = int(ctx.HEX().getText()[3:], 16)
    elif ctx.EXP() is not None:
      value = float(ctx.EXP().getText())
    elif ctx.FLOAT() is not None:
      value = float(ctx.FLOAT().getText())
    elif ctx.INTEGER() is not None:
      value = int(ctx.INTEGER().getText())

    if ctx.MINUS() is not None:
      value = -value
    return value

  # reference: variable
  # variable: IDENTIFIER
  def visitReference(self, ctx):
    var = self.visit(ctx.variable())
    kind = self.scope.lookup(var)
    if kind is None:
      self.error(ctx.variable().IDENTIFIER(),
        "Unknown identifier '{:s}'".format(var))
    return PReference(var, kind)
  def visitVariable(self, ctx):
    return ctx.IDENTIFIER().getText()

  # literalArray: LITARR_START literalArrayRest
  # literalArrayRest: ws? ((parsetimeLiteral | bareLiteralArray | bareSymbol) ws?)* CLOSE_PAREN
  # bareLiteralArray: OPEN_PAREN literalArrayRest

  # subexpression: OPEN_PAREN ws? expression ws? CLOSE_PAREN
  def visitSubexpression(self, ctx):
    return self.visit(ctx.expression())

  # expression: assignment | cascade | keywordSend | binarySend | primitive
  def visitExpression(self, ctx):
    if ctx.assignment() is not None:
      return self.visit(ctx.assignment())
    elif ctx.cascade() is not None:
      return self.visit(ctx.cascade())
    elif ctx.keywordSend() is not None:
      return self.visit(ctx.keywordSend())
    elif ctx.binarySend() is not None:
      return self.visit(ctx.binarySend())
    elif ctx.primitive() is not None:
      return self.visit(ctx.primitive())

  # assignment: variable ws? ASSIGNMENT ws? expression
  def visitAssignment(self, ctx):
    name = self.visit(ctx.variable())
    value = self.visit(ctx.expression())

    kind = self.scope.lookup(name)
    tok = ctx.variable().IDENTIFIER()
    if kind is None:
      self.error(tok, "Unknown identifier '{:s}'".format(name))
    elif kind is KIND_GLOBAL:
      self.error(tok, "Cannot assign to global {:s}".format(name))
    elif kind is KIND_ARG:
      self.error(tok, "Cannot assign to input parameter {:s}".format(name))

    return PAssignment(name, kind, value)

  # primitive : LT ws? KEYWORD ws? stringLit ws? GT;
  def visitPrimitive(self, ctx):
    keyword = ctx.KEYWORD().getText()
    name = self.visit(ctx.stringLit())
    return PPrimitive(keyword, name)

  # unarySend: operand ws? unaryTail?      -> PSend
  # unaryTail: unaryMessage ws? unaryTail? -> [string]
  # unaryMessage: ws? unarySelector        -> string
  # unarySelector: IDENTIFIER              -> string
  def visitUnarySelector(self, ctx):
    return ctx.IDENTIFIER().getText() # Just a string.

  def visitUnaryMessage(self, ctx):
    return self.visit(ctx.unarySelector())

  def visitUnaryTail(self, ctx):
    msg = self.visit(ctx.unaryMessage())
    if ctx.unaryTail() is None:
      return [msg]

    tail = self.visit(ctx.unaryTail())
    tail.insert(0, msg)
    return tail

  def visitUnarySend(self, ctx):
    operand = self.visit(ctx.operand())
    if ctx.unaryTail() is None:
      return operand

    tail = self.visit(ctx.unaryTail())
    # The tail is an array of selector names.
    expr = operand
    for sel in tail:
      expr = PSend(expr, sel)
    return expr


  # binarySend: unarySend binaryTail?                            -> PSend?
  # binaryTail: binaryMessage binaryTail?                        -> [(sel, rhs)]
  # binaryMessage: ws? BINARY_SELECTOR ws? (unarySend | operand) -> (sel, rhs)
  def visitBinaryMessage(self, ctx):
    sel = ctx.BINARY_SELECTOR().getText()
    if ctx.unarySend() is not None:
      rhs = self.visit(ctx.unarySend())
    else:
      rhs = self.visit(ctx.operand())
    return (sel, rhs)

  def visitBinaryTail(self, ctx):
    msg = self.visit(ctx.binaryMessage())
    if ctx.binaryTail() is None:
      return [msg]

    tail = self.visit(ctx.binaryTail())
    tail.insert(0, msg)
    return tail


  def visitBinarySend(self, ctx):
    expr = self.visit(ctx.unarySend())
    tail = []
    if ctx.binaryTail() is not None:
      tail = self.visit(ctx.binaryTail())

    for sel, rhs in tail:
      expr = PSend(expr, sel, [rhs])

    return expr

  # keywordSend: binarySend keywordMessage    -> PSend or POperand
  # keywordMessage: ws? (keywordPair ws?)+    -> [(keyword, PSend/POperand)]
  # keywordPair: KEYWORD ws? binarySend ws?   -> (keyword, PSend/POperand)
  def visitKeywordPair(self, ctx):
    keyword = ctx.KEYWORD().getText() # IDENTIFIER + COLON

    rhs = self.visit(ctx.binarySend())
    return (keyword, rhs)

  def visitKeywordMessage(self, ctx):
    return [ self.visit(p) for p in ctx.keywordPair() ]

  def visitKeywordSend(self, ctx):
    base = self.visit(ctx.binarySend())
    pairs = self.visit(ctx.keywordMessage())
    selector = ""
    args = []

    for p in pairs:
      selector = selector + p[0]
      args.append(p[1])

    return PSend(base, selector, args)


  # cascade: (keywordSend | binarySend) (ws? SEMI_COLON ws? message)+
  # message: binaryMessage | unaryMessage | keywordMessage
  #   visitor therefore returns either (sel, rhs), sel, or [(keyword, arg)].
  def messageTails(self, arg):
    # This untangles the mess of the three returns for message.
    # Always returns (sel, [arg]), no matter the format.
    if type(arg) is list:
      sel = ""
      args = []
      for m in arg:
        sel += m[0]
        args.append(m[1])
      return (sel, args)
    elif type(arg) is tuple:
      return (arg[0], [arg[1]])
    else:
      return (arg, [])

  def visitCascade(self, ctx):
    if ctx.keywordSend() is not None:
      receiver = self.visit(ctx.keywordSend())
    else:
      receiver = self.visit(ctx.binarySend())

    tails = [ self.messageTails(self.visit(m)) for m in ctx.message() ]

    # Receiver is either a bare POperand, or a PSend.
    # NB: 3 + 7 + 4; negate is -10.
    # That is, we want to grab the LHS exactly one layer deep.
    if type(receiver) == PSend:
      # Push the guts of that PSend onto the front of tails.
      tails.insert(0, (receiver.selector, receiver.args))
      # Then make our receiver the inner receiver.
      receiver = receiver.receiver

    return PCascade(receiver, tails)


  # assignment: variable ws? assignment ws? expression


