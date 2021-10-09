"Compiler macros are rewriting rules applied to message sends.
Some example uses:
- Sending ifTrue: to a literal block can be inlined as conditional jump.
- whileTrue: and whileTrue etc. on literal blocks can be inlined as well.

Macros are simply called with applyTo: anSTSend, and they should return a
possibly-modified AST.

Some gotchas:
- Blocks can have locals; these need to still be added to the method as locals,
  and pushed into the Scope while compiling the contents. STScoped is used to
  handle that."

Object subclass: MistMacro [
  applyTo: anSTSend [
    (self appliesTo: anSTSend) ifFalse: [^nil].
    ^ self basicApplyTo: anSTSend
  ]

  appliesTo: anSTSend [
    self subclassResponsibility
  ]
  basicApplyTo: anSTSend [
    self subclassResponsibility
  ]

  "Helpers to compute the opcode numbers."
  skipTruePushNil     [^ STJump for: 16r6800]
  skipFalsePushNil    [^ STJump for: 16r6900]
  skipForward         [^ STJump for: 16r6A00]
  skipBackward        [^ STJump for: 16r6B00]
  skipTruePushTrue    [^ STJump for: 16r6C00]
  skipFalsePushFalse  [^ STJump for: 16r6D00]
]

MistMacro subclass: MMIf [
  "expr ifTrue: literal-block"
  appliesTo: send [
    ^ send selectorSymbol = self expectedSelector and: [
      "Transcript print: send kwargs first last x; cr."
      send kwargs first last x isBlock] "STLiteral[STBlock[...]]"
  ]

  basicApplyTo: send [
    "We want to skip if false over the block's length."
    | block ast |
    block := send kwargs first last x. "STLiteral[STBlock[...]]"
    ast := STScoped new temps: block temps.
    ast body: (OrderedCollection new
      add: send receiver;
      add: self theJump;
      addAll: block body;
      add: STJumpResolver new;
      yourself).
    ^ ast
  ]
]

MMIf subclass: MMIfTrue [
  expectedSelector [
    ^ #ifTrue:
  ]
  theJump [
    ^ STJump skipFalsePushNil
  ]
]

MMIf subclass: MMIfFalse [
  expectedSelector [
    ^ #ifFalse:
  ]
  theJump [
    ^ STJump skipTruePushNil
  ]
]

MistMacro subclass: MistMacroTwoBlocks [
  expectedSelector [
    self subclassResponsibility
  ]

  appliesTo: send [
    ^ send selectorSymbol = self expectedSelector and: [
      send kwargs first last x isBlock and: [
        send kwargs second last x isBlock]] "STLiteral[STBlock[...]]"
  ]
]

MistMacroTwoBlocks subclass: MMIfTrueIfFalse [
  "expr ifTrue: literal-block ifFalse: literal-block"
  expectedSelector [
    ^ #ifTrue:ifFalse:
  ]

  basicApplyTo: send [
    "First, skip if false to after the true part. At the end of the true part,
    unconditionally skip to the end. Swap the jump stack order, then resolve the
    original jump, then the last one.
    Remember the a successful skip pushes nil! So the false block begins with
    dropping that nil pushed by the conditional skip."
    | trueBlock trueAst falseBlock falseAst |
    trueBlock := send kwargs first last x. "STLiteral[STBlock[...]]"
    falseBlock := send kwargs second last x. "STLiteral[STBlock[...]]"
    trueAst := STScoped new temps: trueBlock temps.
    trueAst body: (OrderedCollection new
      add: STJump skipFalsePushNil;
      addAll: trueBlock body;
      add: STJump skipForward; "This is now on top of the jump stack."
      add: STJumpSwap new;     "Swap it with the original jump."
      add: STJumpResolver new; "And resolve that to here."
      yourself).

    falseAst := STScoped new temps: falseBlock temps.
    falseAst body: (OrderedCollection new
      add: STDrop new;
      addAll: falseBlock body;
      add: STJumpResolver new;
      yourself).
    ^ {send receiver. trueAst. falseAst}
  ]
]

MistMacroTwoBlocks subclass: MMIfFalseIfTrue [
  "expr ifFalse: literal-block ifTrue: literal-block"
  expectedSelector [
    ^ #ifFalse:ifTrue:
  ]

  basicApplyTo: send [
    "Just swapping the order of the blocks."
    ^ STKeywordSend new
      receiver: send receiver;
      addKeywordArg: send kwargs second;
      addKeywordArg: send kwargs first;
      yourself
  ]
]

MistMacro subclass: MMWhileBinary [
  "conditionBlock whileTrue: bodyBlock"
  appliesTo: send [
    ^ send selectorSymbol = self expectedSelector and: [
      send receiver x isBlock and: [
        send kwargs first last x isBlock]] "STLiteral[STBlock[...]]"
  ]

  expectedSelector [
    self subclassResponsibility
  ]
  theJump [
    self subclassResponsibility
  ]

  basicApplyTo: send [
    "backref condBlock skip-if-false bodyBlock swap unconditional-back end"
    | condBlock condAst bodyBlock bodyAst |
    condBlock := send receiver x. "STLiteral[STBlock[...]]"
    bodyBlock := send kwargs first last x. "STLiteral[STBlock[...]]"
    condAst := STScoped new temps: condBlock temps.
    condAst body: (OrderedCollection new
      add: STJumpBackRef new;
      addAll: condBlock body;
      add: STJump skipFalsePushNil; "This is now on top of the jump stack."
      yourself).

    bodyAst := STScoped new temps: bodyBlock temps.
    bodyAst body: (OrderedCollection new
      addAll: bodyBlock body;
      add: STDrop new; "Drop the previous expression result from the stack."
      add: STJumpSwap new;
      add: STJumpBack new;
      add: STJumpResolver new;
      "The trailing nil is the result of the loop expression."
      yourself).
    ^ {condAst. bodyAst}
  ]
]

MMWhileBinary subclass: MMWhileTrueBinary [
  expectedSelector [
    ^ #whileTrue:
  ]
  theJump [
    ^ STJump skipFalsePushNil
  ]
]

MMWhileBinary subclass: MMWhileFalseBinary [
  expectedSelector [
    ^ #whileFalse:
  ]
  theJump [
    ^ STJump skipTruePushNil
  ]
]

STSend extend [
  checkMacros [
    self macros do: [:macro |
      (macro applyTo: self) ifNotNil: [:updated | ^updated]].
    ^ self
  ]

  macros [
    self subclassResponsility
  ]
]

STUnarySend extend [
  Macros := {}.

  macros [
    ^ Macros
  ]
]

STBinarySend extend [
  Macros := {}.

  macros [
    ^ Macros
  ]
]

STKeywordSend extend [
  Macros := {
    MMIfTrue new.
    MMIfFalse new.
    MMIfTrueIfFalse new.
    MMIfFalseIfTrue new.
    MMWhileTrueBinary new.
    MMWhileFalseBinary new.
    }.

  macros [
    ^ Macros
  ]
]

