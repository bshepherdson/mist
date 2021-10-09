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

MistMacro subclass: MMIfTrue [
  "expr ifTrue: literal-block"
  appliesTo: send [
    ^ send selectorSymbol = #ifTrue: and: [
      "Transcript print: send kwargs first last x; cr."
      send kwargs first last x isBlock] "STLiteral[STBlock[...]]"
  ]

  basicApplyTo: send [
    "We want to skip if false over the block's length."
    | block ast |
    block := send kwargs first last x. "STLiteral[STBlock[...]]"
    ast := STScoped new temps: block temps.
    ast body: (OrderedCollection new
      add: (STJump skipFalsePushNil: send receiver);
      addAll: block body;
      add: STJumpResolver new;
      yourself).
    ^ ast
  ]
]

MistMacro subclass: MMIfFalse [
  "expr ifFalse: literal-block"
  appliesTo: send [
    ^ send selectorSymbol = #ifFalse: and: [
      "Transcript print: send kwargs first last x; cr."
      send kwargs first last x isBlock] "STLiteral[STBlock[...]]"
  ]

  basicApplyTo: send [
    "We want to skip if true over the block's length."
    | block ast |
    block := send kwargs first last x. "STLiteral[STBlock[...]]"
    ast := STScoped new temps: block temps.
    ast body: (OrderedCollection new
      add: (STJump skipTruePushNil: send receiver);
      addAll: block body;
      add: STJumpResolver new;
      yourself).
    Transcript print: ast; cr.
    ^ ast
  ]
]

MistMacro subclass: MMIfTrueIfFalse [
  "expr ifTrue: literal-block ifFalse: literal-block"
  appliesTo: send [
    ^ send selectorSymbol = #ifTrue:ifFalse: and: [
      "Transcript print: send kwargs first last x; cr."
      send kwargs first last x isBlock and: [
        send kwargs second last x isBlock]] "STLiteral[STBlock[...]]"
  ]

  basicApplyTo: send [
    "First, skip if false to after the true part. At the end of the true part,
    unconditionally skip to the end. Swap the jump stack order, then resolve the
    original jump, then the last one."
    | trueBlock trueAst falseBlock falseAst |
    trueBlock := send kwargs first last x. "STLiteral[STBlock[...]]"
    falseBlock := send kwargs second last x. "STLiteral[STBlock[...]]"
    trueAst := STScoped new temps: trueBlock temps.
    trueAst body: (OrderedCollection new
      add: (STJump skipFalsePushNil: send receiver);
      addAll: trueBlock body;
      add: STJump skipForward; "This is now on top of the jump stack."
      add: STJumpSwap new;     "Swap it with the original jump."
      add: STJumpResolver new; "And resolve that to here."
      yourself).

    falseAst := STScoped new temps: falseBlock temps.
    falseAst body: (OrderedCollection new
      addAll: falseBlock body;
      add: STJumpResolver new;
      yourself).
    ^ {trueAst. falseAst}
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
    MMIfTrueIfFalse new}.

  macros [
    ^ Macros
  ]
]

