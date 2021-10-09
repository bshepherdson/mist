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
    (self appliesTo: anSTSend) ifFalse: [^anSTSend].
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
    Transcript print: ast; cr.
    ^ ast
  ]
]

STSend extend [
  checkMacros [
    ^ self macros inject: self into: [:ast :macro | macro applyTo: ast]
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
  Macros := {MMIfTrue new}.

  macros [
    ^ Macros
  ]
]

