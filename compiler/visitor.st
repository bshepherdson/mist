STKeywordSend extend [
  compileInto: method [
    "Some special cases here, to handle eg. subclass: sends at the top level.
    We can detect the top level with method isTopLevel."
    (method isTopLevel and: [kwargs first first x = 'subclass']) ifTrue: [
      self handleSubclass].

    "Whether special or not, a keyword send is compiled as pushing the receiver,
    then each argument, then the send with the literal value."
    receiver compileInto: method.
    kwargs do: [:each | each last compileInto: method].

    method compileSend: self selectorSymbol args: self argc super: self isSuper
  ]

  isSuper [
    ^ receiver = (STIdentifier for: 'super')
  ]

  selectorSymbol [
    ^ (((kwargs collect: [:each | each first x]) join: ':') , ':') asSymbol
  ]

  handleSubclass [
    "This is a special subclass send at the top level, so capture the new class."
    | superclass name instVars classVars cls |
    superclass := receiver x x. "STLiteral[STIdentifier[.]]"
    name := kwargs first last x x x. "STLiteral[STSymbol[STIdentifier[.]]]"
    instVars := kwargs at: 2 ifAbsent: [nil].
    classVars := kwargs at: 3 ifAbsent: [nil].

    (instVars isNil not and: [instVars first x ~= 'instanceVariableNames'])
      ifTrue: [self error: 'expected subclass:instanceVariableNames:'].

    classVars isNil ifFalse: [self error: 'does not actually handle class vars'].

    cls := MistClass named: name superclass: superclass.
    instVars isNil ifFalse: [cls addInstVars: instVars last x x]. "STLiteral[STString[.]]"
  ]

  argumentList [
    ^ kwargs collect: [:pair | pair last x] "last is an STIdentifier, pull the string."
  ]
]

STBinarySend extend [
  compileInto: method [
    "A binary send is compiled as left, then right, then the send op."
    left  compileInto: method.
    right compileInto: method.
    method compileSend: op x asSymbol args: 1 super: self isSuper
  ]

  isSuper [
    ^ left = (STIdentifier for: 'super')
  ]

  argumentList [
    ^ Array with: right x. "STIdentifier -> string"
  ]
]

STUnarySend extend [
  compileInto: method [
    "A unary send is simply compiled as its receiver and then the send."
    inner compileInto: method.
    method compileSend: selector x asSymbol args: 0 super: self isSuper
  ]

  isSuper [
    ^ inner = (STIdentifier for: 'super')
  ]

  argumentList [
    ^ Array new.
  ]
]

STIdentifier extend [
  compileInto: method [
    "Several possibilities here: reserved word, global class we know of,
    global we don't know, inst var, arg or local."
    | var |
    x = 'self'        ifTrue: [method compile: 16r0000. ^nil].
    x = 'super'       ifTrue: [method compile: 16r0000. ^nil].
    x = 'thisContext' ifTrue: [method compile: 16r0001. ^nil].
    x = 'nil'         ifTrue: [method compile: 16r0002. ^nil].
    x = 'true'        ifTrue: [method compile: 16r0003. ^nil].
    x = 'false'       ifTrue: [method compile: 16r0004. ^nil].

    "Then try to look it up in the scope."
    var := method lookup: x.
    var isNil ifFalse: [method compile: var push. ^nil].

    "If we're still here, then it need to be global."
    x first isUppercase ifFalse: [self error: 'unknown identifier: ' , x].

    "Try to look it up as a class."
    var := MistClass at: x.
    var isNil ifFalse: [method compile: (16r0400 bitOr: var index). ^nil].

    "Nothing left but to try a runtime lookup."
    method compile: (16r0300 bitOr: (method addLiteral: x asSymbol)).
    ^ nil
  ]

  asSymbol [
    "There's a few different types of things under STSymbol, so they all respond
    to this message."
    ^ x asSymbol
  ]
]

STBinarySelector extend [
  asSymbol [
    ^ x asSymbol
  ]
]

STReturn extend [
  compileInto: method [
    "A return is compiled as a regular expression, then 'answer TOS', $6200."
    x compileInto: method.
    method compile: 16r6200
  ]
]

STLiteral extend [
  compileInto: method [
    x isNumber ifTrue: [
      "Three cases: fits in the literal range -128 to 127: inlined.
      Fits in 31 bits: push as a literal number.
      Doesn't: die, since we don't support LargePositiveIntegers yet."
      (x between: -128 and: 127) ifTrue: [
        method compile: (16r0600 bitOr: (x bitAnd: 16rFF)).
        ^ nil].

      x = (x bitAnd: 16r7fffffff) ifTrue: [
        method compileLiteral: x.
        ^ nil].

      self error: 'Cannot fit large numbers into Mist currently'].

    x compileInto: method
  ]
]

STBlock extend [
  compileInto: method [
    method blockNest: self
  ]
]

STAssignments extend [
  compileInto: method [
    "Compiles the inner expression, then a series of dup+stores. Runs right to
    left just in case, though since the stores are simultaneous, there's no real
    harm."
    | target |
    value compileInto: method.
    targets reverseDo: [:each |
      method compile: 16r6300. "dup"
      target := method lookup: each x.
      method compile: target store]
  ]
]

STSymbol extend [
  compileInto: method [
    "Add this as a Symbol literal, then compile a literal push."
    method compileLiteral: x asSymbol
  ]
]

STString extend [
  compileInto: method [
    "Add this as a String literal, then compile a literal push $05xx."
    method compileLiteral: x
  ]
]
