STKeywordSend extend [
  compileInto: method [
    "Some special cases here, to handle eg. subclass: sends at the top level.
    We can detect the top level with method isTopLevel."
    (method isTopLevel and: [kwargs first first x = 'subclass']) ifTrue: [
      self handleSubclass].

    "Whether special or not, a keyword send is compiled as pushing the receiver,
    then each argument, then the send with the literal value."
    receiver isNil ifFalse: [receiver compileInto: method].
    kwargs do: [:each | each last compileInto: method].

    method compileSend: self selectorSymbol
      args: self argc
      super: (self isSuper: receiver)
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
    left isNil ifFalse: [left  compileInto: method].
    right compileInto: method.
    method compileSend: self selectorSymbol args: 1 super: (self isSuper: left)
  ]

  selectorSymbol [
    ^ op x asSymbol
  ]

  argumentList [
    ^ Array with: right x. "STIdentifier -> string"
  ]
]

STUnarySend extend [
  compileInto: method [
    "A unary send is simply compiled as its receiver and then the send."
    inner isNil ifFalse: [inner compileInto: method].
    method compileSend: self selectorSymbol args: 0 super: (self isSuper: inner)
  ]

  selectorSymbol [
    ^ selector x asSymbol
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
    method compile: (method insideBlock ifTrue: [16r6400] ifFalse: [16r6200])
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

      (x between: -2147483648 and: 2147483647) ifTrue: [
        method compileLiteral: x.
        ^ nil].

      self error: 'Cannot fit large numbers into Mist currently: ', x printString].

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
      method compile: 16r6000. "dup"
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

STKeyword extend [
  asSymbol [
    "x is a collection of strings."
    ^ ((x join: ':'), ':') asSymbol
  ]
]

STDynamicArray extend [
  compileInto: method [
    "Creates an array, creates a writeStream for it, then compiles nextPut:s
    into it. This generates a STCascade and calls into it, since that's exactly
    the flow we need, DUPs and so on."
    | cascade |
    cascade := STCascade new.
    cascade receiver: (STUnarySend new
      inner: (STKeywordSend new
        receiver: (STLiteral for: (STIdentifier for: 'Array'));
        addKeywordArg: {STIdentifier for: 'new'. STLiteral for: x size};
        yourself);
      selector: (STIdentifier for: 'writeStream');
      yourself).


    "Then for each element, we add a nextPut: theElement ot the cascade."
    x do: [:each |
      cascade addTail: (STKeywordSend new
        addKeywordArg: {STIdentifier for: 'nextPut'.  each};
        yourself)].

    "End with #contents to get the array on the stack rather than the stream."
    cascade addTail: (STUnarySend new selector: (STIdentifier for: 'contents')).
    cascade compileInto: method
  ]
]

STCascade extend [
  compileInto: method [
    "Cascades begin with a receiver which gets pushed onto the stack, followed
    by 0 or more message send tails. Tails have their receiver slot as nil; they
    know how to handle that.
    After each tail (but not the first one) the result is DROP'd.
    Before each tail except the last, the receiver is DUP'd."
    receiver compileInto: method.
    tails keysAndValuesDo: [:ix :each |
      ix > 1 ifTrue: [method compileDrop].
      ix < tails size ifTrue: [method compileDup].
      each compileInto: method].
  ]
]
