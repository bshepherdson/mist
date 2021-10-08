"The main driver for compiling Smalltalk ASTs (see parser/) into the bytecode used
by the Mist virtual machine.
This compiles into MistMethod objects holding the bytecodes, literals, argc,
locals count, etc."

Object subclass: MistMethod [
  | selector bytecode literals argc locals scope blockDepth |
  initialize [
    bytecode := OrderedCollection new.
    literals := OrderedCollection new.
    blockDepth := 0.
    argc := 0.
    locals := 0
  ]

  selector     [^selector]
  selector:  x [selector := x]
  scope:  x [scope := x]
  argc      [^argc]
  locals    [^locals]
  argc:   n [argc := n]
  locals: n [locals := n]

  lookup: varName [
    "Returns nil if it can't be found on the scope, and a Var otherwise."
    scope ifNil: [^nil].
    ^ scope lookup: varName
  ]

  insideBlock [
    ^ blockDepth > 0
  ]

  isTopLevel [
    ^ scope isNil
  ]

  blockNest: aBlock [
    "There's a lot of bookkeeping here, so it's handled as part of the method
    rather than remotely from STBlock>>compileInto:.

    StartBlock bytecode is $5caa, where c is the arg count and aa is the argv
    start index into the containing method's locals. The next word gives the
    block's body length in bytecodes. We store a 0 there and come back to it."
    | localStart blockStart first |
    localStart := argc + locals + 1. "0-based index for argv, + 1 for self."
    locals := locals + aBlock vars size + aBlock temps size.
    self compile: ((16r5000 bitOr: (aBlock vars size bitShift: 8)) bitOr: localStart).

    "Nest into the block: add its locals and args to the Scope."
    blockDepth := blockDepth + 1.
    scope := Scope new parent: scope.
    1 to: aBlock vars size do: [:ix |
      scope add: (aBlock vars at: ix) x "Unwrap STIdentifier"
        var: (MethodArg new index: localStart + ix - 1)].
    1 to: aBlock temps size do: [:ix |
      scope add: (aBlock temps at: ix) x "Unwrap STIdentifier"
        var: (LocalVar new index: localStart + aBlock vars size + ix - 1)].

    self compile: 0. "Dummy size word."
    blockStart := bytecode size. "Points at the size word, before the block."

    "Now compile the body of the block, with its drops and things."
    first := true.
    aBlock body do: [:each |
      first ifFalse: [self compileDrop].
      first := false.
      each compileInto: self].

    "If the last bytecode was a return, do nothing. If it wasn't, return TOS.
    Unless the block is empty, in which case push nil, then return TOS."
    (aBlock body isEmpty or: [(aBlock body last isKindOf: STReturn) not]) ifTrue: [
      aBlock body isEmpty ifTrue: [self compile: 16r0002]. "Push nil"
      self compile: 16r6200].

    "Now bytecode size is the index of the last bytecode, and
    bytecode size - blockstart is the length of the block. Store that at
    blockStart as the length."
    bytecode at: blockStart put: bytecode size - blockStart.
    blockDepth := blockDepth - 1.
    scope := scope parent.
  ]

  compile: aBytecode [
    aBytecode isNumber ifFalse: [self error: 'non-numeric bytecode: ', aBytecode printString].
    bytecode add: aBytecode
  ]

  compileLiteral: aLiteral [
    self compile: (16r0500 bitOr: (self addLiteral: aLiteral))
  ]

  addLiteral: aLiteralValue [
    "Check if it's already there, and reuse it if so."
    literals keysAndValuesDo: [:ix :each |
      aLiteralValue = each ifTrue: [^ix - 1]].
    literals add: aLiteralValue.
    ^ literals size - 1 "0-based indexing for literals."
  ]

  compileSend: selectorSymbol args: argc super: isSuper [
    "Sends are $2css, with c the arg count and ss the literal number."
    | bc |
    bc := isSuper ifTrue: [16r3000] ifFalse: [16r2000].
    bc := bc bitOr: (argc bitShift: 8).
    bc := bc bitOr: (self addLiteral: selectorSymbol).
    self compile: bc
  ]

  compileDrop [
    self compile: 16r6100
  ]
  compileDup [
    self compile: 16r6000
  ]

  printOn: aStream [
    aStream << 'MistMethod[' << argc << ' args, ' << locals << ' locals'.
    aStream nlTab << 'bytecode'.
    bytecode do: [:each |
      aStream space.
      each printOn: aStream paddedWith: $0 to: 4 base: 16].
    aStream nlTab << 'literals '.
    literals do: [:each | aStream nlTab tab << each]
  ]
]

MistMethod class extend [
  new [^ super new initialize]
]

Object subclass: MistClass [
  | name index superclass instVars classVars instVarBase classVarBase |
  Classes := Dictionary new.
  BootstrappedCount := 30.
  NextIndex := BootstrappedCount + 1.

  name      [^name]
  index     [^index]
  instVarBase [^instVarBase]
  classVarBase [^classVarBase]
  name:         x [name := x]
  index:        x [index := x]
  instVarBase:  x [instVarBase := x]
  classVarBase: x [classVarBase := x]
  superclass:  aString [
    superclass := aString isNil ifFalse: [Classes at: aString]
  ]

  initialize [
    instVars := Dictionary new.
    classVars := Dictionary new.
  ]

  register [
    Classes at: name put: self.
    instVarBase := 0.
    classVarBase := 0.
    superclass isNil ifFalse: [
      instVarBase := superclass instVarBase.
      classVarBase := superclass classVarBase]
  ]

  isBootstrapped [
    ^ self index <= BootstrappedCount
  ]

  scope [
    | parent |
    parent := superclass isNil ifFalse: [superclass scope].
    ^ Scope new vars: instVars; parent: parent
  ]

  classyScope [
    | parent |
    parent := superclass isNil ifFalse: [superclass classyScope].
    ^ Scope new vars: classVars; parent: parent
  ]

  addInstVar: aString [
    instVars at: aString put: (InstVar new index: instVarBase).
    instVarBase := instVarBase + 1
  ]

  addInstVars: aSpaceSeparatedString [
    aSpaceSeparatedString subStrings do: [:each | self addInstVar: each]
  ]
]

MistClass class extend [
  at: index named: name superclass: superclassName [
    | m |
    m := self new initialize.
    m index: index.
    m name: name.
    m superclass: superclassName.
    m register.
    ^ m
  ]

  at: className [
    ^ (self bindingFor: #Classes) value at: className ifAbsent: [nil]
  ]

  named: name superclass: superclassName [
    | m ix |
    m := self new initialize.

    ix := self bindingFor: #NextIndex.
    m index: ix value.
    ix value: ix value + 1.

    m name: name.
    m superclass: superclassName.
    m register.
    ^ m
  ]
]


Object subclass: Var [
  | index |
  index: x [index := x]
  push  [self subclassResponsibility]
  store [self subclassResponsibility]
  printOn: aStream [
    aStream nextPutAll: self class name.
    aStream << '[' << index << ']'.
  ]
]

Var subclass: InstVar [
  push [
    "Push inst var is $02xx"
    ^ 16r0200 bitOr: index
  ]
  store [
    "Store inst var is $11xx"
    ^ 16r1100 bitOr: index
  ]
]

Var subclass: LocalVar [
  push [
    "Push local is $01xx"
    ^ 16r0100 bitOr: index
  ]

  store [
    "Store local is $10xx"
    ^ 16r1000 bitOr: index
  ]
]

LocalVar subclass: MethodArg [
  store [
    self error: 'cannot store to method args'
  ]
]


Object subclass: Scope [
  | vars parent |
  parent  [^parent]
  vars    [
    vars ifNil: [vars := Dictionary new].
    ^vars
  ]

  vars:   x [vars := x]
  parent: x [parent := x]

  lookup: aName [
    ^ self vars at: aName ifAbsent: [
      parent isNil ifFalse: [parent lookup: aName]]
  ]

  add: name var: aVar [
    (self vars includesKey: name) ifTrue: [
      self error: 'duplicate local name: ', name].
    self vars at: name put: aVar
  ]
]

MistClass at: 0 named: 'ProtoObject' superclass: nil.
MistClass at: 1 named: 'Object' superclass: 'ProtoObject'.
(MistClass at: 2 named: 'Behavior' superclass: 'Object')
  addInstVar: 'superclass'; addInstVar: 'methodDict'; addInstVar: 'format'.
MistClass at: 3 named: 'ClassDescription' superclass: 'Behavior'.
(MistClass at: 4 named: 'Class' superclass: 'ClassDescription')
  addInstVar: 'name'; addInstVar: 'subclasses'.
(MistClass at: 5 named: 'Metaclass' superclass: 'ClassDescription')
  addInstVar: 'thisClass'.

MistClass at:  6 named: 'UndefinedObject' superclass: 'Object'.
MistClass at:  7 named: 'Collection' superclass: 'Object'.
MistClass at:  8 named: 'SequenceableCollection' superclass: 'Collection'.
MistClass at:  9 named: 'ArrayedCollection' superclass: 'SequenceableCollection'.
MistClass at: 10 named: 'Array' superclass: 'ArrayedCollection'.
MistClass at: 11 named: 'String' superclass: 'ArrayedCollection'.
MistClass at: 12 named: 'Symbol' superclass: 'String'.
(MistClass at: 13 named: 'HashedCollection' superclass: 'Collection')
  addInstVar: 'array'; addInstVar: 'tally'.
MistClass at: 14 named: 'Dictionary' superclass: 'HashedCollection'.
MistClass at: 15 named: 'IdentityDictionary' superclass: 'Dictionary'.

(MistClass at: 16 named: 'MethodContext' superclass: 'Object')
  addInstVar: 'method'; addInstVar: 'locals'; addInstVar: 'pc';
  addInstVar: 'sender'; addInstVar: 'sp'.

(MistClass at: 17 named: 'CompiledMethod' superclass: 'Object')
  addInstVar: 'bytecode'; addInstVar: 'literals'; addInstVar: 'selector';
  addInstVar: 'class'; addInstVar: 'argc'; addInstVar: 'locals'.
(MistClass at: 18 named: 'BlockClosure' superclass: 'Object')
  addInstVar: 'ctx'; addInstVar: 'pc0'; addInstVar: 'argc';
  addInstVar: 'argv'; addInstVar: 'handlerActive'.


MistClass at: 19 named: 'Boolean' superclass: 'Object'.
MistClass at: 20 named: 'True' superclass: 'Boolean'.
MistClass at: 21 named: 'False' superclass: 'Boolean'.

MistClass at: 22 named: 'Magnitude' superclass: 'Object'.
MistClass at: 23 named: 'Character' superclass: 'Magnitude'.
MistClass at: 24 named: 'Number' superclass: 'Magnitude'.
MistClass at: 25 named: 'Integer' superclass: 'Number'.
MistClass at: 26 named: 'SmallInteger' superclass: 'Integer'.

(MistClass at: 27 named: 'Association' superclass: 'Object')
  addInstVar: 'key'; addInstVar: 'value'.

MistClass at: 28 named: 'WordArray' superclass: 'ArrayedCollection'.

(MistClass at: 29 named: 'Process' superclass: 'Object')
  addInstVar: 'context'; addInstVar: 'next'; addInstVar: 'prev';
  addInstVar: 'processTable'.

(MistClass at: 30 named: 'ProcessTable' superclass: 'Object')
  addInstVar: 'ready'; addInstVar: 'blocked'; addInstVar: 'nextPriority'.

"If I add more built-in classes, I need to update the NextIndex starting value."


Object subclass: MistMethodsBlock [
  | cls classy methods scope |

  for: className classy: isClassy [
    cls := MistClass at: className x.
    classy := isClassy.
    scope := isClassy ifTrue: [cls classyScope] ifFalse: [cls scope].
    methods := OrderedCollection new.
  ]

  compileMethod: method [
    "Expects a STMethod describing this method's code. Compiles it into a
    MistMethod, and includes it on this class."
    | compiled methodScope args first |
    compiled := MistMethod new.

    "Extract the args as a list of strings from the signature."
    methodScope := Scope new parent: scope.
    args := method signature argumentList.
    1 to: args size do: [:ix |
      methodScope add: (args at: ix) var: (MethodArg new index: ix)].

    "Likewise with the top-level temporaries."
    1 to: method temps size do: [:ix |
      methodScope add: (method temps at: ix) x
        var: (LocalVar new index: ix + args size)].

    compiled selector: method signature selectorSymbol.
    compiled scope: methodScope.
    compiled argc: args size.
    compiled locals: method temps size. "Blocks might add to this."

    method primitive isNil ifFalse: [
      compiled compile: (16r7000 bitOr: method primitive)].

    first := true.
    method body do: [:each |
      first ifFalse: [compiled compileDrop].
      first := false.
      each compileInto: compiled].

    "If the method body didn't contain an answer, return self."
    "But if the entire body is a primitive, also skip it."
    ((method body isEmpty and: [method primitive isNil]) or: [
      method body isEmpty not and: [(method body last isKindOf: STReturn) not]])
    ifTrue: [compiled compile: 16r6300].

    methods add: compiled
  ]
]

