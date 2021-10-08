"Defines the AST for my Smalltalk system."

Object subclass: AST [
  printOn: aStream in: aNamespace [
    self printOn: aStream
  ]
]

AST subclass: ASTValue [
  "Loads of AST nodes have a single arbitrary value."
  | x |
  x [^x]
  x: anObject [x := anObject]
  = other [ ^ self class = other class and: [x = other x] ]
  printOn: aStream [
    aStream << self class name asString << '[' << x << ']'
  ]
]

ASTValue class extend [
  for: anObject [^ self new x: anObject]
]

ASTValue subclass: #STIdentifier.
ASTValue subclass: #STBinarySelector.
ASTValue subclass: #STKeyword.
ASTValue subclass: #STSymbol.
ASTValue subclass: #STString.
ASTValue subclass: #STArray.
ASTValue subclass: #STReturn.
ASTValue subclass: #STDynamicArray.

ASTValue subclass: #STLiteral.

AST subclass: STSend [
  argc [self subclassResponsibility]

  SuperValue := (STLiteral for: (STIdentifier for: 'super')).
  isSuper: receiver [
    ^ receiver = SuperValue
  ]

  sendOp: selectorIndex [
    "Returns the bytecode for this send. Sends are $2css, where c is the arg
    count and ss is the literal number for the selector."
    ^ (16r2000 bitOr: (self argc bitShift: 8)) bitOr: selectorIndex
  ]
]

STSend subclass: STUnarySend [
  | inner selector |
  inner [^ inner]
  selector [^ selector]
  inner: anExpr [inner := anExpr]
  selector: anIdent [selector := anIdent]

  "STSend protocol"
  argc [^ 0]

  "Cascade protocol"
  receiver [^ inner]
  asTail   [^ STUnarySend new selector: selector]

  = other [
    ^ (other isKindOf: STUnarySend) and: [
      inner = other inner and: [
        selector = other selector]]
  ]

  printOn: aStream [
    aStream << self class name asString << '[' << inner << ' ' << selector << ']'
  ]
]

STSend subclass: STBinarySend [
  | left op right |
  left  [^left]
  op    [^op]
  right [^right]
  left:  x [left := x]
  op:    x [op := x]
  right: x [right := x]

  "STSend protocol"
  argc [^ 1]

  "Cascade protocol"
  receiver [^ left]
  asTail   [^STBinarySend new op: op; right: right]

  = other [
    ^ (other isKindOf: STBinarySend)
      and: [left = other left
        and: [op = other op
          and: [right = other right]]]
  ]

  printOn: aStream [
    aStream << self class name asString << '[' << left << ', ' << op << ', ' << right << ']'
  ]
]

STSend subclass: STKeywordSend [
  | receiver kwargs |
  receiver [^receiver]
  kwargs   [^kwargs]
  receiver: x [receiver := x]
  kwargs:   x [kwargs := x]

  addKeywordArg: pair [
    kwargs ifNil: [kwargs := OrderedCollection new].
    kwargs add: pair
  ]

  "STSend protocol"
  argc [^ kwargs size]

  "Cascade protocol"
  asTail [^ STKeywordSend new kwargs: kwargs]

  = other [
    ^ (other isKindOf: STKeywordSend) and: [
        receiver = other receiver and: [
          kwargs = other kwargs]]
  ]

  printOn: aStream [
    aStream << self class name asString << '[' << receiver << '   '.
    kwargs do: [:kw | aStream << kw first << ':  ' << kw last << '   '].
    aStream << ']'
  ]
]


AST subclass: STCascade [
  | receiver tails |
  receiver [^ receiver]
  tails    [^ tails]
  receiver: x [receiver := x]
  tails:    x [tails := x]

  addTail: tail [
    tails ifNil: [tails := OrderedCollection new].
    tails add: tail
  ]

  = other [
    ^ (other isKindOf: STCascade) and: [
        receiver = other receiver and: [
          tails = other tails]]
  ]

  printOn: aStream [
    aStream << self class name asString << '[' << receiver.
    tails do: [:t | aStream nlTab << t].
    aStream << ']'
  ]
]

AST subclass: STAssignments [
  | targets value |
  targets [^targets]
  value   [^value]
  targets: x [targets := x]
  value:   x [value := x]

  = other [
    ^ (other isKindOf: STAssignments) and: [
        targets = other targets and: [
          value = other value]]
  ]

  printOn: aStream [
    aStream << self class name asString << '['.
    targets do: [:each | aStream << each << ' := '].
    aStream << value << ']'
  ]
]

AST subclass: STBlock [
  | vars temps body |
  vars  [^vars]
  body  [^body]
  temps [^temps]
  vars:  x [vars := x]
  body:  x [body := x]
  temps: x [temps := x]

  = other [
    ^ (other isKindOf: STBlock) and: [
        vars = other vars and: [
          temps = other temps and: [
            body = other body]]]
  ]

  printOn: aStream [
    aStream << self class name asString << '['.
    vars do: [:each | aStream space; nextPut: $:; << each].
    vars isEmpty ifFalse: [aStream << '  |  '].

    temps isEmpty ifFalse: [
      aStream << '|'.
      temps do: [:each | aStream space << each].
      aStream << ' |'].

    body do: [:each | aStream nlTab << each].
    aStream << ']'
  ]
]

AST subclass: STMethod [
  | signature temps primitive body |
  signature [^signature]
  temps     [^temps    ]
  body      [^body     ]
  primitive [^primitive]
  signature: x [signature := x]
  primitive: x [primitive := x]
  temps:     x [temps     := x]
  body:      x [body      := x]

  = other [
    ^ (other isKindOf: STMethod) and: [
        signature = other signature and: [
          temps = other temps and: [
            primitive = other primitive and: [
              body = other body]]]]
  ]

  printOn: aStream [
    aStream << self class name asString << '[' << signature.
    Transcript print: primitive; cr.
    primitive isNil ifFalse: [
      aStream nlTab << '<primitive: ' << primitive asString << '>'].
    temps isEmpty ifFalse: [
      aStream nlTab << '| '.
      temps do: [:each | aStream << each; space].
      aStream << '|'].
    body do: [:each | aStream nlTab << each].
    aStream << ']'
  ]
]

AST subclass: STMethodsBlock [
  | cls classy methods |

  initialize [
    classy := false.
    methods := OrderedCollection new
  ]

  cls   [^cls]
  classy  [^classy]
  methods [^methods]
  cls:   x [cls := x]
  classy:  x [classy := x]

  addMethod: method [
    methods add: method
  ]

  = other [
    ^ (other isKindOf: STMethodsBlock) and: [
        cls = other cls and: [
          classy = other classy and: [
            methods = other methods]]]
  ]

  printOn: aStream [
    aStream << '! ' << cls x.
    classy ifTrue: [aStream << ' class'].
    methods do: [:each |
      aStream nl << '! ' << each signature.
      each primitive isNil ifFalse: [
        aStream nlTab << '<primitive: ' << each primitive << '>'].
      each temps isEmpty ifFalse: [
        aStream nlTab << '| ' << each temps << ' |'].

      each body do: [:stmt | aStream nlTab << stmt]].
    aStream nl << '!!'
  ]
]

STMethodsBlock class extend [
  new [
    ^super new initialize
  ]
]
