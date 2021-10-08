MistMethodsBlock extend [
  emitInto: out [
    "Methods blocks are encoded as
    #(1 #ClassName methods...) or 2 for classy ones."
    out add: 5. "Literal array type"
    out add: methods size + 2. "Size word for the outer array."
    (classy ifTrue: [2] ifFalse: [1]) emitInto: out.
    cls name asSymbol emitInto: out.
    methods do: [:each | each emitInto: out]
  ]
]

MistMethod extend [
  emitInto: out [
    self isTopLevel
      ifTrue:  [self emitTopLevel: out]
      ifFalse: [self emitRealMethod: out]
  ]

  emitRealMethod: out [
    "Methods are encoded as
    #( #selectorSymbol args locals #(bytecodes) #( literals ))"
    out add: 5. "Literal array type."
    out add: 5. "Always 5 elements."
    selector emitInto: out.
    argc emitInto: out.
    locals emitInto: out.
    self emitTrailingPart: out
  ]

  emitTopLevel: out [
    "Top-level code is encoded as:
    #( 3 #(bytecodes) #( literals ))"
    out add: 5.
    out add: 3.
    3 emitInto: out.
    self emitTrailingPart: out
  ]

  emitTrailingPart: out [
    (MistWordArray new words: bytecode) emitInto: out.
    literals emitInto: out
  ]
]

SequenceableCollection extend [
  emitInto: out [
    out add: 5. "Array type"
    out add: self size. "Length word"
    self do: [:each | each emitInto: out]
  ]
]

Object subclass: MistWordArray [
  | words |
  words: x [words := x]

  emitInto: out [
    out add: 9. "word array type"
    out add: words size. "length word"
    words do: [:each | out add: each]
  ]
]

SmallInteger extend [
  emitInto: out [
    "32-bit big-endian numbers."
    out add: 1.
    out add: ((self bitShift: -16) bitAnd: 16rFFFF).
    out add: (self bitAnd: 16rFFFF).
  ]
]

Character extend [
  emitInto: out [
    out add: 2.
    out add: self asciiValue
  ]
]

String extend [
  emitTypeFlag [^ 3]

  "This works for Symbols and Strings."
  emitInto: out [
    out add: self emitTypeFlag.
    out add: self size.
    self do: [:ch | out add: ch asciiValue]
  ]
]

Symbol extend [
  emitTypeFlag [^ 4]
]

True extend [
  emitInto: out [
    out add: 6
  ]
]

False extend [
  emitInto: out [
    out add: 7
  ]
]

UndefinedObject extend [
  emitInto: out [
    out add: 8
  ]
]

