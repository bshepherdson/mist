
Object subclass: MistCompiler [
  | results |
  results [^results]

  compileChunks: chunks [
    results ifNil: [results := OrderedCollection new].
    chunks do: [:each |
      (each isKindOf: STMethodsBlock)
        ifTrue:  [results add: (self compileMethods: each)]
        ifFalse: [results add: (self compileCode: each)]].
  ]

  compileMethods: methodsBlock [
    | mb |
    mb := MistMethodsBlock new.
    mb for: methodsBlock cls classy: methodsBlock classy.
    methodsBlock methods do: [:each | mb compileMethod: each].
    ^ mb
  ]

  compileCode: someCode [
    | fake |
    "Transcript print: someCode; cr."
    fake := MistMethod new.
    someCode compileInto: fake.
    fake compile: 16r6200. "Add a dummy return TOS."
    ^ fake
  ]

  writeDriverFile: filename [
    "Compiles the results list into 'driver' format; see DESIGN.md."
    | out fs |
    out := OrderedCollection new.
    results do: [:each | each emitInto: out].
    fs := FileStream open: filename mode: FileStream create.
    out do: [:each | fs nextPutShort: each].
    fs close
  ]
]

[ | mc inputs |
  mc := MistCompiler new.
  "First arg is the output file."
  inputs := Smalltalk arguments copyFrom: 2.
  inputs do: [:each |
    mc compileChunks: (Parser new parseFile: each)].
  mc writeDriverFile: Smalltalk arguments first
] value.

