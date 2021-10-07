
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
    Transcript print: someCode; cr.
    fake := MistMethod new.
    someCode compileInto: fake.
    ^ fake
  ]
]

MistCompiler new compileChunks: (Parser new parseFile: 'st/Kernel.st').

