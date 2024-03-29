"Kernel classes: the founding classes that make the system run.
Only Object and Metaclass are defined in the core VM; everything else flows from
here. Several of the cornerstone methods, however, are just primitive calls to
Javascript functions."

! Behavior
! basicNew
  <primitive: 0>
! basicNew: aNumber
  <primitive: 25>
!  new
  ^ self basicNew initialize
! methodDictionary
  ^ methodDict

! superclass
  ^ superclass

! allInstVarNames
  | vars |
  superclass == nil
    ifTrue: [vars := self instVarNames copy]
    ifFalse: [vars := superclass allInstVarNames, self instVarNames].
  ^ vars

! instVarNames
  "Answer an Array of the instance variable names. Behaviors must make up fake
  local instance variable names since they don't have instance variable names."
  | mySize superSize |
  mySize := self instSize.
  superSize := superclass == nil
    ifTrue: [0]
    ifFalse: [superclass instSize].
  mySize = superSize ifTrue: [^#()].
  ^ (superSize + 1 to: mySize) collect: [:i | 'inst', i printString]

! methods
  ^ self methodDictionary associations collect: [:asc |
    | m |
    m := Method new.
    m selector: asc key.
    m compiled: asc value.
    m theClass: self.
    m]

! selectors
  "Answers an Array of Symbols for the methods on this class."
  ^ self methodDictionary keys

! subclass: subclassSymbol
  <primitive: 10>
! subclass: subclassSymbol instanceVariableNames: aSpaceSeparatedString
  <primitive: 11>
! subclass: subclassSymbol instanceVariableNames: aSpaceSeparatedString classVarNames: anotherSpaceString
  <primitive: 12>

! inheritsFrom: aClass
  | aSuperclass |
  aSuperclass := self superclass.
  [aSuperclass == nil]
      whileFalse:
          [aSuperclass == aClass ifTrue: [^true].
          aSuperclass := aSuperclass superclass].
  ^false

! instSize
  <primitive: 29>
!!


! ClassDescription
! instVarNames
  ^ instanceVariables == nil ifTrue: [#()] ifFalse: [instanceVariables]
!!

! Class
! allClassVarNames
  ^ superclass
    ifNil: [self classVarNames copy]
    ifNotNil: [super allClassVarNames, self classVarNames]

! classVarNames
  ^ classPool keys
!!


Object subclass: #Method instanceVariableNames: 'selector compiled theClass'.

! Method
! compiled: aCompiledMethod
  compiled := aCompiledMethod
! selector: aSymbol
  selector := aSymbol
! theClass: aClass
  theClass := aClass
! selector
  ^ selector
! numArgs
  ^ compiled numArgs
! theClass
  ^ theClass
!!


"Do nothing in #initialize by default."
! Object
! initialize
  ^ self
! initialize: size
  ^ self
! class
  <primitive: 1>
! yourself
  ^ self
! value
  ^ self "Allows anything to be sent #value, used by eg. test assertions."
! species
  "Provides the best class to use to clone the receiver. Defaults to self class
  but might be something different. Eg. Interval's species is Array."
  ^ self class
! isKindOf: aClass
  ^ self class == aClass or: [ self class inheritsFrom: aClass ]
! halt
  "Triggers the debugger!"
  <primitive: 22>

! == anObject
  <primitive: 30>
! ~= anObject
  ^ (self = anObject) not
! ~~ anObject
  ^ (self == anObject) not

! = anObject
  ^ self == anObject
! hash
  ^ self basicHash
! basicHash
  <primitive: 2>

! instVarAt: anIndex
  <primitive: 3>
! instVarAt: anIndex put: aValue
  <primitive: 4>

! basicAt: anIndex
  <primitive: 26>
! basicAt: anIndex put: aValue
  <primitive: 27>
! at: anIndex
  <primitive: 26>
! at: anIndex put: aValue
  <primitive: 27>

! perform: aSelector
  <primitive: 5>
! perform: aSelector with: anArg
  <primitive: 6>
! perform: aSelector with: anArg with: anotherArg
  <primitive: 7>
! perform: aSelector with: anArg with: anotherArg with: aThirdArg
  <primitive: 8>
! perform: aSelector withArguments: anArray
  <primitive: 9>

! fullPrintString
  ^self printString

! printString
  "Default printString implementation is 'a ClassName'."
  | s |
  s := WriteStream on: (String new: 32).
  self printOn: s.
  ^ s contents

! printOn: aStream
  aStream << 'a ' << self class name

! putOn: aStream
  aStream nextPut: self

! asString
  "Default to the printString representation."
  ^self printString

! isCharacter
  ^ false
! isLiteral
  ^ false
! shouldBePrintedAsLiteral
  ^ self isLiteral

! copy
  "Copies this object. Implemented as a primitive for speed."
  ^self shallowCopy postCopy
! shallowCopy
  "Shallow copy of this object, duplicating its instance variables. SHOULD NOT
  be overridden. Override postCopy if you need custom copying behavior.
  TODO: Rewrite as a primitive for speed."
  | class newObject index |
  class := self class.
  "TODO: Variable-size classes (eg. arrays) need special handling here."
  newObject := class basicNew.
  index := class instSize.
  [index > 0] whileTrue:
      [newObject instVarAt: index put: (self instVarAt: index).
      index := index - 1].
  ^ newObject

! postCopy
  "Does nothing by default."
  ^ self
!!


! Class
! name
  ^name
!!

! Metaclass
! name
  ^ thisClass name, ' class'
! printOn: aStream
  aStream << thisClass name
!!



"Object subclass: #BlockClosurbe
  instanceVariableNames: 'ctx pc0 argc argv handlerActive'."

"Blocks and evaluating blocks."
! Object
! isBlock
  ^ false
!!

! BlockClosure
! isBlock
  ^ true
! value
  <primitive: 13>
! valueNoContextSwitch
  <primitive: 14>
! valueWithArguments: argsArray
  <primitive: 58>
! value: arg1
  <primitive: 15>
! value: arg1 value: arg2
  <primitive: 16>
! value: arg1 value: arg2 value: arg3
  <primitive: 17>
! value: arg1 value: arg2 value: arg3 value: arg4
  <primitive: 18>

! cull: aValue
  "Runs the receiver with 0 or 1 arguments, depending on what it expects.
  Allows flexible code that doesn't need to care if a block takes an argument."
  ^ argc = 0
    ifTrue: [self value]
    ifFalse: [self value: aValue]

! whileTrue: bodyBlock
  "NEVER CALLED: This version is ruthlessly recursive, but it should work.
  Instead, sends of whileTrue and friends should be expanded by
  the parser's macro system into jump bytecodes."
  self value ifTrue: [bodyBlock value. self whileTrue: bodyBlock]
! whileFalse: bodyBlock
  "NEVER CALLED: See BlockClosure>>whileTrue:"
  self value ifFalse: [bodyBlock value. self whileFalse: bodyBlock]

! whileTrue
  "NEVER CALLED: See BlockClosure>>whileTrue:"
  self value ifTrue: [self whileTrue]
! whileFalse
  "NEVER CALLED: See BlockClosure>>whileTrue:"
  self value ifFalse: [self whileFalse]
!!


"Nil and basic checking."
! UndefinedObject
! ifNil: aBlock
  ^ aBlock value
! ifNotNil: aBlock
  ^ self
! ifNil: aBlock ifNotNil: otherBlock
  ^ aBlock value
! isNil
  ^ true
! printOn: aStream
  aStream << 'nil'
! printString
  ^ 'nil'
! = anObject
  ^ anObject isNil
! asString
  ^ 'nil'
! isLiteral
  ^ true
!!

! Object
! ifNil: aBlock
  ^ self
! ifNotNil: aBlock
  ^ aBlock value
! ifNil: aBlock ifNotNil: otherBlock
  ^ otherBlock value
! ifNotNil: aBlock ifNil: otherBlock
  ^ self ifNil: otherBlock ifNotNil: aBlock
! isNil
  ^ false
! isNotNil
  ^ self isNil not
! notNil
  ^ self isNil not
!!


"Booleans and basic control flow."
! Boolean
! ifFalse: falseBlock ifTrue: trueBlock
  ^ self ifTrue: trueBlock ifFalse: falseBlock
! isLiteral
  ^ true
!!

! True class
! new
  ^ true
!!

! True
! ifTrue: trueBlock ifFalse: falseBlock
  ^ trueBlock value
! ifFalse: falseBlock
  ^ nil
! ifTrue: trueBlock
  ^ trueBlock value
! not
  ^ false

! and: aBlock
  "NEVER CALLED: and: and or: should be rewritten by the compiler."
  ^ aBlock value
! or: aBlock
  "NEVER CALLED: and: and or: should be rewritten by the compiler."
  ^ self

! printOn: aStream
  aStream << self printString
! printString
  ^ 'true'
!!

! False class
! new
  ^ false
!!

! False
! ifTrue: trueBlock ifFalse: falseBlock
  ^ falseBlock value
! ifFalse: falseBlock
  ^ falseBlock value
! ifTrue: trueBlock
  ^ nil
! not
  ^ true

! and: aBlock
  "NEVER CALLED: and: and or: should be rewritten by the compiler."
  ^ self
! or: aBlock
  "NEVER CALLED: and: and or: should be rewritten by the compiler."
  ^ aBlock value

! printString
  ^ 'false'
! printOn: aStream
  aStream << self printString
!!


"Transcript"
Object subclass: #Transcript.

! Transcript class
! show: aValue
  self showString: aValue asString.
  ^ aValue

! showString: aString
  <primitive: 20>
!!



"Object subclass: #Number
  instanceVariableNames: 'raw'."

! Magnitude
! <= aNumber
  ^ (aNumber < self) not
! > aNumber
  ^ aNumber < self
! >= aNumber
  ^ (self < aNumber) not
!!


! Number
! asSmallInteger
  self subclassResponsibility

! = aValue
  "Subclasses should be handling this - if they've failed to, we come here."
  ^ self == aValue

! even
  ^ (self bitAnd: 1) == 0
! odd
  ^ (self bitAnd: 1) == 1

! sign
  ^ self < 0 ifTrue: [-1] ifFalse: [1]

! abs
  ^ self < 0 ifTrue: [self negated] ifFalse: [self]

! negated
  ^0 - self
! min: aNumber
  ^ self < aNumber ifTrue: [self] ifFalse: [aNumber]
! max: aNumber
  ^ self < aNumber ifTrue: [aNumber] ifFalse: [self]
! between: lo and: hi
  ^ self >= lo and: [self <= hi]

"Superclass numbers - if we've gotten here then convert everything to fractions,
they're the most flexible."
! + aNumber
  ^ self asFraction + aNumber asFraction
! - aNumber
  ^ self asFraction - aNumber asFraction
! * aNumber
  ^ self asFraction * aNumber asFraction
! / aNumber
  ^ self asFraction / aNumber asFraction
! < aNumber
  ^ self asFraction < aNumber asFraction
!!

! Integer
! isLiteral
  ^ true

"! / aNumber
  ^ (aNumber isKindOf: Fraction)
    ifTrue: [Fraction numerator: self * aNumber denominator
        denominator: aNumber numerator]
    ifFalse: [Fraction numerator: self denominator: aNumber]
"
!!

! SmallInteger
! + aNumber
  <primitive: 31>
  ^ super + aNumber
! - aNumber
  <primitive: 32>
  ^ super - aNumber
! * aNumber
  <primitive: 33>
  ^ super * aNumber
! // aNumber
  <primitive: 34>
  ^ super // aNumber
! \\ aNumber
  <primitive: 35>
  ^ super \\ aNumber
! < aNumber
  <primitive: 36>
  ^ super < aNumber
! <= aNumber
  ^ (aNumber < self) not
! > aNumber
  ^ aNumber < self
! >= aNumber
  ^ (self < aNumber) not
! = aNumber
  <primitive: 37>
  ^ super = aNumber
! ~= aNumber
  ^ (self = aNumber) not
! bitOr: aNumber
  <primitive: 38>
  ^ super bitOr: aNumber
! bitAnd: aNumber
  <primitive: 39>
  ^ super bitAnd: aNumber
! bitXor: aNumber
  <primitive: 40>
  ^ super bitXor: aNumber
! bitShift: aDelta
  <primitive: 43>
  ^ super bitShift: aDelta
! bitInvert
  <primitive: 41>
  ^ super bitInvert

! asSmallInteger
  ^ self

! nextPowerOf2
  | c |
  c := 2.
  [self <= c] whileFalse: [c := c bitShift: 1].
  ^ c

! hash
  ^ self

! to: end do: aBlock
  | index |
  index := self.
  [index <= end] whileTrue:
      [aBlock value: index.
      index := index + 1]
! to: end by: delta do: aBlock
  | index |
  index := self.
  delta < 0
    ifTrue: [
      [index >= end] whileTrue:
          [aBlock value: index.
          index := index + delta]]
    ifFalse: [
      [index <= end] whileTrue:
          [aBlock value: index.
          index := index + delta]]

! timesRepeat: aBlock
  | index |
  index := self.
  [index > 0] whileTrue:
      [aBlock cull: index.
      index := index - 1]
! printString
  <primitive: 42>
! printOn: aStream
  self putOn: aStream
! putOn: aStream
  aStream nextPutAll: self printString
!!


! CompiledMethod
! numArgs
  ^ argc
! selector
  ^ selector
! owningClass
  ^ class
! bytecode
  ^ bytecode
!!

! MethodContext class
! new
  "This is a special case: create a mixed IV + variable."
  ^ self new: 19
! new: aSize
  ^ (self basicNew: 19) initialize; yourself
!!

! Character class
! value: anInteger
  ^ AsciiTable at: anInteger + 1
!!

! Character
! asciiValue
  ^ asciiValue
! isLiteral
  ^ true
! isCharacter
  ^ true
! < aCharacter
  aCharacter class == Character ifTrue: [^ asciiValue < aCharacter asciiValue].
  ^ aCharacter >= self
! shouldBePrintedAsLiteral
  ^ asciiValue between: 33 and: 255
! printOn: aStream
  aStream nextPut: $$; nextPut: self
!!

