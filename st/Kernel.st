"Kernel classes: the founding classes that make the system run.
Only Object and Metaclass are defined in the core VM; everything else flows from
here. Several of the cornerstone methods, however, are just primitive calls to
Javascript functions."

! ClassDescription
! basicNew
  <builtin: 'basicNew'>
!  new
  ^ self basicNew initialize
  "| inst |
  inst := self basicNew.
  inst initialize.
  ^ inst"

! methodDictionary
  ^ methodDict
!!

"Do nothing in #initialize by default."
! Object
! initialize
  ^ self
! class
  <builtin: 'class'>
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
  <builtin: 'halt'>

! instVarAt: anIndex
  <builtin: 'instVarAt:'>
! instVarAt: anIndex put: aValue
  <builtin: 'instVarAt:put:'>

! copy
  "Copies this object. Implemented as a primitive for speed."
  ^self shallowCopy postCopy
! shallowCopy
  "Shallow copy of this object, duplicating its instance variables. SHOULD NOT
  be overridden. Override postCopy if you need custom copying behavior."
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


"Subclass and its variations."
! Class
! subclass: aSymbol
  <builtin: 'subclass:'>
! subclass: aSymbol instanceVariableNames: aString
  <builtin: 'subclass:instanceVariableNames:'>
! subclass: aSymbol instanceVariableNames: aString classVariableNames: anotherString
  <builtin: 'subclass:instanceVariableNames:classVariableNames:'>

! name
  ^name

! superclass
  ^ superclass

! methods
  ^ self methodDictionary values

! selectors
  "Answers an Array of Symbols for the methods on this class."
  ^ self methodDictionary keys
!!


! Behavior
! inheritsFrom: aClass
  | aSuperclass |
  aSuperclass := self superclass.
  [aSuperclass == nil]
      whileFalse:
          [aSuperclass == aClass ifTrue: [^true].
          aSuperclass := aSuperclass superclass].
  ^false
!!


Object subclass: #BlockClosure
  instanceVariableNames: 'argc locals bytecode methodRecord argv handlerActive'.

"Blocks and evaluating blocks."
! Object
! isBlock
  ^ false
!!

! BlockClosure
! isBlock
  ^ true
! value
  <builtin: 'runBlock'>
! valueNoContextSwitch
  <builtin: 'runBlockNCS'>
! value: arg1
  <builtin: 'runBlock'>
! value: arg1 value: arg2
  <builtin: 'runBlock'>
! value: arg1 value: arg2 value: arg3
  <builtin: 'runBlock'>
! value: arg1 value: arg2 value: arg3 value: arg4
  <builtin: 'runBlock'>

! cull: aValue
  "Runs the receiver with 0 or 1 arguments, depending on what it expects.
  Allows flexible code that doesn't need to care if a block takes an argument."
  6 halt.
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
! NullObject
! ifNil: aBlock
  ^ aBlock value
! ifNil: aBlock ifNotNil: otherBlock
  ^ aBlock value
! isNil
  ^ true
!!

! Object
! ifNil: aBlock
  ^ self
! ifNil: aBlock ifNotNil: otherBlock
  ^ otherBlock value
! isNil
  ^ false
! isNotNil
  ^ self isNil not
!!


"Booleans and basic control flow."
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
!!


"Transcript"
Object subclass: #Transcript.

! Transcript class
! show: aValue
  <builtin: 'console.log'>
!!


! Object
! == anObject
  <builtin: '=='>
! ~= anObject
  ^ (self = anObject) not
!!


Object subclass: #Number
  instanceVariableNames: 'raw'.

! Number
! + aNumber
  <builtin: '+'>
! - aNumber
  <builtin: '-'>
! * aNumber
  <builtin: '*'>
! / aNumber
  <builtin: '/'>
! % aNumber
  <builtin: '%'>
! < aNumber
  <builtin: '<'>
! <= aNumber
  ^ (aNumber < self) not
! > aNumber
  ^ aNumber < self
! >= aNumber
  ^ (self < aNumber) not
! = aNumber
  <builtin: 'num='>
! ~= aNumber
  ^ (self = aNumber) not
! bitOr: aNumber
  <builtin: '|'>
! bitAnd: aNumber
  <builtin: '&'>
! bitXor: aNumber
  <builtin: '^'>
! bitInvert
  <builtin: '^-1'>
! negated
  ^0 - self
! floor
  ^ self < 0 ifTrue: [self negated floor negated] ifFalse: [self bitOr: 0]
! min: aNumber
  ^ self < aNumber ifTrue: [self] ifFalse: [aNumber]
! max: aNumber
  ^ aNumber min: self
! to: end do: aBlock
  | index |
  index := self.
  [index <= end] whileTrue:
      [aBlock value: index.
      index := index + 1]
! timesDo: aBlock
  | index |
  index := self.
  [index > 0] whileTrue:
      [aBlock cull: index.
      index := index - 1]
! between: lo and: hi
  ^ self >= lo and: [self <= hi]
!!


! CompiledMethod
! numArgs
  ^ argc
! selector
  ^ selector
!!
