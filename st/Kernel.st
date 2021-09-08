"Kernel classes: the founding classes that make the system run.
Only Object and Metaclass are defined in the core VM; everything else flows from
here. Several of the cornerstone methods, however, are just primitive calls to
Javascript functions."

! ClassDescription
! basicNew
  <builtin: 'basicNew'>
!  new
  | inst |
  inst := self basicNew.
  inst initialize.
  ^ inst
!!

"Do nothing in #initialize by default."
! Object
! initialize
  ^ self
! class
  <builtin: 'class'>
! yourself
  ^ self
! species
  "Provides the best class to use to clone the receiver. Defaults to self class
  but might be something different. Eg. Interval's species is Array."
  ^ self class
!!


"Add the asSymbol method to strings."
! String
! asSymbol
  <builtin: 'toSymbol'>
!!


"Subclass and its variations."
! Class
! subclass: aSymbol
  <builtin: 'subclass:'>
! subclass: aSymbol instanceVariableNames: aString
  <builtin: 'subclass:instanceVariableNames:'>
! subclass: aSymbol instanceVariableNames: aString classVariableNames: anotherString
  <builtin: 'subclass:instanceVariableNames:classVariableNames:'>

! superclass
  ^ superclass

! selectors
  "Answers an Array of Symbols for the methods on this class."
  <builtin: 'selectors'>
!!


Object subclass: #BlockClosure
  instanceVariableNames: 'bytecode argv argc methodRecord'.

Object subclass: #CompiledMethod
  instanceVariableNames: 'bytecode locals argc selector'.


"Blocks and evaluating blocks."
! BlockClosure
! isBlock
  ^ true
!!

! Object
! isBlock
  ^ true
!!

! BlockClosure
! value
  <builtin: 'runBlock'>
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
  ^ argc = 0
    ifTrue: [self value]
    ifFalse: [self value: aValue]
!!


"Nil and basic checking."
! NullObject
! ifNil: aBlock
  ^ aBlock value
! isNil
  ^ true
!!

! Object
! ifNil: aBlock
  ^ self
! isNil
  ^ false
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
!!


Object subclass: #Exception
  instanceVariableNames: 'context message'.

Exception >> ! message
  ^ message
!.

Exception >> ! message: aString
  message := aString
!.

Exception class >> ! cause: aString
  | e |
  e := self new.
  e message: aString.
  ^ e
!.


Exception subclass: #AbstractMethod.
AbstractMethod >> ! message
  ^ 'Send reached an abstract method - missing implementation'
!.

Object >> ! subclassResponsibility
  self error: AbstractMethod new
!.

