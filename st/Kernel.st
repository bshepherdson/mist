"Kernel classes: the founding classes that make the system run.
Only Object and Metaclass are defined in the core VM; everything else flows from
here. Several of the cornerstone methods, however, are just primitive calls to
Javascript functions."

Class >> ! basicNew
  ^ <builtin: 'basicNew'>
!.

Class >> ! new
  | inst |
  inst := self basicNew.
  inst initialize.
  ^ inst
!.

"Do nothing in #initialize by default."
Object >> ! initialize
  ^ self
!.

Object >> ! class
  ^ <builtin: 'class'>
!.


"Add the asSymbol method to strings."
String >> ! asSymbol
  ^ <builtin: 'toSymbol'>
!.


"Subclass and its variations."
Class >> ! subclass: aSymbol
  ^ <builtin: 'subclass:'>
!.
Class >> ! subclass: aSymbol instanceVariableNames: aString
  ^ <builtin: 'subclass:instanceVariableNames:'>
!.
Class >> ! subclass: aSymbol instanceVariableNames: aString classVariableNames: anotherString
  ^ <builtin: 'subclass:instanceVariableNames:classVariableNames:'>
!.

Class >> ! superclass
  ^ superclass
!.

"Object subclass: #Collection.
Collection subclass: #SequenceableCollection.
SequenceableCollection subclass: #ArrayedCollection.
ArrayedCollection subclass: #Array.
ArrayedCollection subclass: #String.
String subclass: #Symbol instanceVariableNames: 'raw'."

Object subclass: #BlockClosure
  instanceVariableNames: 'bytecode argv argc'.

Object subclass: #CompiledMethod
  instanceVariableNames: 'bytecode locals argc selector'.

