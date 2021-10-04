! String
! beginsWith: aString
  self size < aString size ifTrue: [^false].
  1 to: aString size do: [:i | (self at: i) = (aString at: i) ifFalse: [^false]].
  ^true
! asString
  ^self
! , aString
  | s |
  s := String new: self size + aString size.
  1 to: self size do: [:i | s at: i put: (self at: i)].
  1 to: aString size do: [:i | s at: i + self size put: (aString at: i)].
  ^ s
! printString
  ^ '''' , self , ''''
!!

! Symbol
! = anObject
  "Symbols are interned, so Symbols can short-circuit with ==."
  ^ self == anObject
! asSymbol
  ^self
!!

