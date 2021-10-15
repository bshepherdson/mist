! String
! beginsWith: aString
  self size < aString size ifTrue: [^false].
  1 to: aString size do: [:i | (self at: i) = (aString at: i) ifFalse: [^false]].
  ^true

! at: anIndex
  ^ Character value: (super at: anIndex)
! at: anIndex put: aCharacter
  super at: anIndex put: aCharacter asciiValue

! isLiteral
  ^ true

! asString
  ^self
! printOn: aStream
  aStream nextPut: $'.
  self putOn: aStream.
  aStream nextPut: $'

! printString
  ^ '''' , self , ''''

! putOn: aStream
  aStream nextPutAll: self
!!

! Symbol
! = anObject
  "Symbols are interned, so Symbols can short-circuit with ==."
  ^ self == anObject
! asSymbol
  ^self
!!

