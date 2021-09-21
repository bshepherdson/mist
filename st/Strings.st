! String
! beginsWith: aString
  <builtin: 'starts_with_str'>
! asString
  ^self
! , aString
  <builtin: 'str_concat'>
! printString
  ^ '''' , self , ''''
!!

! Symbol
! = anObject
  "Symbols are interned, so Symbols can short-circuit with ==."
  ^ self == anObject
!!

