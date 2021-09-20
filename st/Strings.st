! String
! beginsWith: aString
  <builtin: 'starts_with_str'>
! asString
  ^self
!!

! Symbol
! = anObject
  "Symbols are interned, so Symbols can short-circuit with ==."
  ^ self == anObject
!!

