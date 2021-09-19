! String
! startsWith: aString
  <builtin: 'starts_with_str'>
!!

! Symbol
! = anObject
  "Symbols are interned, so Symbols can short-circuit with ==."
  ^ self == anObject
!!

