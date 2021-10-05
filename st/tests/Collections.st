TestCase subclass: #CollectionsGeneralArrays.

! CollectionsGeneralArrays
! testAdd
  self should: [(Array new: 5) add: 7] raise: ShouldNotImplement
! testAddAll
  self should: [(Array new: 5) addAll: {7. 8. 9}] raise: ShouldNotImplement
! testRemove
  self should: [(Array new: 5) remove: 7] raise: ShouldNotImplement
! testRemoveAll
  self should: [(Array new: 5) removeAll: {7. 8. 9}] raise: ShouldNotImplement

! testIncludes
  | arr |
  arr := {7. 8. 9} asArray.
  self assert: (arr includes: 7).
  self assert: (arr includes: 8).
  self assert: (arr includes: 9).
  self deny: (arr includes: nil).
  self deny: (arr includes: 12)
!!

"TODO: I eventually fixed the exceptions mess by removing a nested ensure: from
inside part of the event delivery. I'm not sure now what will happen if the
error-handling block of an on:do: itself has an error. I should test that."

"START HERE: First case of primitive failure: 7 = nil chokes assumes the arg
is a SmallInteger. I should be checking for that and causing the primitives to
fail."

Transcript show: CollectionsGeneralArrays suite run printString.
99 halt.
