TestCase subclass: #CollectionsGeneralArrays.

! CollectionsGeneralArrays
"! testAdd
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
"

! testIsEmpty
  self assert: Array new isEmpty.
  self deny: {7. 8. 9} asArray isEmpty

"! testOccurrencesOf"

!!

"TODO: I eventually fixed the exceptions mess by removing a nested ensure: from
inside part of the event delivery. I'm not sure now what will happen if the
error-handling block of an on:do: itself has an error. I should test that."

"START HERE: Still problems with exceptions! This current one I've got figured
out, and it's a doozy: in MethodContext>>resume:through: there's a whileFalse:
that calls thisContext in the body. In real Smalltalks eg. Pharo thisContext is
the method's context, and there's no actual return, since it's being rewritten
into a loop! In mine, whileFalse: is a real call, so thisContext is different.
It gets rewritten, and then at the end of the whileFalse: body it returns to
the wrong place!
I'll have to bite the bullet and rewrite the methods to turn those into loops."

Transcript show: (
  ArithmeticBasics suite,
  LogicBasics suite,
  CollectionsGeneralArrays suite
) run printString.
99 halt.
