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
  self assert: Array new isEmpty not.
  self deny: {7. 8. 9} asArray isEmpty

"! testOccurrencesOf"

!!

"TODO: I eventually fixed the exceptions mess by removing a nested ensure: from
inside part of the event delivery. I'm not sure now what will happen if the
error-handling block of an on:do: itself has an error. I should test that."

"START HERE: Trouble with exceptions still (of course). It's correctly executing
handlers and correctly unwinding ensure:'s but it's not returning to the right
places after the ensures are done.
I think the flow we want is to chase on:do: handlers until one handles this
error, then unwind the stack through any ensure: or ifCurtailed: blocks until
at last we return as if from on:do: with the original exception handler's return
value.

Ah! I've got it I think: primitives in Pharo are *immediately executed by the
caller*, there's no stack from for them. I should make that change and then I
think all the stack unwinding will start working properly."

Object subclass: #Playground.
! Playground
! trial
  ^ [[Error new signal. 7] ensure: [Transcript show: 'ensured']] on: Error do: [8]
!!

Transcript show: Playground new trial.
"Transcript show: CollectionsGeneralArrays suite run printString."
99 halt.
