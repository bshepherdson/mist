TestCase subclass: #ArithmeticBasics.

! ArithmeticBasics
! testPlus0
  self assert: 7 + 0 equals: 7
! testPlus1
  self assert: 7 + 1 equals: 8
! testPlus1Negative
  self assert: -7 + 1 equals: -6
! testPlus1NegativeToZero
  self assert: -1 + 1 equals: 0

! testMultBy0
  self assert: 90 * 0 equals: 0.
  self assert:  0 * 0 equals: 0.
  self assert: -7 * 0 equals: 0.
  self assert: 0 * 90 equals: 0.
  self assert: 0 *  0 equals: 0.
  self assert: 0 * -7 equals: 0
! testMultBy1
  self assert: 90 * 1 equals: 90.
  self assert:  0 * 1 equals:  0.
  self assert: -7 * 1 equals: -7.
  self assert: 1 * 90 equals: 90.
  self assert: 1 *  0 equals:  0.
  self assert: 1 * -7 equals: -7

! testMult
  self assert:  90 *  7 equals:  630.
  self assert:  90 * -7 equals: -630.
  self assert: -90 *  7 equals: -630.
  self assert: -90 * -7 equals:  630

! testBitXor
  self assert: (2r0011 bitXor: 2r0110) equals: 2r0101
! testBitAnd
  self assert: (2r0011 bitAnd: 2r0110) equals: 2r0010
! testBitOr
  self assert: (2r0011 bitOr:  2r0110) equals: 2r0111
! testBitShift
  self assert: (2r0011 bitOr:  2r0110) equals: 2r0111
! testBitInvert
  self assert:  0 bitInvert equals: -1. "2s complement"
  self assert: -1 bitInvert equals:  0. "2s complement"
  self assert:  3 bitInvert equals: -4  "2s complement"

! testMin
  self assert: ( 7 min:  0) equals:  0.
  self assert: (-7 min:  0) equals: -7.
  self assert: ( 7 min:  7) equals:  7.
  self assert: (-7 min:  7) equals: -7.
  self assert: (-7 min: -4) equals: -7.
  self assert: ( 7 min:  4) equals:  4
! testMax
  self assert: ( 7 max:  0) equals:  7.
  self assert: (-7 max:  0) equals:  0.
  self assert: ( 7 max:  7) equals:  7.
  self assert: (-7 max:  7) equals:  7.
  self assert: (-7 max: -4) equals: -4.
  self assert: ( 7 max:  4) equals:  7

! testBetween
  self assert: ( 6 between: 3 and: 9).
  self assert: ( 3 between: 3 and: 9).
  self assert: ( 9 between: 3 and: 9).
  self deny:   ( 2 between: 3 and: 9).
  self deny:   (10 between: 3 and: 9).

  self assert: ( 6 between: -3 and: 9).
  self assert: ( 3 between: -3 and: 9).
  self assert: ( 9 between: -3 and: 9).
  self assert: ( 2 between: -3 and: 9).
  self assert: ( 0 between: -3 and: 9).
  self assert: (-2 between: -3 and: 9).
  self assert: (-3 between: -3 and: 9).
  self deny:   (-4 between: -3 and: 9)

! testToDo
  | arr |
  arr := Array new: 5.
  1 to: 5 do: [:i | arr at: i put: i * i].
  self assert: arr equals: {1. 4. 9. 16. 25} asArray

! testTimesRepeat
  | arr |
  arr := OrderedCollection new: 3.
  3 timesRepeat: [arr add: 7].
  self assert: arr asArray equals: {7. 7. 7}

!!

TestCase subclass: #LogicBasics.
! LogicBasics
! testAnd
  | x |
  self deny:   (false and: [false]).
  self deny:   (true  and: [false]).
  self deny:   (false and: [true]).
  self assert: (true  and: [true]).

  x := 2.
  false and: [x := 1].
  self assert: x equals: 2 "Short-circuiting works"

! testOr
  | x |
  self deny:   (false or: [false]).
  self assert: (true  or: [false]).
  self assert: (false or: [true]).
  self assert: (true  or: [true]).

  x := 2.
  true or: [x := 1].
  self assert: x equals: 2 "Short-circuiting works"

! testIfTrue
  self assert: (true  ifTrue: [7]) equals: 7.
  self assert: (false ifTrue: [7]) equals: nil

! testIfFalse
  self assert: (true  ifFalse: [7]) equals: nil.
  self assert: (false ifFalse: [7]) equals: 7

! testIfTrueIfFalse
  self assert: (true  ifTrue: [6] ifFalse: [7]) equals: 6.
  self assert: (false ifTrue: [6] ifFalse: [7]) equals: 7
! testIfFalseIfTrue
  self assert: (true  ifFalse: [6] ifTrue: [7]) equals: 7.
  self assert: (false ifFalse: [6] ifTrue: [7]) equals: 6

! testIfNilAndFriends
  self assert: (nil  ifNotNil: [6] ifNil: [7]) equals: 7.
  self assert: (17   ifNotNil: [6] ifNil: [7]) equals: 6.
  self assert: (nil  ifNotNil: [6] ifNil: [7]) equals: 7.
  self assert: (17   ifNotNil: [6] ifNil: [7]) equals: 6
!!



