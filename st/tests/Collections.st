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

! testIsEmpty
  self assert: Array new isEmpty.
  self deny: {7. 8. 9} isEmpty

! testOccurrencesOf
  | arr |
  arr := {1. 2. 3. 4. 5. 6. 7. 8. 9. 7. 5. 5. 5. 4. 2}.
  self assert: (arr occurrencesOf: 0) equals: 0.
  self assert: (arr occurrencesOf: 1) equals: 1.
  self assert: (arr occurrencesOf: 2) equals: 2.
  self assert: (arr occurrencesOf: 3) equals: 1.
  self assert: (arr occurrencesOf: 4) equals: 2.
  self assert: (arr occurrencesOf: 5) equals: 4.
  self assert: (arr occurrencesOf: 6) equals: 1.
  self assert: (arr occurrencesOf: 7) equals: 2.
  self assert: (arr occurrencesOf: 8) equals: 1.
  self assert: (arr occurrencesOf: 9) equals: 1

! testDo
  | count total arr |
  total := count := 0.
  arr := Array new: 5.
  {1. 3. 5. 7. 9} do: [:each |
    total := total + each.
    count := count + 1.
    arr at: count put: each * 2].

  self assert: count equals: 5.
  self assert: total equals: 25.
  self assert: arr equals: {2. 6. 10. 14. 18}

! testSelect
  | arr |
  arr := {1. 2. 3. 4. 5. 6} select: [:each | each even].
  self assert: arr class equals: Array.
  self assert: arr equals: {2. 4. 6}

! testReject
  | arr |
  arr := {1. 2. 3. 4. 5. 6} reject: [:each | each even].
  self assert: arr class equals: Array.
  self assert: arr equals: {1. 3. 5}

! testCollect
  self assert: ({1. 2. 3. 4. 5} collect: [:each | each * 2])
    equals: {2. 4. 6. 8. 10}

! testDetectIfNone
  self assert: ({1. 3. 5. 7} detect: [:x | x > 4] ifNone: [0]) equals: 5.
  self assert: ({1. 3. 5. 7} detect: [:x | x > 9] ifNone: [0]) equals: 0.

! testDetect
  self assert: ({1. 3. 5. 7} detect: [:x | x > 4]) equals: 5.
  self should: [{1. 3. 5. 7} detect: [:x | x > 9]] raise: NotFound.

! testInjectInto
  self assert: ({1. 3. 5. 7. 9} inject: 100 into: [:sum :x | sum - x])
    equals: 75

! testNew
  self assert: Array new equals: {}.
  self assert: (Array new: 3) equals: {nil. nil. nil}.

! testWiths
  self assert: (Array with: 7) equals: {7}.
  self assert: (Array with: 7 with: 8) equals: {7. 8}.
  self assert: (Array with: 7 with: 8 with: 9) equals: {7. 8. 9}.
  self assert: (Array with: 7 with: 8 with: 9 with: 10) equals: {7. 8. 9. 10}
!!



TestCase subclass: #CollectionsGeneralSets.

! CollectionsGeneralSets
! testAdd
  self assert: ((Set new: 5) add: 7; add: 8; add: 7) equals: {7. 8} asSet.
"! testAddAll
  self assert: (Set new addAll: {7. 8. 9. 7}) equals: {7. 8. 9} asSet.
! testRemove
  self assert: ({7. 8. 9} asSet remove: 7) equals: {8. 9} asSet.
! testRemoveAll
  self assert: ({5. 6. 7. 8. 9} asSet removeAll: {7. 8. 9})
    equals: {5. 6} asSet.

! testIncludes
  | arr |
  arr := {7. 8. 9} asArray.
  self assert: (arr includes: 7).
  self assert: (arr includes: 8).
  self assert: (arr includes: 9).
  self deny: (arr includes: nil).
  self deny: (arr includes: 12)

! testIsEmpty
  self assert: Array new isEmpty.
  self deny: {7. 8. 9} isEmpty

! testOccurrencesOf
  | arr |
  arr := {1. 2. 3. 4. 5. 6. 7. 8. 9. 7. 5. 5. 5. 4. 2}.
  self assert: (arr occurrencesOf: 0) equals: 0.
  self assert: (arr occurrencesOf: 1) equals: 1.
  self assert: (arr occurrencesOf: 2) equals: 2.
  self assert: (arr occurrencesOf: 3) equals: 1.
  self assert: (arr occurrencesOf: 4) equals: 2.
  self assert: (arr occurrencesOf: 5) equals: 4.
  self assert: (arr occurrencesOf: 6) equals: 1.
  self assert: (arr occurrencesOf: 7) equals: 2.
  self assert: (arr occurrencesOf: 8) equals: 1.
  self assert: (arr occurrencesOf: 9) equals: 1

! testDo
  | count total arr |
  total := count := 0.
  arr := Array new: 5.
  {1. 3. 5. 7. 9} do: [:each |
    total := total + each.
    count := count + 1.
    arr at: count put: each * 2].

  self assert: count equals: 5.
  self assert: total equals: 25.
  self assert: arr equals: {2. 6. 10. 14. 18}

! testSelect
  | arr |
  arr := {1. 2. 3. 4. 5. 6} select: [:each | each even].
  self assert: arr class equals: Array.
  self assert: arr equals: {2. 4. 6}

! testReject
  | arr |
  arr := {1. 2. 3. 4. 5. 6} reject: [:each | each even].
  self assert: arr class equals: Array.
  self assert: arr equals: {1. 3. 5}

! testCollect
  self assert: ({1. 2. 3. 4. 5} collect: [:each | each * 2])
    equals: {2. 4. 6. 8. 10}

! testDetectIfNone
  self assert: ({1. 3. 5. 7} detect: [:x | x > 4] ifNone: [0]) equals: 5.
  self assert: ({1. 3. 5. 7} detect: [:x | x > 9] ifNone: [0]) equals: 0.

! testDetect
  self assert: ({1. 3. 5. 7} detect: [:x | x > 4]) equals: 5.
  self should: [{1. 3. 5. 7} detect: [:x | x > 9]] raise: NotFound.

! testInjectInto
  self assert: ({1. 3. 5. 7. 9} inject: 100 into: [:sum :x | sum - x])
    equals: 75

! testNew
  self assert: Array new equals: {}.
  self assert: (Array new: 3) equals: {nil. nil. nil}.

! testWiths
  self assert: (Array with: 7) equals: {7}.
  self assert: (Array with: 7 with: 8) equals: {7. 8}.
  self assert: (Array with: 7 with: 8 with: 9) equals: {7. 8. 9}.
  self assert: (Array with: 7 with: 8 with: 9 with: 10) equals: {7. 8. 9. 10}
"
!!



Transcript show: (
  ArithmeticBasics suite,
  LogicBasics suite,
  CollectionsGeneralArrays suite,
  CollectionsGeneralSets suite
) run printString.

99 halt.
