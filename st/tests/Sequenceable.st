TestCase subclass: #SequenceableCollectionTests.

! SequenceableCollectionTests
! testAtAll
  self assert: ({9. 8. 7. 6. 5. 4} atAll: {2. 3. 4} put: 0)
    equals: {9. 0. 0. 0. 5. 4}.

! testAtAllPut
  self assert: ({9. 8. 7. 6. 5. 4} atAllPut: 3)
    equals: {3. 3. 3. 3. 3. 3}.

! testFirst
  self assert: {6. 2. 3. 4} first equals: 6.
  self assert: {6} first equals: 6.
  self should: [Array new first] raise: NotFound.

! testLast
  self assert: {6. 2. 3. 4} last equals: 4.
  self assert: {6} last equals: 6.
  self should: [Array new last] raise: NotFound.

! testIndexOf
  self assert: ({9. 8. 7. 6} indexOf: 9) equals: 1.
  self assert: ({9. 8. 7. 6} indexOf: 7) equals: 3.
  self assert: ({9. 8. 7. 6} indexOf: 6) equals: 4.
  self assert: ({9. 8. 7. 6} indexOf: 2) equals: 0.

! testIndexOfIfAbsent
  self assert: ({9. 8. 7. 6} indexOf: 9 ifAbsent: [12]) equals: 1.
  self assert: ({9. 8. 7. 6} indexOf: 7 ifAbsent: [12]) equals: 3.
  self assert: ({9. 8. 7. 6} indexOf: 6 ifAbsent: [12]) equals: 4.
  self assert: ({9. 8. 7. 6} indexOf: 2 ifAbsent: [12]) equals: 12.

! testIndexOfSubCollection
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 1)
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 2)
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 3)
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 4)
    equals: 0.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 15}
      startingAt: 1)
    equals: 0.

! testIndexOfSubCollectionIfAbsent
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 1
      ifAbsent: [99])
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 2
      ifAbsent: [99])
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 3
      ifAbsent: [99])
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 5}
      startingAt: 4
      ifAbsent: [99])
    equals: 99.
  self assert: ({9. 8. 7. 6. 5. 4}
      indexOfSubCollection: {7. 6. 15}
      startingAt: 1
      ifAbsent: [99])
    equals: 99.

! testReplaceFromToWith
  self assert: ({9. 8. 7. 6. 5. 4}
      replaceFrom: 3
      to: 5
      with: {1. 2. 3})
    equals: {9. 8. 1. 2. 3. 4}.
  self should: [{9. 8. 7. 6. 5. 4}
      replaceFrom: 3
      to: 5
      with: {1. 2}]
    raise: Error.

! testReplaceFromToWithStartingAt
  self assert: ({9. 8. 7. 6. 5. 4}
      replaceFrom: 3
      to: 5
      with: {1. 2. 3. 4. 5}
      startingAt: 2)
    equals: {9. 8. 2. 3. 4. 4}.
  self assert: ({9. 8. 7. 6. 5. 4}
      replaceFrom: 3
      to: 5
      with: {1. 2. 3}
      startingAt: 1)
    equals: {9. 8. 1. 2. 3. 4}.
  self assert: ({9. 8. 7. 6. 5. 4}
      replaceFrom: 3
      to: 4
      with: {1. 2. 3}
      startingAt: 1)
    equals: {9. 8. 1. 2. 5. 4}.

! testComma
  self assert: {1. 2. 3}, {4. 5. 6} equals: {1. 2. 3. 4. 5. 6}.
  self assert: {1}, {4. 5} equals: {1. 4. 5}.

! testCopyFrom
  self assert: ({1. 2. 3. 4. 5} copyFrom: 3 to: 5) equals: {3. 4. 5}.
  self assert: ({1. 2. 3. 4. 5} copyFrom: 2 to: 2) equals: {2}.

! testCopyReplaceAllWith
  self assert: ({1. 2. 3. 4. 5. 6. 7. 8}
      copyReplaceAll: {4. 5. 6} with: {10. 12})
    equals: {1. 2. 3. 10. 12. 7. 8}.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 8}
      copyReplaceAll: {4} with: {10. 12})
    equals: {1. 2. 3. 10. 12. 5. 6. 7. 8}.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11}
      copyReplaceAll: {4} with: {10. 12})
    equals: {1. 2. 3. 10. 12. 5. 6. 7. 10. 12. 2. 11}.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11}
      copyReplaceAll: {1. 2. 3} with: {})
    equals: {4. 5. 6. 7. 4. 2. 11}.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11}
      copyReplaceAll: {2. 11} with: {13})
    equals: {1. 2. 3. 4. 5. 6. 7. 4. 13}.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11}
      copyReplaceAll: {2. 11} with: {})
    equals: {1. 2. 3. 4. 5. 6. 7. 4}.

! testCopyReplaceFromToWith
  self assert: ({9. 8. 7. 6. 5} copyReplaceFrom: 2 to: 4 with: {1. 2})
    equals: {9. 1. 2. 5}.
  self assert: ({9. 8. 7. 6. 5} copyReplaceFrom: 1 to: 0 with: {1. 2})
    equals: {1. 2. 9. 8. 7. 6. 5}.
  self assert: ({9. 8. 7. 6. 5} copyReplaceFrom: 6 to: 5 with: {1. 2})
    equals: {9. 8. 7. 6. 5. 1. 2}.
  self assert: ({9. 8. 7. 6. 5} copyReplaceFrom: 1 to: 5 with: {1. 2})
    equals: {1. 2}.

! testCopyWith
  self assert: ({9. 8. 7. 6} copyWith: 5) equals: {9. 8. 7. 6. 5}.

! testFindFirst
  self assert: ({9. 8. 7. 6} findFirst: [:x | x < 8]) equals: 3.
  self assert: ({9. 8. 7. 6} findFirst: [:x | x < 4]) equals: 0.

! testFindLast
  self assert: ({9. 8. 7. 6} findLast: [:x | x < 8]) equals: 4.
  self assert: ({9. 8. 7. 6} findLast: [:x | x < 4]) equals: 0.

! testReverseDo
  | arr count sum |
  sum := count := 0.
  arr := OrderedCollection new.
  {9. 8. 7. 6. 5} reverseDo: [:each |
    sum := sum + each.
    count := count + 1.
    arr add: each].

  self assert: sum equals: 35.
  self assert: count equals: 5.
  self assert: arr equals: {5. 6. 7. 8. 9} asOrderedCollection.

! testWithDo
  | result |
  result := OrderedCollection new.
  {1. 2. 3. 4. 5} with: {4. 7. 2. 3. 9} do: [:x :y | result add: x + y].
  self assert: result equals: {5. 9. 5. 7. 14} asOrderedCollection.

  self should: [{1. 2. 3} with: {2. 3} do: [:x :y | result add: x + y]]
    raise: Error.
!!


Transcript show: (
  ArithmeticBasics suite,
  LogicBasics suite,
  CollectionsGeneralArrays suite,
  CollectionsGeneralSets suite,
  CollectionsGeneralBags suite,
  CollectionsGeneralDictionary suite,
  CollectionsGeneralOrderedCollection suite,
  DictionariesTests suite,
  SequenceableCollectionTests suite
) run printString.

99 halt.
