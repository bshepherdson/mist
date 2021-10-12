TestCase subclass: #SequenceableCollectionOCs.

! SequenceableCollectionOCs
! testAtAll
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection atAll: {2. 3. 4} put: 0)
    equals: {9. 0. 0. 0. 5. 4} asOrderedCollection.

! testAtAllPut
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection atAllPut: 3)
    equals: {3. 3. 3. 3. 3. 3} asOrderedCollection.

! testFirst
  self assert: {6. 2. 3. 4} asOrderedCollection first equals: 6.
  self assert: {6} asOrderedCollection first equals: 6.
  self should: [OrderedCollection new first] raise: NotFound.

! testLast
  self assert: {6. 2. 3. 4} asOrderedCollection last equals: 4.
  self assert: {6} asOrderedCollection last equals: 6.
  self should: [OrderedCollection new last] raise: NotFound.

! testIndexOf
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 9) equals: 1.
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 7) equals: 3.
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 6) equals: 4.
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 2) equals: 0.

! testIndexOfIfAbsent
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 9 ifAbsent: [12])
    equals: 1.
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 7 ifAbsent: [12])
    equals: 3.
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 6 ifAbsent: [12])
    equals: 4.
  self assert: ({9. 8. 7. 6} asOrderedCollection indexOf: 2 ifAbsent: [12])
    equals: 12.

! testIndexOfSubCollection
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 1)
    equals: 3.
  "self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 2)
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 3)
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 4)
    equals: 0.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 15} asOrderedCollection
      startingAt: 1)
    equals: 0."

! testIndexOfSubCollectionIfAbsent
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 1
      ifAbsent: [99])
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 2
      ifAbsent: [99])
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 3
      ifAbsent: [99])
    equals: 3.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 5} asOrderedCollection
      startingAt: 4
      ifAbsent: [99])
    equals: 99.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      indexOfSubCollection: {7. 6. 15} asOrderedCollection
      startingAt: 1
      ifAbsent: [99])
    equals: 99.

! testReplaceFromToWith
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      replaceFrom: 3
      to: 5
      with: {1. 2. 3})
    equals: {9. 8. 1. 2. 3. 4} asOrderedCollection.
  self should: [{9. 8. 7. 6. 5. 4} asOrderedCollection
      replaceFrom: 3
      to: 5
      with: {1. 2}]
    raise: Error.

! testReplaceFromToWithStartingAt
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      replaceFrom: 3
      to: 5
      with: {1. 2. 3. 4. 5}
      startingAt: 2)
    equals: {9. 8. 2. 3. 4. 4} asOrderedCollection.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      replaceFrom: 3
      to: 5
      with: {1. 2. 3}
      startingAt: 1)
    equals: {9. 8. 1. 2. 3. 4} asOrderedCollection.
  self assert: ({9. 8. 7. 6. 5. 4} asOrderedCollection
      replaceFrom: 3
      to: 4
      with: {1. 2. 3}
      startingAt: 1)
    equals: {9. 8. 1. 2. 5. 4} asOrderedCollection.

! testComma
  "Result has the type of the left arg."
  self assert: {1. 2. 3} asOrderedCollection, {4. 5. 6}
    equals: {1. 2. 3. 4. 5. 6} asOrderedCollection.
  "self assert: {1. 2. 3}, {4. 5. 6} asOrderedCollection
    equals: {1. 2. 3. 4. 5. 6}.
  self assert: {1} asOrderedCollection, {4. 5} asOrderedCollection
    equals: {1. 4. 5} asOrderedCollection."

! testCopyFrom
  self assert: ({1. 2. 3. 4. 5} asOrderedCollection copyFrom: 3 to: 5)
    equals: {3. 4. 5} asOrderedCollection.
  self assert: ({1. 2. 3. 4. 5} asOrderedCollection copyFrom: 2 to: 2)
    equals: {2} asOrderedCollection.

! testCopyReplaceAllWith
  self assert: ({1. 2. 3. 4. 5. 6. 7. 8} asOrderedCollection
      copyReplaceAll: {4. 5. 6} with: {10. 12})
    equals: {1. 2. 3. 10. 12. 7. 8} asOrderedCollection.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 8} asOrderedCollection
      copyReplaceAll: {4} with: {10. 12})
    equals: {1. 2. 3. 10. 12. 5. 6. 7. 8} asOrderedCollection.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11} asOrderedCollection
      copyReplaceAll: {4} with: {10. 12})
    equals: {1. 2. 3. 10. 12. 5. 6. 7. 10. 12. 2. 11} asOrderedCollection.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11} asOrderedCollection
      copyReplaceAll: {1. 2. 3} with: {})
    equals: {4. 5. 6. 7. 4. 2. 11} asOrderedCollection.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11} asOrderedCollection
      copyReplaceAll: {2. 11} with: {13})
    equals: {1. 2. 3. 4. 5. 6. 7. 4. 13} asOrderedCollection.
  self assert: ({1. 2. 3. 4. 5. 6. 7. 4. 2. 11} asOrderedCollection
      copyReplaceAll: {2. 11} with: {})
    equals: {1. 2. 3. 4. 5. 6. 7. 4} asOrderedCollection.

! testCopyReplaceFromToWith
  self assert: ({9. 8. 7. 6. 5} asOrderedCollection
      copyReplaceFrom: 2 to: 4 with: {1. 2})
    equals: {9. 1. 2. 5} asOrderedCollection.
  self assert: ({9. 8. 7. 6. 5} asOrderedCollection
      copyReplaceFrom: 1 to: 0 with: {1. 2})
    equals: {1. 2. 9. 8. 7. 6. 5} asOrderedCollection.
  self assert: ({9. 8. 7. 6. 5} asOrderedCollection
      copyReplaceFrom: 6 to: 5 with: {1. 2})
    equals: {9. 8. 7. 6. 5. 1. 2} asOrderedCollection.
  self assert: ({9. 8. 7. 6. 5} asOrderedCollection
      copyReplaceFrom: 1 to: 5 with: {1. 2})
    equals: {1. 2} asOrderedCollection.

! testCopyWith
  self assert: ({9. 8. 7. 6} asOrderedCollection copyWith: 5)
    equals: {9. 8. 7. 6. 5} asOrderedCollection.

! testFindFirst
  self assert: ({9. 8. 7. 6} asOrderedCollection findFirst: [:x | x < 8])
    equals: 3.
  self assert: ({9. 8. 7. 6} asOrderedCollection findFirst: [:x | x < 4])
    equals: 0.

! testFindLast
  self assert: ({9. 8. 7. 6} asOrderedCollection findLast: [:x | x < 8])
    equals: 4.
  self assert: ({9. 8. 7. 6} asOrderedCollection findLast: [:x | x < 4])
    equals: 0.

! testReverseDo
  | arr count sum |
  sum := count := 0.
  arr := OrderedCollection new.
  {9. 8. 7. 6. 5} asOrderedCollection reverseDo: [:each |
    sum := sum + each.
    count := count + 1.
    arr add: each].

  self assert: sum equals: 35.
  self assert: count equals: 5.
  self assert: arr equals: {5. 6. 7. 8. 9} asOrderedCollection.

! testWithDo
  | result |
  result := OrderedCollection new.
  {1. 2. 3. 4. 5} asOrderedCollection with: {4. 7. 2. 3. 9}
    do: [:x :y | result add: x + y].
  self assert: result equals: {5. 9. 5. 7. 14} asOrderedCollection.

  self should: [{1. 2. 3} asOrderedCollection with: {2. 3}
      do: [:x :y | result add: x + y]]
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
  SequenceableCollectionArrays suite,
  SequenceableCollectionOCs suite
) run printString.

99 halt.
