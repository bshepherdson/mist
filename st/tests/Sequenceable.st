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
