TestCase subclass: #SortedCollectionTests.


! SortedCollectionTests
! testSorting
  | sc |
  sc := SortedCollection new.
  sc add: 6.
  sc add: 3.
  sc add: 1.
  sc add: 8.
  sc add: 5.
  self assert: sc asArray equals: {1. 3. 5. 6. 8}.

  self assert: {7. 3. 5. 6. 1. 2} asSortedCollection asArray
    equals: {1. 2. 3. 5. 6. 7}.
  self assert: ({7. 3. 5. 6. 1. 2} asSortedCollection: [:a :b | a > b]) asArray
    equals: {7. 6. 5. 3. 2. 1}.
  self assert: 'a string' asSortedCollection asArray
    equals: ' aginrst' asArray.
  sc := {7. 3. 5. 4. 1. 9} asSortedCollection.
  self assert: sc asArray equals: {1. 3. 4. 5. 7. 9}.
  sc sortBlock: [:a :b | a > b].
  self assert: sc asArray equals: {9. 7. 5. 4. 3. 1}.
!!

