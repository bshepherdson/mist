TestCase subclass: #OrderedCollectionTests.


! OrderedCollectionTests
! testAfter
  self assert: ({1. 2. 3. 4. 5. 6} asOrderedCollection after: 1)
    equals: 2.
  self assert: ({1. 2. 3. 4. 5. 6} asOrderedCollection after: 4)
    equals: 5.
  self assert: ({1. 2. 3. 4. 5. 6} asOrderedCollection after: 5)
    equals: 6.
  self should: [{1. 2. 3. 4. 5. 6} asOrderedCollection after: 6]
    raise: NotFound.
  self should: [{1. 2. 3. 4. 5. 6} asOrderedCollection after: 12]
    raise: NotFound.

! testBefore
  self assert: ({1. 2. 3. 4. 5. 6} asOrderedCollection before: 2)
    equals: 1.
  self assert: ({1. 2. 3. 4. 5. 6} asOrderedCollection before: 4)
    equals: 3.
  self assert: ({1. 2. 3. 4. 5. 6} asOrderedCollection before: 6)
    equals: 5.
  self should: [{1. 2. 3. 4. 5. 6} asOrderedCollection before: 1]
    raise: NotFound.
  self should: [{1. 2. 3. 4. 5. 6} asOrderedCollection before: 12]
    raise: NotFound.

! testAddAfter
  self assert: ({1. 2. 3. 4} asOrderedCollection add: 7 after: 3; yourself)
    equals: {1. 2. 3. 7. 4} asOrderedCollection.
  self assert: ({1. 2. 3. 4} asOrderedCollection add: 7 after: 4; yourself)
    equals: {1. 2. 3. 4. 7} asOrderedCollection.
  self should: [{1. 2. 3. 4} asOrderedCollection add: 7 after: 14; yourself]
    raise: NotFound.

! testAddBefore
  self assert: ({1. 2. 3. 4} asOrderedCollection add: 7 before: 3; yourself)
    equals: {1. 2. 7. 3. 4} asOrderedCollection.
  "self assert: ({1. 2. 3. 4} asOrderedCollection add: 7 before: 1; yourself)
    equals: {7. 1. 2. 3. 4} asOrderedCollection."
  self should: [{1. 2. 3. 4} asOrderedCollection add: 7 before: 14; yourself]
    raise: NotFound.

! testAddFirst
  | oc |
  oc := {1. 2. 3} asOrderedCollection.
  self assert: (oc addFirst: 7) equals: 7.
  self assert: (oc addFirst: 6) equals: 6.
  self assert: (oc addFirst: 5) equals: 5.
  self assert: (oc addFirst: 4) equals: 4.
  self assert: (oc addFirst: 3) equals: 3.
  self assert: (oc addFirst: 2) equals: 2.
  self assert: oc equals: {2. 3. 4. 5. 6. 7. 1. 2. 3} asOrderedCollection.

! testAddAllFirst
  | oc |
  oc := OrderedCollection new addAll: {9. 8. 7. 6. 5}; yourself.
  self assert: (oc addAllFirst: {1. 2. 3. 4. 5. 6}) equals: {1. 2. 3. 4. 5. 6}.
  self assert: oc equals: {1. 2. 3. 4. 5. 6. 9. 8. 7. 6. 5} asOrderedCollection.

  oc := OrderedCollection new addAll: {9. 8. 7. 6. 5}; yourself.
  self assert: (oc addAllFirst: {2}) equals: {2}.
  self assert: oc equals: {2. 9. 8. 7. 6. 5} asOrderedCollection.

! testAddAllLast
  | oc |
  oc := OrderedCollection new addAll: {9. 8. 7. 6. 5}; yourself.
  self assert: (oc addAllLast: {1. 2. 3. 4. 5. 6}) equals: {1. 2. 3. 4. 5. 6}.
  self assert: oc equals: {9. 8. 7. 6. 5. 1. 2. 3. 4. 5. 6} asOrderedCollection.

  oc := OrderedCollection new addAll: {9. 8. 7. 6. 5}; yourself.
  self assert: (oc addAllLast: {2}) equals: {2}.
  self assert: oc equals: {9. 8. 7. 6. 5. 2} asOrderedCollection.

! testRemoveFirst
  | oc |
  oc := OrderedCollection new addAll: {9. 8. 7. 6. 5}; yourself.
  self assert: oc removeFirst equals: 9.
  self assert: oc removeFirst equals: 8.
  self assert: oc equals: {7. 6. 5} asOrderedCollection.
  self assert: oc size equals: 3.

! testRemoveLast
  | oc |
  oc := OrderedCollection new addAll: {9. 8. 7. 6. 5}; yourself.
  self assert: oc removeLast equals: 5.
  self assert: oc removeLast equals: 6.
  self assert: oc equals: {9. 8. 7} asOrderedCollection.
  self assert: oc size equals: 3.

!!

