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
  self assert: ((Set new: 5) add: 7; add: 8; add: 7; yourself)
    equals: {7. 8} asSet.
! testAddAll
  self assert: (Set new addAll: {7. 8. 9. 7}; yourself) equals: {7. 8. 9} asSet.
! testRemove
  self assert: ({7. 8. 9} asSet remove: 7; yourself) equals: {8. 9} asSet.
! testRemoveAll
  self assert: ({5. 6. 7. 8. 9} asSet removeAll: {7. 8. 9}; yourself)
    equals: {5. 6} asSet.
! testIncludes
  | set |
  set := {7. 8. 9} asSet.
  self assert: (set includes: 7).
  self assert: (set includes: 8).
  self assert: (set includes: 9).
  self deny: (set includes: nil).
  self deny: (set includes: 12)

! testIsEmpty
  self assert: Set new isEmpty.
  self deny: {7. 8. 9} asSet isEmpty

! testOccurrencesOf
  | set |
  set := {1. 2. 3. 4. 5. 6. 7. 8. 9. 7. 5. 5. 5. 4. 2} asSet.
  self assert: (set occurrencesOf: 0) equals: 0.
  self assert: (set occurrencesOf: 1) equals: 1.
  self assert: (set occurrencesOf: 2) equals: 1.
  self assert: (set occurrencesOf: 3) equals: 1.
  self assert: (set occurrencesOf: 4) equals: 1.
  self assert: (set occurrencesOf: 5) equals: 1.
  self assert: (set occurrencesOf: 6) equals: 1.
  self assert: (set occurrencesOf: 7) equals: 1.
  self assert: (set occurrencesOf: 8) equals: 1.
  self assert: (set occurrencesOf: 9) equals: 1

! testDo
  | count total set |
  total := count := 0.
  set := Set new: 5.
  {1. 3. 5. 7. 9} asSet do: [:each |
    total := total + each.
    count := count + 1.
    set add: each * 2].

  self assert: count equals: 5.
  self assert: total equals: 25.
  self assert: set equals: {2. 6. 10. 14. 18} asSet

! testSelect
  | set |
  set := {1. 2. 3. 4. 5. 6} asSet select: [:each | each even].
  self assert: set class equals: Set.
  self assert: set equals: {2. 4. 6} asSet

! testReject
  | set |
  set := {1. 2. 3. 4. 5. 6} asSet reject: [:each | each even].
  self assert: set class equals: Set.
  self assert: set equals: {1. 3. 5} asSet

! testCollect
  self assert: ({1. 2. 3. 4. 5} asSet collect: [:each | each * 2])
    equals: {2. 4. 6. 8. 10} asSet

! testDetectIfNone
  self assert: ({5. 7} asSet includes: (
    {1. 3. 5. 7} asSet detect: [:x | x > 4] ifNone: [0])).
  self assert: ({1. 3. 5. 7} asSet detect: [:x | x > 9] ifNone: [0])
    equals: 0.

! testDetect
  self assert: ({5. 7} asSet includes: (
    {1. 3. 5. 7} asSet detect: [:x | x > 4])).
  self should: [{1. 3. 5. 7} asSet detect: [:x | x > 9]]
    raise: NotFound.

! testInjectInto
  self assert: ({1. 3. 5. 7. 9} asSet inject: 100 into: [:sum :x | sum - x])
    equals: 75

! testWiths
  self assert: (Set with: 7) equals: {7} asSet.
  self assert: (Set with: 7 with: 8) equals: {7. 8} asSet.
  self assert: (Set with: 7 with: 8 with: 9) equals: {7. 8. 9} asSet.
  self assert: (Set with: 7 with: 8 with: 9 with: 10)
    equals: {7. 8. 9. 10} asSet
!!

TestCase subclass: #CollectionsGeneralBags.

! CollectionsGeneralBags
! testAdd
  self assert: ((Bag new: 5) add: 7; add: 8; add: 7; yourself)
    equals: {7. 8. 7} asBag.
! testAddAll
  self assert: (Bag new addAll: {7. 8. 9. 7}; yourself)
    equals: {7. 7. 8. 9} asBag.
! testRemove
  self assert: ({7. 8. 9} asBag remove: 7; yourself)
    equals: {8. 9} asBag.
! testRemoveAll
  self assert: ({5. 6. 7. 8. 9} asBag removeAll: {7. 8. 9}; yourself)
    equals: {5. 6} asBag.

! testIncludes
  | bag |
  bag := {7. 8. 9. 8. 6. 7} asBag.
  self assert: (bag includes: 7).
  self assert: (bag includes: 8).
  self assert: (bag includes: 9).
  self deny: (bag includes: nil).
  self deny: (bag includes: 12)

! testIsEmpty
  self assert: Bag new isEmpty.
  self deny: {7. 8. 9} asBag isEmpty

! testOccurrencesOf
  | bag |
  bag := {1. 2. 3. 4. 5. 6. 7. 8. 9. 7. 5. 5. 5. 4. 2} asBag.
  self assert: (bag occurrencesOf: 0) equals: 0.
  self assert: (bag occurrencesOf: 1) equals: 1.
  self assert: (bag occurrencesOf: 2) equals: 2.
  self assert: (bag occurrencesOf: 3) equals: 1.
  self assert: (bag occurrencesOf: 4) equals: 2.
  self assert: (bag occurrencesOf: 5) equals: 4.
  self assert: (bag occurrencesOf: 6) equals: 1.
  self assert: (bag occurrencesOf: 7) equals: 2.
  self assert: (bag occurrencesOf: 8) equals: 1.
  self assert: (bag occurrencesOf: 9) equals: 1

! testDo
  | count total bag |
  total := count := 0.
  bag := Bag new: 5.
  {1. 3. 5. 3. 9} asBag do: [:each |
    total := total + each.
    count := count + 1.
    bag add: each * 2].

  self assert: count equals: 5.
  self assert: total equals: 21.
  self assert: bag equals: {2. 6. 10. 6. 18} asBag

! testSelect
  | bag |
  bag := {1. 2. 3. 4. 3. 2} asBag select: [:each | each even].
  self assert: bag class equals: Bag.
  self assert: bag equals: {2. 4. 2} asBag

! testReject
  | bag |
  bag := {1. 2. 3. 4. 2. 5. 4. 1} asBag reject: [:each | each even].
  self assert: bag class equals: Bag.
  self assert: bag equals: {1. 3. 5. 1} asBag

! testCollect
  self assert: ({1. 2. 3. 4. 5. 3. 5. 6} asBag collect: [:each | each * 2])
    equals: {2. 4. 6. 8. 10. 6. 10. 12} asBag

! testDetectIfNone
  self assert: ({5. 7. 5. 6} asBag includes: (
    {1. 1. 3. 5. 6. 5. 7} asBag detect: [:x | x > 4] ifNone: [0])).
  self assert: ({1. 3. 5. 7} asBag detect: [:x | x > 9] ifNone: [0])
    equals: 0.

! testDetect
  self assert: ({5. 7. 5. 6} asBag includes: (
    {1. 3. 5. 6. 5. 7} asBag detect: [:x | x > 4])).
  self should: [{1. 3. 5. 7} asBag detect: [:x | x > 9]]
    raise: NotFound.

! testInjectInto
  self assert: ({1. 3. 5. 3. 9} asBag inject: 100 into: [:sum :x | sum - x])
    equals: 79

! testWiths
  self assert: (Bag with: 7) equals: {7} asBag.
  self assert: (Bag with: 7 with: 8) equals: {7. 8} asBag.
  self assert: (Bag with: 7 with: 8 with: 9) equals: {7. 8. 9} asBag.
  self assert: (Bag with: 7 with: 8 with: 7 with: 10)
    equals: {7. 8. 7. 10} asBag
!!


TestCase subclass: #CollectionsGeneralOrderedCollection.

! CollectionsGeneralOrderedCollection
! testAdd
  | oc |
  oc := OrderedCollection new: 5.
  1 to: 20 do: [:ix | oc add: ix].

  self assert: oc size equals: 20.
  self assert: (oc at: 1) equals: 1.
  self assert: (oc at: 6) equals: 6.
  self assert: (oc at: 2) equals: 2.
  self assert: (oc at: 20) equals: 20
! testAddAll
  self assert: (OrderedCollection new addAll: {7. 8. 9}; yourself)
    equals: {7. 8. 9} asOrderedCollection
! testRemove
  | oc |
  oc := OrderedCollection new addAll: {4. 5. 6. 7. 8}; yourself.
  self assert: oc size equals: 5.
  self assert: (oc at: 2) equals: 5.
  oc remove: 5.
  self assert: oc size equals: 4.
  self assert: (oc at: 1) equals: 4.
  self assert: (oc at: 2) equals: 6.
  self assert: oc equals: {4. 6. 7. 8} asOrderedCollection

! testRemoveAll
  self assert: ((OrderedCollection new
        addAll: {4. 5. 6. 7. 8. 9. 10};
        yourself)
      removeAll: {7. 8. 9};
      yourself)
    equals: {4. 5. 6. 10} asOrderedCollection.

! testIncludes
  | oc |
  oc := {7. 8. 9} asOrderedCollection.
  self assert: (oc includes: 7).
  self assert: (oc includes: 8).
  self assert: (oc includes: 9).
  self deny: (oc includes: nil).
  self deny: (oc includes: 12)

! testIsEmpty
  self assert: OrderedCollection new isEmpty.
  self deny: {7. 8. 9} asOrderedCollection isEmpty

! testOccurrencesOf
  | oc |
  oc := {1. 2. 3. 4. 5. 6. 7. 8. 9. 7. 5. 5. 5. 4. 2} asOrderedCollection.
  self assert: (oc occurrencesOf: 0) equals: 0.
  self assert: (oc occurrencesOf: 1) equals: 1.
  self assert: (oc occurrencesOf: 2) equals: 2.
  self assert: (oc occurrencesOf: 3) equals: 1.
  self assert: (oc occurrencesOf: 4) equals: 2.
  self assert: (oc occurrencesOf: 5) equals: 4.
  self assert: (oc occurrencesOf: 6) equals: 1.
  self assert: (oc occurrencesOf: 7) equals: 2.
  self assert: (oc occurrencesOf: 8) equals: 1.
  self assert: (oc occurrencesOf: 9) equals: 1

! testDo
  | count total arr |
  total := count := 0.
  arr := Array new: 5.
  {1. 3. 5. 7. 9} asOrderedCollection do: [:each |
    total := total + each.
    count := count + 1.
    arr at: count put: each * 2].

  self assert: count equals: 5.
  self assert: total equals: 25.
  self assert: arr equals: {2. 6. 10. 14. 18}

! testSelect
  | oc |
  oc := {1. 2. 3. 4. 5. 6} asOrderedCollection select: [:each | each even].
  self assert: oc class equals: OrderedCollection.
  self assert: oc equals: {2. 4. 6} asOrderedCollection

! testReject
  | oc |
  oc := {1. 2. 3. 4. 5. 6} asOrderedCollection reject: [:each | each even].
  self assert: oc class equals: OrderedCollection.
  self assert: oc equals: {1. 3. 5} asOrderedCollection

! testCollect
  self assert: ({1. 2. 3. 4. 5} asOrderedCollection collect: [:each | each * 2])
    equals: {2. 4. 6. 8. 10} asOrderedCollection

! testDetectIfNone
  self assert: ({1. 3. 5. 7} asOrderedCollection detect: [:x | x > 4] ifNone: [0]) equals: 5.
  self assert: ({1. 3. 5. 7} asOrderedCollection detect: [:x | x > 9] ifNone: [0]) equals: 0.

! testDetect
  self assert: ({1. 3. 5. 7} asOrderedCollection detect: [:x | x > 4])
    equals: 5.
  self should: [{1. 3. 5. 7} asOrderedCollection detect: [:x | x > 9]]
    raise: NotFound.

! testInjectInto
  self assert: ({1. 3. 5. 7. 9} asOrderedCollection
      inject: 100 into: [:sum :x | sum - x])
    equals: 75

! testNew
  | oc |
  oc := OrderedCollection new.
  self assert: oc equals: {} asOrderedCollection.
  self assert: oc size equals: 0.
  self assert: oc isEmpty.
  self assert: oc capacity > 0.


  oc := OrderedCollection new: 20.
  self assert: oc capacity equals: 20.
  self assert: oc size equals: 0.
  self assert: oc isEmpty.

! testWiths
  self assert: (OrderedCollection with: 7)
    equals: {7} asOrderedCollection.
  self assert: (OrderedCollection with: 7 with: 8)
    equals: {7. 8} asOrderedCollection.
  self assert: (OrderedCollection with: 7 with: 8 with: 9)
    equals: {7. 8. 9} asOrderedCollection.
  self assert: (OrderedCollection with: 7 with: 8 with: 9 with: 10)
    equals: {7. 8. 9. 10} asOrderedCollection
!!


TestCase subclass: #CollectionsGeneralDictionary.

! CollectionsGeneralDictionary
! testAdd
  self assert: ((Dictionary new: 5)
      add: (Association key: 1 value: 7);
      add: (Association key: 2 value: 8);
      add: (Association key: 3 value: 7);
      yourself)
    equals: (Dictionary new addAll: {7. 8. 7}; yourself).
! testAddAll
  self assert: (Dictionary new addAll: {7. 8. 9. 7}; yourself)
    equals: (Dictionary new
      at: 1 put: 7;
      at: 2 put: 8;
      at: 3 put: 9;
      at: 4 put: 7;
      yourself).
! testRemove
  self should: [Dictionary new remove: 7] raise: ShouldNotImplement.
! testRemoveAll
  self should: [Dictionary new removeAll: {7. 8. 9}]
    raise: ShouldNotImplement.

! testIncludes
  | d |
  d := Dictionary new addAll: {7. 8. 9. 8. 6. 7}; yourself.
  self assert: (d includes: 7).
  self assert: (d includes: 8).
  self assert: (d includes: 9).
  self deny: (d includes: nil).
  self deny: (d includes: 12)

! testIsEmpty
  self assert: Dictionary new isEmpty.
  self deny: (Dictionary new addAll: {7. 8. 9}; yourself) isEmpty

! testOccurrencesOf
  | d |
  d := Dictionary new
    addAll: {1. 2. 3. 4. 5. 6. 7. 8. 9. 7. 5. 5. 5. 4. 2};
    yourself.
  self assert: (d occurrencesOf: 0) equals: 0.
  self assert: (d occurrencesOf: 1) equals: 1.
  self assert: (d occurrencesOf: 2) equals: 2.
  self assert: (d occurrencesOf: 3) equals: 1.
  self assert: (d occurrencesOf: 4) equals: 2.
  self assert: (d occurrencesOf: 5) equals: 4.
  self assert: (d occurrencesOf: 6) equals: 1.
  self assert: (d occurrencesOf: 7) equals: 2.
  self assert: (d occurrencesOf: 8) equals: 1.
  self assert: (d occurrencesOf: 9) equals: 1

! testDo
  | count total arr |
  total := count := 0.
  arr := Array new: 5.
  (Dictionary new addAll: {1. 3. 5. 3. 9}; yourself) do: [:each |
    total := total + each.
    count := count + 1.
    arr at: count put: each * 2].

  self assert: count equals: 5.
  self assert: total equals: 21.
  self assert: arr equals: {2. 6. 10. 6. 18}

! testSelect
  | d |
  d := (Dictionary new addAll: {1. 2. 3. 4. 3. 2}; yourself)
    select: [:each | each even].
  self assert: d class equals: Dictionary.
  self assert: d
    equals: (Dictionary new
      at: 2 put: 2;
      at: 4 put: 4;
      at: 6 put: 2;
      yourself)

! testReject
  | d |
  d := (Dictionary new
    addAll: {1. 2. 3. 4. 2. 5. 4. 1};
    yourself) reject: [:each | each even].
  self assert: d class equals: Dictionary.
  self assert: d equals: (Dictionary new
    at: 1 put: 1;
    at: 3 put: 3;
    at: 6 put: 5;
    at: 8 put: 1;
    yourself)

! testCollect
  self assert: (Dictionary new
      addAll: {1. 2. 3. 4. 5. 3. 5. 6};
      collect: [:each | each * 2])
    equals: (Dictionary new addAll: {2. 4. 6. 8. 10. 6. 10. 12}; yourself)

! testDetectIfNone
  | d matches |
  d := Dictionary new addAll: {1. 1. 3. 5. 6. 5. 7}; yourself.
  matches := d values select: [:x | x > 4].
  self assert: (matches includes: (d detect: [:x | x > 4] ifNone: [0])).
  self assert: (d detect: [:x | x > 9] ifNone: [0]) equals: 0.

! testDetect
  | d matches |
  d := Dictionary new addAll: {1. 1. 3. 5. 6. 5. 7}; yourself.
  matches := d values select: [:x | x > 4].
  self assert: (matches includes: (d detect: [:x | x > 4])).
  self should: [d detect: [:x | x > 9]] raise: NotFound.

! testInjectInto
  self assert: (Dictionary new
      addAll: {1. 3. 5. 3. 9};
      inject: 100 into: [:sum :x | sum - x])
    equals: 79

! testWiths
  self assert: (Dictionary
      with: (Association key: 1 value: 7))
    equals: (Dictionary new addAll: {7}; yourself).
  self assert: (Dictionary
      with: (Association key: 1 value: 7)
      with: (Association key: 2 value: 8))
    equals: (Dictionary new addAll: {7. 8}; yourself).
  self assert: (Dictionary
      with: (Association key: 1 value: 7)
      with: (Association key: 2 value: 8)
      with: (Association key: 3 value: 9))
    equals: (Dictionary new addAll: {7. 8. 9}; yourself).
  self assert: (Dictionary
      with: (Association key: 1 value: 7)
      with: (Association key: 2 value: 8)
      with: (Association key: 3 value: 9)
      with: (Association key: 4 value: 10))
    equals: (Dictionary new addAll: {7. 8. 9. 10}; yourself).
!!


TestCase subclass: #CollectionsGeneralLinkedList.

Link subclass: #LinkNode instanceVariableNames: 'value'.

! LinkNode class
! for: aValue
  ^ LinkNode new value: aValue; yourself
!!

! LinkNode
! value
  ^ value
! value: aValue
  value := aValue
! = aLinkNode
  self class == aLinkNode class ifFalse: [^false].
  ^ value = aLinkNode value

! printOn: aStream
  aStream << 'LinkNode(' << value << ')'
!!

! CollectionsGeneralLinkedList

! listOf: aCollection
  | lst |
  lst := LinkedList new.
  aCollection do: [:each | lst add: (LinkNode for: each)].
  ^ lst

! linkify: aCollection
  ^ aCollection collect: [:each | LinkNode for: each]

! testAdd
  | lst |
  lst := LinkedList new.
  1 to: 20 do: [:ix | lst add: (LinkNode for: ix)].

  self assert: lst size equals: 20.
  self assert: (lst at: 1)  equals: (LinkNode for: 1).
  self assert: (lst at: 6)  equals: (LinkNode for: 6).
  self assert: (lst at: 2)  equals: (LinkNode for: 2).
  self assert: (lst at: 20) equals: (LinkNode for: 20)
! testAddAll
  self assert: (LinkedList new addAll: (self linkify: {7. 8. 9}); yourself)
    equals: (self listOf: {7. 8. 9}).

! testIncludes
  | lst |
  lst := self listOf: {7. 8. 9}.
  self assert: (lst includes: (LinkNode for: 7)).
  self assert: (lst includes: (LinkNode for: 8)).
  self assert: (lst includes: (LinkNode for: 9)).
  self deny: (lst includes: (LinkNode for: nil)).
  self deny: (lst includes: (LinkNode for: 12))

! testIsEmpty
  self assert: LinkedList new isEmpty.
  self deny: (self listOf: {7. 8. 9}) isEmpty

! testOccurrencesOf
  | lst |
  lst := self listOf: {1. 2. 3. 4. 5. 6. 7. 8. 9. 7. 5. 5. 5. 4. 2}.
  self assert: (lst occurrencesOf: (LinkNode for: 0)) equals: 0.
  self assert: (lst occurrencesOf: (LinkNode for: 1)) equals: 1.
  self assert: (lst occurrencesOf: (LinkNode for: 2)) equals: 2.
  self assert: (lst occurrencesOf: (LinkNode for: 3)) equals: 1.
  self assert: (lst occurrencesOf: (LinkNode for: 4)) equals: 2.
  self assert: (lst occurrencesOf: (LinkNode for: 5)) equals: 4.
  self assert: (lst occurrencesOf: (LinkNode for: 6)) equals: 1.
  self assert: (lst occurrencesOf: (LinkNode for: 7)) equals: 2.
  self assert: (lst occurrencesOf: (LinkNode for: 8)) equals: 1.
  self assert: (lst occurrencesOf: (LinkNode for: 9)) equals: 1

! testDo
  | count total arr |
  total := count := 0.
  arr := Array new: 5.
  (self listOf: {1. 3. 5. 7. 9}) do: [:each |
    total := total + each value.
    count := count + 1.
    arr at: count put: each value * 2].

  self assert: count equals: 5.
  self assert: total equals: 25.
  self assert: arr equals: {2. 6. 10. 14. 18}

! testSelect
  | lst |
  lst := (self listOf: {1. 2. 3. 4. 5. 6}) select: [:each | each value even].
  self assert: lst class equals: LinkedList.
  self assert: lst equals: (self listOf: {2. 4. 6})

! testReject
  | lst |
  lst := (self listOf: {1. 2. 3. 4. 5. 6}) reject: [:each | each value even].
  self assert: lst class equals: LinkedList.
  self assert: lst equals: (self listOf: {1. 3. 5})

! testCollect
  self assert: ((self listOf: {1. 2. 3. 4. 5})
      collect: [:each | LinkNode for: each value * 2])
    equals: (self listOf: {2. 4. 6. 8. 10})

! testDetectIfNone
  self assert: ((self listOf: {1. 3. 5. 7})
      detect: [:x | x value > 4] ifNone: [0])
    equals: (LinkNode for: 5).
  self assert: ((self listOf: {1. 3. 5. 7})
      detect: [:x | x value > 9] ifNone: [0])
    equals: 0.

! testDetect
  self assert: ((self listOf: {1. 3. 5. 7}) detect: [:x | x value > 4])
    equals: (LinkNode for: 5).
  self should: [(self listOf: {1. 3. 5. 7}) detect: [:x | x value > 9]]
    raise: NotFound.

! testInjectInto
  self assert: ((self listOf: {1. 3. 5. 7. 9})
      inject: 100 into: [:sum :x | sum - x value])
    equals: 75

! testNew
  | lst |
  lst := LinkedList new.
  self assert: lst asArray equals: {}.
  self assert: lst size equals: 0.
  self assert: lst isEmpty.
  self assert: lst capacity = 0.

! testWiths
  self assert: (LinkedList with: (LinkNode for: 7))
    equals: (self listOf: {7}).
  self assert: (LinkedList with: (LinkNode for: 7) with: (LinkNode for: 8))
    equals: (self listOf: {7. 8}).
  self assert: (LinkedList with: (LinkNode for: 7) with: (LinkNode for: 8)
      with: (LinkNode for: 9))
    equals: (self listOf: {7. 8. 9}).
  self assert: (LinkedList with: (LinkNode for: 7) with: (LinkNode for: 8)
      with: (LinkNode for: 9) with: (LinkNode for: 10))
    equals: (self listOf: {7. 8. 9. 10})
!!


TestCase subclass: #CollectionsGeneralInterval.

! CollectionsGeneralInterval
! testBannedThings
  self should: [(Interval from: 1 to: 8) add: 12]
    raise: ShouldNotImplement.
  self should: [(Interval from: 1 to: 8 by: 2) add: 12]
    raise: ShouldNotImplement.
  self should: [(Interval from: 1 to: 8 by: 2) at: 12 put: 9]
    raise: ShouldNotImplement.

! testAddAll
  self should: [(Interval from: 1 to: 8 by: 2) addAll: {6. 3. 2}]
    raise: ShouldNotImplement.

! testRemove
  self should: [(Interval from: 1 to: 8) remove: 6] raise: ShouldNotImplement.

! testRemoveAll
  self should: [(Interval from: 1 to: 8) removeAll: {6. 7. 8}]
    raise: ShouldNotImplement.

! testIncludes
  | iv |
  iv := Interval from: 1 to: 12 by: 2.
  self assert: (iv includes: 5).
  self assert: (iv includes: 7).
  self assert: (iv includes: 9).
  self deny: (iv includes: nil).
  self deny: (iv includes: 6).
  self deny: (iv includes: 12)

! testIsEmpty
  self assert: (Interval from: 1 to: 0) isEmpty.
  self deny: {7. 8. 9} asOrderedCollection isEmpty

! testOccurrencesOf
  | iv |
  iv := Interval from: 20 to: 0 by: -2.
  self assert: (iv occurrencesOf:  0) equals: 1.
  self assert: (iv occurrencesOf:  2) equals: 1.
  self assert: (iv occurrencesOf:  4) equals: 1.
  self assert: (iv occurrencesOf:  6) equals: 1.
  self assert: (iv occurrencesOf:  8) equals: 1.
  self assert: (iv occurrencesOf: 10) equals: 1.
  self assert: (iv occurrencesOf: 12) equals: 1.
  self assert: (iv occurrencesOf: 14) equals: 1.
  self assert: (iv occurrencesOf: 16) equals: 1.
  self assert: (iv occurrencesOf: 18) equals: 1.
  self assert: (iv occurrencesOf: 13) equals: 0.
  self assert: (iv occurrencesOf: 15) equals: 0.
  self assert: (iv occurrencesOf: 17) equals: 0.
  self assert: (iv occurrencesOf: 19) equals: 0

! testDo
  | count total arr |
  total := count := 0.
  arr := Array new: 5.
  (Interval from: 1 to: 9 by: 2) do: [:each |
    total := total + each.
    count := count + 1.
    arr at: count put: each * 2].

  self assert: count equals: 5.
  self assert: total equals: 25.
  self assert: arr equals: {2. 6. 10. 14. 18}

! testSelect
  | iv |
  iv := (Interval from: 1 to: 6) select: [:each | each even].
  self assert: iv class equals: OrderedCollection.
  self assert: iv equals: {2. 4. 6} asOrderedCollection

! testReject
  | iv |
  iv := (Interval from: 1 to: 6) reject: [:each | each even].
  self assert: iv class equals: OrderedCollection.
  self assert: iv equals: {1. 3. 5} asOrderedCollection

! testCollect
  self assert: ((Interval from: 1 to: 5) collect: [:each | each * 2])
    equals: {2. 4. 6. 8. 10} asOrderedCollection

! testDetectIfNone
  self assert: ((Interval from: 1 to: 7 by: 2) detect: [:x | x > 4] ifNone: [0])
    equals: 5.
  self assert: ((Interval from: 1 to: 7 by: 2) detect: [:x | x > 9] ifNone: [0])
    equals: 0.

! testDetect
  self assert: ((Interval from: 1 to: 7 by: 2) detect: [:x | x > 4])
    equals: 5.
  self should: [(Interval from: 1 to: 7 by: 2) detect: [:x | x > 9]]
    raise: NotFound.

! testInjectInto
  self assert: ((Interval from: 1 to: 9 by: 2)
      inject: 100 into: [:sum :x | sum - x])
    equals: 75

! testSizeAndCapacity
  | iv |
  iv := Interval from: 1 to: 9 by: 2.
  self assert: iv size equals: 5.
  self assert: iv isEmpty not.
  self assert: iv capacity equals: 5.

  iv := Interval from: 3 to: 2. "Should be empty."
  self assert: iv size equals: 0.
  self assert: iv isEmpty.
  self assert: iv capacity equals: 0.

  iv := Interval from: 3 to: 3. "Should be just 3."
  self assert: iv size equals: 1.
  self assert: iv isEmpty not.
  self assert: iv capacity equals: 1.

! testWiths
  self should: [Interval with: 7]
    raise: ShouldNotImplement.
  self should: [Interval with: 7 with: 8]
    raise: ShouldNotImplement.
  self should: [Interval with: 7 with: 8 with: 9]
    raise: ShouldNotImplement.
  self should: [Interval with: 7 with: 8 with: 9 with: 10]
    raise: ShouldNotImplement.
!!


