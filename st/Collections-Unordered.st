"Collection subclass: #HashedCollection
  instanceVariableNames: 'array tally'.
  - This is defined in the VM."

Error subclass: #KeyNotFound
  instanceVariableNames: 'key'.
Error subclass: #ValueNotFound
  instanceVariableNames: 'value'.

! KeyNotFound ! key: aKey
  key := aKey
!!
! ValueNotFound ! value: aValue
  value := aValue
!!

"Object subclass: #Association
  instanceVariableNames: 'key value'."

! Association
! value
  ^ value
! key
  ^ key
! value: aValue
  value := aValue
! key: aKey
  key := aKey

! hash
  ^ key hash
! = anAssoc
  ^ key = anAssoc key
! < anAssoc
  ^ key < anAssoc key
!!

! Association class
! key: aKey value: aValue
  "^ self new key: aKey; value: aValue"
  | assoc |
  assoc := self new key: aKey.
  ^ assoc value: aValue
!!

! HashedCollection class
! new
  ^ self new: 8
!!

! HashedCollection
! needsToGrow
  "No floats, but we want to check for 2/3rds full.
  tally/capacity > 2/3
  3 * tally > 2 * capacity"
  ^ (2 * self capacity) < (3 * tally)

! initialize: size
  super initialize: size.
  array := Array new: size.
  tally := 0

! hashMask
  ^ array size - 1 "Powers of 2, so -1 makes a mask."

! findElementOrNil: aKey
  | index row |
  index := aKey hash bitAnd: self hashMask.
  row := array at: index.
  [row isNil] whileFalse: [
    (self matchKey: aKey with: row key) ifTrue: [^ index].
    index := index + 1 bitAnd: self hashMask.
    row := array at: index].
  ^ index

! at: aKey ifAbsent: aBlockOrValue
  "The array is treated as a hash table with linear probing."
  | asc |
  asc := array at: (self findElementOrNil: aKey).
  asc ifNil: [^ asc value].
  ^aBlockOrValue value

! at: aKey
  ^ self at: aKey ifAbsent: [self errorKeyNotFound: aKey]

! at: aKey put: aValue
  "Sets an association at the given key, replacing any previous association."
  | index row |
  index := self findElementOrNil: aKey.
  row := array at: index.
  row ifNil: [ row value: aValue. ^self ].
  row at: index put: (Association key: aKey value: aValue).
  tally := tally + 1.
  self growIfNeeded

! associationAt: aKey
  ^ self assocationAt: aKey ifAbsent: [self errorKeyNotFound: aKey]

! associationAt: aKey ifAbsent: aBlockOrValue
  | asc |
  asc := array at: (self findElementOrNil: aKey).
  asc ifNil: [^ asc].
  ^aBlockOrValue value

! errorKeyNotFound: aKey
  (KeyNotFound new key: aKey) signal
! errorValueNotFound: aValue
  (ValueNotFound new value: aValue) signal

! growIfNeeded
  | oldArray |
  self needsToGrow ifFalse: [^self].
  oldArray := array.
  array := Array new: self size * 2.
  tally := 0.
  array do: [:each | each isNotNil: [self add: each]]

! values
  "Returns a Smalltalk Array of the values."
  | vals index |
  vals := Array new: tally.
  index := 1.
  self associationsDo: [:asc |
    vals at: index put: asc value. index := index + 1].
  ^ vals

! keys
  "Returns a Smalltalk Array of the values."
  | vals index |
  vals := Array new: tally.
  index := 1.
  self associationsDo: [:asc | vals at: index put: asc key. index := index + 1].
  ^ vals

! associations
  | vals index |
  vals := Array new: tally.
  index := 1.
  self associationsDo: [:asc | vals at: index put: asc. index := index + 1].
  ^ vals

! associationsDo: aBlock
  array do: [:each | each ifNotNil: [aBlock value: each]]

! size
  ^ tally
! capacity
  ^ array size

! removeKey: aKey
  ^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound: aKey ]
! removeKey: aKey ifAbsent: aBlock
  "Removes a key, or calls the block if it's not there.
  Two cases: Found in the array, and not found."
  | index row |
  index := self findElementOrNil: aKey.
  row := array at: index.
  row ifNil: [^ aBlock value ].
  tally := tally - 1.
  array at: index put: nil.
  self fixCollisionsFrom: index.
  ^ row value

! fixCollisionsFrom: start
  "The element at start has been removed and replaced by nil. We need to adjust
  any later elements that spilled down due to collisions."
  | element index |
  index := start.
  [ (element := array at: (index := index bitAnd: self hashMask)) isNil ]
    whileFalse: [
      | newIndex |
      (newIndex := self findElementOrNil: element key) = index
        ifTrue: [array swap: index with: newIndex] ]


! species
  ^ self class

! = aDictionary
  "Two HashedCollections are equal iff
  (a) they are the same kind of thing.
  (b) they have the same set of keys.
  (c) for each (common) key, they have the same value."
  self == aDictionary ifTrue: [ ^ true ].
  self species == aDictionary species ifFalse: [ ^ false ].
  self size = aDictionary size ifFalse: [ ^ false ].
  self associationsDo: [ :assoc |
    (aDictionary at: assoc key ifAbsent: [ ^ false ]) = assoc value
      ifFalse: [ ^ false ]].
  ^ true

! add: anAssoc
  self at: anAssoc key put: anAssoc value

! addAll: aKeyedCollection
  aKeyedCollection == self ifFalse: [
    aKeyedCollection keysAndValuesDo: [ :key :value |
      self at: key put: value]].
  ^ aKeyedCollection

! at: aKey ifAbsentPut: aValue
  ^ self at: aKey ifAbsent: [self at: aKey put: aValue]

! collect: aBlock
  "Evaluate aBlock with each value as the argument. Collect the results into a
  new collection with the same keys. Answer the new collection."
  | col |
  col := self species new.
  self assocationsDo: [ :each |
    col at: each key put: (aBlock value: each value) ].
  ^ col

! do: aBlock
  ^ self valuesDo: aBlock

! fillFrom: aCollection with: aBlock
  aCollection keysAndValuesDo: [ :key :value |
    self at: key put: (aBlock value: value) ]

! includes: anObject
  self do: [ :each | anObject = each ifTrue: [ ^ true ]].
  ^ false

! includesAssociation: anAssoc
  ^ (self associationAt: anAssoc key
      ifAbsent: [ ^ false ]) value = anAssoc value

! includesKey: aKey
  ^ (super at: aKey) isNotNil

! isDictionary
  ^ true

! keyAtValue: aValue
  ^ self keyAtValue: aValue ifAbsent: [ self errorValueNotFound: aValue ]
! keyAtValue: aValue ifAbsent: aBlock
  self associationsDo: [ :assoc | assoc value = aValue ifTrue: [ ^ assoc key ]].
  ^ aBlock value
! keysAndValuesDo: aBlock
  ^ self assocationsDo: [ :assoc |
    aBlock value: assoc key value: assoc value]

! keysDo: aBlock
  "super keys is the set of hashes; we want to grab the Associations stored as
  values, and grab their keys."
  ^ super values do: [ :assoc | aBlock value: assoc key ]

! remove: anObject
  self shouldNotImplement
! remove: anObject ifAbsent: aBlock
  self shouldNotImplement

! select: aBlock
  | col |
  col := self copyEmpty.
  self associationsDo: [ :each |
    (aBlock value: each value) ifTrue: [ col add: each copy ]].
  ^ col

! valuesDo: aBlock
  ^ self associationsDo: [ :each | aBlock value: each value ]
!!

! Object
! isDictionary
  ^ false
!!

"A Set is a HashedCollection without values, just membership.
Implemented by using elements' hashes as keys and the elements as values."
HashedCollection subclass: #Set.

! Set class
! new: aSize
  "Size is actually ignored here, JS takes care of things."
  ^ self new
!!

! Set
! = aSet
  self == aSet ifTrue: [^true]. "Avoids recursion."
  (aSet isKindOf: Set) ifFalse: [^false].
  self size = aSet size ifFalse: [^false].
  self do: [:each | (aSet includes: each) ifFalse: [^false]].
  ^ true

! add: newObject
  "Adds newObject to this Set, if it's not already present. Answers newObject."
  self at: newObject hash put: newObject
! collect: aBlock
  | newSet |
  newSet := self species new: self size.
  self values do: [:each | newSet add: (aBlock value: each)].
  ^ newSet
! difference: aCollection
  "Answer the set theoretic difference between two collections. This one is
  specialized since no intermediate Set is necessary. The result is a new set
  containing only those keys not found in aCollection."
  | set |
  set := self copy.
  aCollection do: [:each | set remove: each ifAbsent: [ "Do nothing" ]].
  ^ set

! do: aBlock
  self values do: aBlock

! includes: anObject
  ^ (self at: anObject hash) isNotNil

! intersection: aCollection
  | xsec |
  xsec := self class new.
  aCollection do: [:each | (self includes: each) ifTrue: [xsec add: each]].
  ^ xsec

! isHealthy
  self associationsDo: [:each | each key = each value hash ifFalse: [^false]].
  ^ true
!!

