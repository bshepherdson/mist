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

! printOn: aStream
  aStream print: key; nextPutAll: ' -> '; print: value
!!

! Association class
! key: aKey value: aValue
  "^ self new key: aKey; value: aValue"
  | assoc |
  assoc := self new key: aKey.
  ^ assoc value: aValue
!!


"HashedCollection is abstract; it relies on its three subclasses to
implement a few key details. HashedCollection stores Associations in a hash
table way, using linear probing.
- The table is always a power-of-2 size, enabling bit-masking over division.
- The table is allowed to get up to 2/3rds full before expanding.

- Dictionary stores pairs of arbitrary keys and values.
- Set stores arbitrary values as both the key and value sides.
- Bag stores arbitrary values as keys, with a count as the value.
"

! HashedCollection class
! new
  ^ self new: 8
! new: size
  ^ self basicNew initialize: size; yourself.
!!

! HashedCollection
! needsToGrow
  "No floats, but we want to check for 2/3rds full.
  tally/capacity > 2/3
  3 * tally > 2 * capacity"
  ^ (2 * self capacity) < (3 * tally)

! initialize: size
  super initialize: size.
  array := Array new: size nextPowerOf2.
  tally := 0

! hashMask
  ^ array size - 1 "Powers of 2, so -1 makes a mask."

! findElementOrNil: aKey
  | index row |
  index := (aKey hash bitAnd: self hashMask).
  row := array at: index + 1.
  [row isNil] whileFalse: [
    (self matchKey: aKey with: row key) ifTrue: [^ index + 1].
    index := index + 1 bitAnd: self hashMask.
    row := array at: index + 1].
  ^ index + 1

! matchKey: aKey with: anotherKey
  "Defines the matching of keys, post-hashing. This is extracted out so
  IdentityDictionary can override it to be based on ==."
  ^ aKey = anotherKey

! at: aKey ifAbsent: aBlockOrValue
  (self associationAt: aKey ifAbsent: [^aBlockOrValue value]) value

! at: aKey
  ^ self at: aKey ifAbsent: [self errorKeyNotFound: aKey]

! associationAt: aKey
  ^ self associationAt: aKey ifAbsent: [self errorKeyNotFound: aKey]

! associationAt: aKey ifAbsentPut: aBlock
  | index assoc |
  index := self findElementOrNil: aKey.
  assoc := array at: index.
  assoc ifNil: [
    tally := tally + 1.
    assoc := aBlock value.
    (assoc isKindOf: Association)
      ifFalse: [self error: (Error signal: 'need Association')].
    array at: index put: assoc.
    self growIfNeeded].
  ^ assoc

! associationAt: aKey ifAbsent: aBlockOrValue
  | asc |
  asc := array at: (self findElementOrNil: aKey).
  asc ifNotNil: [^ asc].
  ^aBlockOrValue value

! addAssociation: anAssoc
  | assoc |
  assoc := self associationAt: anAssoc key ifAbsentPut: [anAssoc].
  assoc value: anAssoc value

! at: aKey put: aValue
  | assoc |
  assoc := self associationAt: aKey ifAbsentPut: [Association key: aKey value: aValue].
  assoc value: aValue

! errorKeyNotFound: aKey
  (KeyNotFound new key: aKey) signal
! errorValueNotFound: aValue
  (ValueNotFound new value: aValue) signal

! growIfNeeded
  | oldArray |
  self needsToGrow ifFalse: [^self].
  oldArray := array.
  array := Array new: array size * 2.
  tally := 0.
  oldArray do: [:each | each ifNotNil: [self addAssociation: each]]

! associationsDo: aBlock
  array do: [:each | each ifNotNil: [aBlock value: each]]

! size
  ^ tally
! capacity
  ^ array size

! remove: something
  "This means different things in different subclasses."
  self subclassResponsibility

! removeKey: aKey ifAbsent: aBlock
  | assoc |
  assoc := self removeAssociationAt: aKey ifAbsent: aBlock.
  ^ assoc value

! removeKey: aKey
  ^ self removeKey: aKey ifAbsent: [self errorKeyNotFound: aKey]

! removeAssociationAt: aKey ifAbsent: aBlock
  "Removes a key, or calls the block if it's not there.
  Two cases: Found in the array, and not found."
  | index row |
  index := self findElementOrNil: aKey.
  row := array at: index.
  row ifNil: [^ aBlock value].
  tally := tally - 1.
  array at: index put: nil.
  self fixCollisionsFrom: index.
  ^ row


! fixCollisionsFrom: start
  "The element at start has been removed and replaced by nil. We need to adjust
  any later elements that spilled down due to collisions."
  | element index |
  index := start - 1. "0-based index"
  [ index := index + 1 bitAnd: self hashMask.
    (element := array at: index + 1) isNil ]
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

! add: something
  "This means different things in different subclasses. They should call
  addAssociation: with the Association they want to store."
  self subclassResponsibility

! addAll: aKeyedCollection
  aKeyedCollection == self ifFalse: [
    aKeyedCollection keysAndValuesDo: [ :key :value |
      self addAssociation: (Association key: key value: value)]].
  ^ aKeyedCollection

! do: aBlock
  self subclassResponsibility

! fillFrom: aCollection with: aBlock
  aCollection keysAndValuesDo: [ :key :value |
    self at: key put: (aBlock value: value) ]

! includes: anObject
  self valuesDo: [ :each | anObject = each ifTrue: [^ true]].
  ^ false

! includesAssociation: anAssoc
  ^ (self associationAt: anAssoc key
      ifAbsent: [^ false]) value = anAssoc value

! includesKey: aKey
  self at: aKey ifAbsent: [^false].
  ^ true

! valuesDo: aBlock
  self do: aBlock
!!

! Object
! isDictionary
  ^ false
! isSet
  ^ false
! isBag
  ^ false
!!


"Abstract methods on HashedCollection: add: at:ifAbsent: at:put: remove:"

"A Dictionary stores arbitrary keys and values."
"HashedCollection subclass: #Dictionary."

! Dictionary
! isDictionary
  ^ true
! add: anAssociation
  self addAssociation: anAssociation.
  ^ anAssociation
! at: aKey ifAbsent: aBlock
  ^ (self associationAt: aKey ifAbsent: [^ aBlock value]) value
! remove: aValue
  self shouldNotImplement

! keys
  "Returns a Set of the keys."
  | vals |
  vals := Set new: tally.
  self associationsDo: [:asc |
    vals add: asc key].
  ^ vals

! values
  "Returns a Set of the values."
  | vals |
  vals := Set new: tally.
  self associationsDo: [:asc |
    vals add: asc value].
  ^ vals

! associations
  "Returns an Array of the associations."
  | vals index |
  vals := Array new: tally.
  index := 0.
  self associationsDo: [:asc |
    vals at: (index := index + 1) put: asc].
  ^ vals

! keyAtValue: aValue
  ^ self keyAtValue: aValue ifAbsent: [ self errorValueNotFound: aValue ]
! keyAtValue: aValue ifAbsent: aBlock
  self associationsDo: [ :assoc | assoc value = aValue ifTrue: [^ assoc key]].
  ^ aBlock value

! keysAndValuesDo: aBlock
  ^ self associationsDo: [ :assoc |
    aBlock value: assoc key value: assoc value]

! keysDo: aBlock
  "super keys is the set of hashes; we want to grab the Associations stored as
  values, and grab their keys."
  ^ self associationsDo: [:assoc | aBlock value: assoc key]

! valuesDo: aBlock
  ^ self associationsDo: [:each | aBlock value: each value]

! do: aBlock
  ^ self valuesDo: aBlock

! select: aBlock
  | d |
  d := self copyEmpty.
  self associationsDo: [:each |
    (aBlock value: each value) ifTrue: [d add: each copy]].
  ^ d

! collect: aBlock
  | d |
  d := self copyEmpty.
  self associationsDo: [:each |
    d at: each key put: (aBlock value: each value)].
  ^ d
!!

"A Set is a HashedCollection without values, just membership.
Implemented by using elements' hashes as keys and the elements as values."
HashedCollection subclass: #Set.

! Set
! isSet
  ^ true

! = aSet
  self == aSet ifTrue: [^true]. "Avoids recursion."
  (aSet isKindOf: Set) ifFalse: [^false].
  self size = aSet size ifFalse: [^false].
  self do: [:each | (aSet includes: each) ifFalse: [^false]].
  ^ true

! add: newObject
  "Adds newObject to this Set, if it's not already present. Answers newObject."
  self addAssociation: (Association key: newObject value: newObject).
  ^ newObject

! addAll: aCollection
  "HashedCollection addAll: does the wrong thing."
  aCollection do: [:each | self add: each].
  ^ aCollection

! remove: aValue
  ^ super removeKey: aValue
! remove: aValue ifAbsent: aBlock
  ^ super removeKey: aValue ifAbsent: aBlock

! occurrencesOf: aValue
  ^ (self includes: aValue) ifTrue: [1] ifFalse: [0]

! difference: aCollection
  "Answer the set theoretic difference between two collections. This one is
  specialized since no intermediate Set is necessary. The result is a new set
  containing only those keys not found in aCollection."
  | set |
  set := self copy.
  aCollection do: [:each | set remove: each ifAbsent: [ "Do nothing" ]].
  ^ set

! do: aBlock
  array do: [:each |
    each ifNotNil: [aBlock value: each value]]

! includes: anObject
  self at: anObject ifAbsent: [^false].
  ^ true

! intersection: aCollection
  | xsec |
  xsec := self class new.
  aCollection do: [:each | (self includes: each) ifTrue: [xsec add: each]].
  ^ xsec

! isHealthy
  self associationsDo: [:each | each key = each value ifFalse: [^false]].
  ^ true

! putOn: aStream
  aStream << self class name.
  aStream nextPut: $(.
  self putContentsOn: aStream.
  aStream nextPut: $)
!!

HashedCollection subclass: #Bag.

! Bag
! isBag
  ^ true

! size
  | total |
  total := 0.
  self valuesDo: [:count | total := total + count].
  ^ total

! add: newObject
  "Adds newObject to this Set, if it's not already present. Answers newObject."
  ^ self add: newObject withOccurrences: 1

! add: newObject withOccurrences: aCount
  | assoc |
  assoc := self associationAt: newObject
    ifAbsentPut: [Association key: newObject value: 0].
  assoc value: assoc value + aCount

! addAll: aCollection
  (aCollection isKindOf: Bag) ifTrue: [
    aCollection associationsDo: [:asc |
      self add: asc key withOccurrences: asc value].
    ^ aCollection].

  aCollection do: [:each | self add: each].
  ^ aCollection

! at: aKey
  self shouldNotImplement
! at: aKey put: aValue
  self shouldNotImplement

! = aBag
  self == aBag ifTrue: [^true]. "Avoids recursion."
  (aBag isKindOf: Bag) ifFalse: [^false].
  self size = aBag size ifFalse: [^false].
  self associationsDo: [:each |
    (aBag occurrencesOf: each key) = each value ifFalse: [^false]].
  ^ true

! remove: aValue
  self removeKey: aValue.
  ^ aValue

! removeKey: aKey ifAbsent: aBlock
  | assoc |
  assoc := self associationAt: aKey ifAbsent: aBlock.
  assoc value = 1 ifTrue: [^ self removeAssociationAt: aKey ifAbsent: []].
  assoc value: assoc value - 1

! removeKey: aKey
  ^ self removeKey: aKey ifAbsent: [self errorKeyNotFound: aKey].

! do: aBlock
  array do: [:each |
    each ifNotNil: [
      each value timesRepeat: [aBlock value: each key]]]

! occurrencesOf: aValue
  ^ (self associationAt: aValue ifAbsent: [^0]) value

! includes: anObject
  self associationAt: anObject ifAbsent: [^false].
  ^ true
!!


! IdentityDictionary
! matchKey: aKey with: anotherKey
  ^ aKey == anotherKey
!!

