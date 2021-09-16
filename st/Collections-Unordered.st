Collection subclass: #HashedCollection
  instanceVariableNames: 'map'.

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

Object subclass: #Association
  instanceVariableNames: 'key value'.

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


! HashedCollection
! initialize
  super initialize.
  <builtin: 'new object'>
! at: aStringKey
  "Answers the raw value at the given key, or nil."
  <builtin: 'dict_at:'>
! at: aStringKey put: aValue
  "Sets an association at the given key, replacing any previous association."
  <builtin: 'dict_at:put:'>

! values
  "Returns a Smalltalk Array of the values."
  <builtin: 'dict_values'>
! keys
  "Returns a Smalltalk Array of the values."
  <builtin: 'dict_keys'>
! size
  self keys size

! removeKey: aStringKey
  ^ self removeKey: aStringKey ifAbsent: [ self errorKeyNotFound: aStringKey ]
! removeKey: aStringKey ifAbsent: aBlock
  | value |
  value := self at: aStringKey.
  value ifNil: [ ^ aBlock value ]. "Nothing to remove."
  self rawRemoveKey: aStringKey.
  ^ value
! rawRemoveKey: aStringKey
  <builtin: 'dict_removeKey:'>


! associationsDo: aBlock
  self keys do: [ :each |
    aBlock value: (Association key: each value: (self at: each)) ]
! assocations
  "Returns a Smalltalk Array of Associations."
  | col |
  col := Array new.
  self associationsDo: [ :each | col add: each ].
  ^ col

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
!!


"A Dictionary holds arbitrary Smalltalk keys and values, by the keys' #hash
values. The Javascript map has the hashes of the keys pointing to the Smalltalk
*assocations* as its values.
It uses the superclass methods at:, at:put:, keys and values to implement the
basic layer. Dictionary is mostly about juggling arbitrary keys into hash
strings, and arbitrary values into Associations."
HashedCollection subclass: #Dictionary
  instanceVariableNames: ''.

! Dictionary
! add: anAssoc
  super at: anAssoc key hash put: anAssoc

! addAll: aKeyedCollection
  aKeyedCollection == self ifFalse: [
    aKeyedCollection keysAndValuesDo: [ :key :value |
      self at: key put: value]].
  ^ aKeyedCollection

! associationAt: aKey
  ^ self associationAt: aKey ifAbsent: [self errorKeyNotFound: aKey]

! associationAt: aKey ifAbsent: exceptionBlock
  | assoc |
  assoc := super at: aKey hash.
  assoc ifNotNil: [ ^ assoc ].
  ^ exceptionBlock value

! associationsDo: aBlock
  "super values is the associations stored as the values in the
  HashedCollection"
  super values do: aBlock

! at: aKey
  ^ self at: aKey ifAbsent: [ self errorKeyNotFound: aKey ]
! at: aKey ifAbsent: aBlock
  | assoc |
  assoc := super at: aKey hash.
  assoc ifNil: aBlock ifNotNil: [ ^ assoc value ]
! at: aKey ifPresent: aBlock
  ^ self at: aKey ifPresent: aBlock ifAbsent: [ ^ nil ]
! at: aKey ifPresent: presentBlock ifAbsent: absentBlock
  | assoc |
  assoc := super at: aKey hash.
  assoc ifNil: [ ^ absentBlock value ].
  ^ presentBlock cull: assoc value
! at: aKey ifAbsentPut: aValue
  (super at: aKey hash) ifNil: [ self at: aKey put: aValue ]
! at: aKey ifPresent: aBlock ifAbsentPut: aValue
  self at: aKey ifPresent: aBlock ifAbsent: [ self at: aKey put: aValue ]
! at: aKey put: aValue
  super at: aKey hash put: (Association key: aKey value: aValue)

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

! errorKeyNotFound: aKey
  (KeyNotFound new key: aKey) signal
! errorValueNotFound: aValue
  (ValueNotFound new value: aValue) signal

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
  ^ (super at: aKey hash) isNotNil

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

! removeKey: aKey
  ^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound: aKey ]
! removeKey: aKey ifAbsent: aBlock
  | assoc |
  assoc := super removeKey: aKey hash ifAbsent: [ ^ aBlock value ].
  ^ assoc value

! select: aBlock
  | col |
  col := self copyEmpty.
  self associationsDo: [ :each |
    (aBlock value: each value) ifTrue: [ col add: each copy ]].
  ^ col

! values
  "super values are the Associations, extract their values."
  ^ super values collect: [ :each | each value ]
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

