Collection subclass: #HashedCollection
  instanceVariableNames: 'map'.

Error subclass: #KeyNotFound
  instanceVariableNames: 'key'.
Error subclass: #ValueNotFound
  instanceVariableNames: 'value'.

KeyNotFound >> ! key: aKey
  key := aKey
!.
ValueNotFound >> ! value: aValue
  value := aValue
!.

Object subclass: #Association
  instanceVariableNames: 'key value'.

Association >> ! value
  ^ value
!.
Association >> ! key
  ^ key
!.
Association >> ! value: aValue
  value := aValue
!.
Association >> ! key: aKey
  key := aKey
!.

Association class >> ! key: aKey value: aValue
  ^ self new key: aKey; value: aValue
!.

Association >> ! hash
  ^ key hash
!.
Association >> ! = anAssoc
  ^ key = anAssoc key
!.
Association >> ! < anAssoc
  ^ key < anAssoc key
!.



HashedCollection >> ! initialize
  super initialize.
  map := <builtin: 'new object'>
!.

HashedCollection >> ! at: aStringKey
  "Answers the raw value at the given key, or nil."
  ^ <builtin: 'dict_at:'>
!.
HashedCollection >> ! at: aStringKey put: aValue
  "Sets an association at the given key, replacing any previous association."
  <builtin: 'dict_at:put:'>
!.

HashedCollection >> ! values
  "Returns a Smalltalk Array of the values."
  ^ <builtin: 'dict_values'>
!.
HashedCollection >> ! keys
  "Returns a Smalltalk Array of the values."
  ^ <builtin: 'dict_keys'>
!.
HashedCollection >> ! size
  self keys size
!.

HashedCollection >> ! removeKey: aStringKey
  ^ self removeKey: aStringKey ifAbsent: [ self errorKeyNotFound: aStringKey ]
!.

HashedCollection >> ! removeKey: aStringKey ifAbsent: aBlock
  | value |
  value := self at: aStringKey.
  value ifNil: [ ^ aBlock value ]. "Nothing to remove."
  self rawRemoveKey: aStringKey.
  ^ value
!.

HashedCollection >> ! rawRemoveKey: aStringKey
  <builtin: 'dict_removeKey:'>
!.


HashedCollection >> ! associationsDo: aBlock
  self keys do: [ :each |
    aBlock value: (Association key: each value: (self at: each)) ]
!.
HashedCollection >> ! assocations
  "Returns a Smalltalk Array of Associations."
  | col |
  col := Array new.
  self associationsDo: [ :each | col add: each ].
  ^ col
!.

HashedCollection >> ! = aDictionary
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
!.

HashedCollection >> ! add: anAssoc
  self at: anAssoc key put: anAssoc value
!.


"A Dictionary holds arbitrary Smalltalk keys and values, by the keys' #hash
values. The Javascript map has the hashes of the keys pointing to the Smalltalk
*assocations* as its values.
It uses the superclass methods at:, at:put:, keys and values to implement the
basic layer. Dictionary is mostly about juggling arbitrary keys into hash
strings, and arbitrary values into Associations."
HashedCollection subclass: #Dictionary
  instanceVariableNames: ''.

Dictionary >> ! add: anAssoc
  super at: anAssoc key hash put: anAssoc
!.

Dictionary >> ! addAll: aKeyedCollection
  aKeyedCollection == self ifFalse: [
    aKeyedCollection keysAndValuesDo: [ :key :value |
      self at: key put: value]].
  ^ aKeyedCollection
!.

Dictionary >> ! associationAt: aKey
  ^ self associationAt: aKey ifAbsent: [self errorKeyNotFound: aKey]
!.

Dictionary >> ! associationAt: aKey ifAbsent: exceptionBlock
  | assoc |
  assoc := super at: aKey hash.
  assoc ifNotNil: [ ^ assoc ].
  ^ exceptionBlock value
!.


Dictionary >> ! associationsDo: aBlock
  "super values is the associations stored as the values in the
  HashedCollection"
  super values do: aBlock
!.

Dictionary >> ! at: aKey
  ^ self at: aKey ifAbsent: [ self errorKeyNotFound: aKey ]
!.

Dictionary >> ! at: aKey ifAbsent: aBlock
  | assoc |
  assoc := super at: aKey hash.
  assoc ifNil: aBlock ifNotNil: [ ^ assoc value ]
!.

Dictionary >> ! at: aKey ifPresent: aBlock
  ^ self at: aKey ifPresent: aBlock ifAbsent: [ ^ nil ]
!.

Dictionary >> ! at: aKey ifPresent: presentBlock ifAbsent: absentBlock
  | assoc |
  assoc := super at: aKey hash.
  assoc ifNil: [ ^ absentBlock value ].
  ^ presentBlock cull: assoc value
!.


Dictionary >> ! at: aKey ifAbsentPut: aValue
  (super at: aKey hash) ifNil: [ self at: aKey put: aValue ]
!.

Dictionary >> ! at: aKey ifPresent: aBlock ifAbsentPut: aValue
  self at: aKey ifPresent: aBlock ifAbsent: [ self at: aKey put: aValue ]
!.

Dictionary >> ! at: aKey put: aValue
  super at: aKey hash put: (Association key: aKey value: aValue)
!.

Dictionary >> ! collect: aBlock
  "Evaluate aBlock with each value as the argument. Collect the results into a
  new collection with the same keys. Answer the new collection."
  | col |
  col := self species new.
  self assocationsDo: [ :each |
    col at: each key put: (aBlock value: each value) ].
  ^ col
!.

Dictionary >> ! do: aBlock
  ^ self valuesDo: aBlock
!.

Dictionary >> ! errorKeyNotFound: aKey
  KeyNotFound new key: aKey; signal
!.
Dictionary >> ! errorValueNotFound: aValue
  ValueNotFound new value: aValue; signal
!.

Dictionary >> ! fillFrom: aCollection with: aBlock
  aCollection keysAndValuesDo: [ :key :value |
    self at: key put: (aBlock value: value) ]
!.

Dictionary >> ! includes: anObject
  self do: [ :each | anObject = each ifTrue: [ ^ true ]].
  ^ false
!.

Dictionary >> ! includesAssociation: anAssoc
  ^ (self associationAt: anAssoc key
      ifAbsent: [ ^ false ]) value = anAssoc value
!.

Dictionary >> ! includesKey: aKey
  ^ (super at: aKey hash) isNotNil
!.

Dictionary >> ! isDictionary
  ^ true
!.
Object >> ! isDictionary
  ^ false
!.

Dictionary >> ! keyAtValue: aValue
  ^ self keyAtValue: aValue ifAbsent: [ self errorValueNotFound: aValue ]
!.

Dictionary >> ! keyAtValue: aValue ifAbsent: aBlock
  self associationsDo: [ :assoc | assoc value = aValue ifTrue: [ ^ assoc key ]].
  ^ aBlock value
!.

Dictionary >> ! keysAndValuesDo: aBlock
  ^ self assocationsDo: [ :assoc |
    aBlock value: assoc key value: assoc value]
!.

Dictionary >> ! keysDo: aBlock
  "super keys is the set of hashes; we want to grab the Associations stored as
  values, and grab their keys."
  ^ super values do: [ :assoc | aBlock value: assoc key ]
!.

Dictionary >> ! remove: anObject
  self shouldNotImplement
!.
Dictionary >> ! remove: anObject ifAbsent: aBlock
  self shouldNotImplement
!.

Dictionary >> ! removeKey: aKey
  ^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound: aKey ]
!.
Dictionary >> ! removeKey: aKey ifAbsent: aBlock
  | assoc |
  assoc := super removeKey: aKey hash ifAbsent: [ ^ aBlock value ].
  ^ assoc value
!.

Dictionary >> ! select: aBlock
  | col |
  col := self copyEmpty.
  self associationsDo: [ :each |
    (aBlock value: each value) ifTrue: [ col add: each copy ]].
  ^ col
!.

Dictionary >> ! values
  "super values are the Associations, extract their values."
  ^ super values collect: [ :each | each value ]
!.
Dictionary >> ! valuesDo: aBlock
  ^ self associationsDo: [ :each | aBlock value: each value ]
!.

