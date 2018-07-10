"Builds the basics of the collection hierarchy.
Collection
  HashedCollection      Wraps a raw Javascript map; uses raw string keys.
    Dictionary            Any Smalltalk value as the key, using #hash.
  Array                 Native array of Smalltalk values.
"

Object subclass: #Collection.

"Early declaration of the Array class, to be filled in later.
Raw Javascript arrays have their prototype modified to hold $class, turning them
into Smalltalk values."
Collection subclass: #Array.

"First, a block of the abstract methods all subclasses must provide."
Collection >> ! species
  self subclassResponsibility
!.

Collection >> ! do: aBlock
  self subclassResponsibility
!.

Collection >> ! remove: oldObject ifAbsent: exceptionBlock
  self subclassResponsibility
!.


"The errors a Collection can emit."
Error subclass: #NotFound
  instanceVariableNames: 'object'.

NotFound >> ! object: anObject
  object := anObject
!.

NotFound class >> ! signalFor: anItem
  ^ self new object: anItem
!.




Collection >> ! addAll: aCollection
  aCollection do: [ :each | self add: each ].
  ^ aCollection
!.

Collection >> ! addIfNotPresent: anObject
  (self includes: anObject) ifFalse: [ self add: anObject ].
  ^ anObject
!.

Collection >> ! allSatisfy: aBlock
  self do: [:each | (aBlock value: each) ifFalse: [^ false]].
  ^ true
!.

Collection >> ! anySatisfy: aBlock
  self do: [:each | (aBlock value: each) ifTrue: [^ true]].
  ^ false
!.

Collection >> ! asArray
  | array index |
  array := Array new: self size.
  index := 0.
  self do: [:each | array at: (index := index + 1) put: each].
  ^ array
!.

Collection >> ! capacity
  ^ self size
!.

Collection >> ! collect: aBlock
  | newCollection |
  newCollection := self species new.
  self do: [:each | newCollection add: (aBlock value: each)].
  ^ newCollection
!.

Collection >> ! collect: aBlock as: aClass
  "Evaluates aBlock with each of the receiver's elements.
  Collects the resulting values into a new instance of aClass, and answers it."
  ^ (aClass new: self size) fillFrom: self with: aBlock
!.

Collection >> ! collect: aBlock into: aCollection
  ^ aCollection fillFrom: self with: aBlock
!.

Collection >> ! contents
  ^ self
!.

Collection >> ! copyEmpty
  ^ self species new
!.

Collection >> ! count: aBlock
  "Answers the number of elements for which the block answers true.
  Equivalent to (self select: aBlock) size, but faster."
  | sum |
  sum := 0.
  self do: [ :each | (aBlock value: each) ifTrue: [sum := sum + 1]].
  ^ sum
!.

Collection >> ! detect: aBlock
  ^ self detect: aBlock ifNone: [self errorNotFound: aBlock]
!.

Collection >> ! detect: detectBlock ifFound: foundBlock
  self detect: detectBlock ifFound: foundBlock ifNone: [ 7 "Do nothing." ]
!.

Collection >> ! detect: aBlock ifFound: foundBlock ifNone: exceptionBlock
  self do: [ :each |
    (aBlock value: each) ifTrue: [ ^ foundBlock cull: each ] ].
  ^ exceptionBlock value
!.

Collection >> ! detect: aBlock ifNone: exceptionBlock
  ^ self detect: aBlock ifFound: [ :el | el ] ifNone: exceptionBlock
!.

Collection >> ! do: elementBlock separatedBy: separatorBlock
  | beforeFirst |
  beforeFirst := true.
  self do:
    [:each |
      beforeFirst
        ifTrue: [beforeFirst := false]
        ifFalse: [separatorBlock value].
      elementBlock value: each]
!.

Collection >> ! do: aBlock without: anItem
  ^ self do: [:each | anItem = each ifFalse: [aBlock value: each]]
!.

Collection >> ! fillFrom: aCollection with: aBlock
  "Evaluate aBlock for each of aCollection's elements. Collect the resulting
  values into self. Answer self."
  aCollection do: [ :each | self add: (aBlock value: each) ]
!.

Collection >> ! hash
  | hash |
  hash := self species hash.
  self size <= 10 ifTrue:
    [self do: [:each | hash := hash bitXor: each hash]].
  ^ hash bitXor: self size hash
!.

Collection >> ! ifEmpty: aBlock
  ^ self isEmpty ifTrue: [ aBlock value ] ifFalse: [ self ]
!.

Collection >> ! ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock
  self isEmpty ifTrue: [ ^ emptyBlock value ].
  ^ notEmptyBlock cull: self
!.

Collection >> ! ifNotEmpty: aBlock
  ^ self isEmpty ifTrue: [ self ] ifFalse: [ aBlock cull: self ]
!.

Collection >> ! includes: anObject
  ^ self anySatisfy: [ :each | each = anObject ]
!.

Collection >> ! includesAll: aCollection
  aCollection do: [ :each | (self includes: each) ifFalse: [ ^ false] ].
  ^ true
!.

Collection >> ! includesAny: aCollection
  aCollection do: [ :each | (self includes: each) ifTrue: [ ^ true ] ].
  ^ false
!.

Collection >> ! inject: thisValue into: binaryBlock
  | nextValue |
  nextValue := thisValue.
  self do: [ :each | nextValue := binaryBlock value: nextValue value: each ].
  ^ nextValue
!.

Collection >> ! isCollection
  ^ true
!.
Object >> ! isCollection
  ^ false
!.

Collection >> ! isEmpty
  ^self size = 0
!.

Collection >> ! isEmptyOrNil
  ^ self isEmpty
!.
NullObject >> ! isEmptyOrNil
  ^ true
!.
Object >> ! isEmptyOrNil
  ^ false
!.

Collection >> ! isNotEmpty
  ^ self isEmpty not
!.

Collection >> ! isSequenceable
  ^ false
!.

Collection >> ! noneSatisfy: aBlock
  self do: [ :each | (aBlock value: each) ifTrue: [ ^ false ] ].
  ^ true
!.

Collection >> ! occurrencesOf: anObject
  | tally |
  tally := 0.
  self do: [ :each | anObject = each ifTrue: [ tally := tally + 1 ] ].
  ^ tally
!.

Collection >> ! reject: aBlock
  ^ self select: [ :each | (aBlock value: each) == false ]
!.

Collection >> ! remove: oldObject
  ^ self remove: oldObject ifAbsent: [self errorNotFound: oldObject]
!.

Collection >> ! removeAll
  self do: [:each | self remove: each]
!.

Collection >> ! removeAll: aCollection
  aCollection == self ifTrue: [ ^self removeAll ].
  aCollection do: [:each | self remove: each].
  ^ aCollection
!.

Collection >> ! select: aBlock
  | newCollection |
  newCollection := self copyEmpty.
  self do: [ :each |
    (aBlock value: each)
      ifTrue: [ newCollection add: each ]].
  ^ newCollection
!.

Collection >> ! size
  | tally |
  tally := 0.
  self do: [ :each | tally := tally + 1].
  ^ tally
!.

Collection >> ! sorted
  ^ self asArray sorted
!.

Collection >> ! sorted: aSortBlockOrNil
  ^ self asArray sort: aSortBlockOrNil
!.

