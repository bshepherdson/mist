"Array was already defined in Collection.st"

! Array class
! basicNew
  ^ self basicNew: 0
! basicNew: aNumber
  <primitive: 25>
! new: aNumber
  ^ (self basicNew: aNumber) initialize
!!

! Array
! asArray
  ^ self
! size
  <primitive: 28>

! at: index ifAbsent: exceptionBlock
  "1-based Smalltalk index."
  ^ (index between: 1 and: self size)
    ifTrue:  [self at: index]
    ifFalse: exceptionBlock

! do: aBlock
  1 to: self size do: [ :idx | aBlock value: (self at: idx) ]

! shallowCopy
  "This isn't supposed to be overridden, but since Array is magic and just a JS
  array, it needs special handling."
  | dst |
  dst := self class new.
  1 to: self size do: [:each | dst at: each put: (self at: each)].
  ^ dst

! copyFrom: lo to: hi
  | dst |
  dst := Array new: hi - lo + 1.
  lo to: hi do: [:i | dst at: i - lo + 1 put: (self at: i)].
  ^ dst

! swap: i with: j
  | tmp |
  tmp := self at: i.
  self at: i put: (self at: j).
  self at: j put: tmp
! sort
  "Actually does a sort on the Array, using Smalltalk <."
  ^ self mergeSort: 1 to: self size
! sorted
  ^ self copy sort
! mergeSort: lo to: hi
  | mid |
  "Only one value: done."
  lo = hi ifTrue: [^nil].
  "Two values: swap into order."
  lo + 1 = hi
      ifTrue: [(self at: lo) > (self at: hi)
          ifTrue: [self swap: lo with: hi].
                  ^self].
  "More values: divide and sub-sort, then merge."
  mid := (hi + lo / 2) floor.
  self mergeSort: lo to: mid.
  self mergeSort: mid + 1 to: hi.
  self merge: lo thru: mid to: hi
! merge: lo thru: mid to: hi
  | loSide hiSide i j dst |
  loSide := self copyFrom: lo to: mid.
  hiSide := self copyFrom: mid + 1 to: hi.
  i := 1.
  j := 1.
  dst := lo.
  [i <= loSide size and: [j <= hiSide size]]
    whileTrue: [|x l h|
      l := loSide at: i.
      h := hiSide at: j.
      x := l <= h ifTrue: [i := i + 1. l] ifFalse: [j := j + 1. h].
      self at: dst put: x.
      dst := dst + 1].

  "Once one has run out, we just stream the other into place."
  [i <= loSide size] whileTrue: [
    self at: dst put: (loSide at: i).
    dst := dst + 1.
    i := i + 1].
  [j <= hiSide size] whileTrue: [
    self at: dst put: (hiSide at: j).
    dst := dst + 1.
    j := j + 1]

! printString
  "Prints the array as a {1. 2. 3} up to 20 elements, then elides the rest."
  | s |
  s := '{'.
  1 to: (self size min: 20)
    do: [:i | s := s
                 , (i > 1 ifTrue: ['. '] ifFalse: [''])
                 , (self at: i) printString ].
  self size > 20 ifTrue: [s := s , '. (' , (self size - 20) , ' more)'].
  ^ s , '}'
!!


! WordArray class
! basicNew
  ^ self basicNew: 0
! basicNew: aNumber
  <primitive: 25>
! new: aNumber
  ^ (self basicNew: aNumber) initialize
!!


! WordArray
! size
  <primitive: 50>
! at: anIndex
  <primitive: 51>
! at: anIndex put: aWord
  <primitive: 52>
!!


! SequenceableCollection
! isSequenceable
  ^ true
! species
  ^ OrderedCollection
! do: aBlock
  1 to: self size do: [:ix | aBlock value: (self at: ix)]
! select: aBlock "TODO: This is better with streaming, see Pharo."
  | dst |
  dst := self species new: self size.
  self do: [:each | (aBlock value: each) ifTrue: [dst add: each]].
  ^ dst

! copyFrom: start to: stop
  | dst |
  dst := self species new: stop - start + 1.
  start to: stop do: [:ix | dst at: ix - start + 1 put: (self at: ix)].
  ^ dst

! indexOf: anElement
  ^ self indexOf: anElement ifAbsent: [0]
! indexOf: anElement ifAbsent: exceptionBlock
  anIndex to: self size do: [:ix | (self at: ix) = anElement ifTrue: [^ix]].
  ^ exceptionBlock value

! indexOfSubCollection: aSubCollection startingAt: anIndex
  ^ self indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: [0]
! indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: exBlock
  | subSize |
  subSize := aSubCollection size.
  anIndex to: self size - subSize + 1 do: [:ix |
    (self copyFrom: ix to: ix + subSize - 1) = aSubCollection ifTrue: [^ix]].
  ^ exBlock value

!!


ArrayedCollection subclass: #OrderedCollection
    instanceVariableNames: 'array tally'.

! OrderedCollection class
! new: aSize
  | oc |
  oc := self basicNew.
  oc privInitCapacity: aSize.
  oc initialize.
  ^ oc

! new
  ^ self new: 8
!!

! OrderedCollection
! privInitCapacity: aSize
  array := Array new: aSize

! initialize
  super initialize.
  tally := 0

! at: anIndex
  ^ array at: anIndex
! at: anIndex put: aValue
  self ensureCapacity: anIndex.
  array at: anIndex put: aValue.
  tally := tally max: anIndex

! ensureCapacity: aSize
  array size < aSize ifTrue: [
    | arr |
    arr := Array new: aSize.
    1 to: tally do: [:ix | arr at: ix put: (array at: ix)].
    array := arr]

! add: aValue
  self at: tally + 1 put: aValue "That increases the tally."

! size
  ^ tally

! capacity
  ^ array size

! printString
  | s first |
  s := '{'.
  first := true.
  self do: [:each |
    first ifFalse: [s := s , '. '].
    s := s , each printString.
    first := false].
  ^ s, '}'
!!


