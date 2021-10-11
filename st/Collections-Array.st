"Array was already defined in Collection.st"

! Array class
! basicNew
  ^ self basicNew: 0
! basicNew: aNumber
  <primitive: 25>
! new: aNumber
  ^ (self basicNew: aNumber) initialize

! with: item1
  "Array has to override these, since it doesn't have add:"
  ^ {item1}
! with: item1 with: item2
  ^ {item1. item2}
! with: item1 with: item2 with: item3
  ^ {item1. item2. item3}
! with: item1 with: item2 with: item3 with: item4
  ^ {item1. item2. item3. item4}
!!


! Array
! asArray
  ^ self
! size
  <primitive: 28>

! add: anElement
  self shouldNotImplement
! addAll: aCollection
  self shouldNotImplement

! at: index ifAbsent: exceptionBlock
  "1-based Smalltalk index."
  ^ (index between: 1 and: self size)
    ifTrue:  [self at: index]
    ifFalse: exceptionBlock

! do: aBlock
  1 to: self size do: [ :idx | aBlock value: (self at: idx) ]

! do: aBlock separatedBy: sepBlock
  self isEmpty ifTrue: [^self].
  aBlock value: self first.
  2 to: self size do: [:idx | sepBlock value. aBlock value: (self at: idx)]

! shouldBePrintedAsLiteral
  ^ self class == Array and: [
    self allSatisfy: [:each | each shouldBePrintedAsLiteral]]

! isLiteral
  ^ self class == Array and: [
    self allSatisfy: [:each | each isLiteral]]

! printOn: aStream
  self shouldBePrintedAsLiteral ifTrue: [
    self printAsLiteralFormOn: aStream. ^self].

  aStream nextPut: ${.
  self putContentsOn: aStream.
  aStream nextPut: $}

! printAsLiteralFormOn: aStream
  aStream nextPut: $#; nextPut: $(.
  self putContentsOn: aStream.
  aStream nextPut: $)

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
! putOn: aStream
  aStream nextPutAll: self
! isLiteral
  ^ self class == WordArray
!!


! SequenceableCollection
! species
  ^ self class
! isSequenceable
  ^ true
! remove: anObject ifAbsent: aBlock
  self shouldNotImplement

! readStream
  ^ ReadStream on: self

! writeStream
  ^ WriteStream on: self

! do: aBlock
  1 to: self size do: [:ix | aBlock value: (self at: ix)]

! keysAndValuesDo: aBinaryBlock
  1 to: self size do: [:ix |
    aBinaryBlock value: ix value: (self at: ix)]

! select: aBlock "TODO: This is better with streaming, see Pharo."
  | dst count |
  dst := self species new: self size.
  count := 0.
  self do: [:each | (aBlock value: each) ifTrue: [dst at: (count := count + 1) put: each]].
  ^ dst copyFrom: 1 to: count

! collect: aBlock
  "Can't use add: for Array, so we use at:put: here."
  | dst |
  dst := self species new: self size.
  1 to: self size do: [:ix | dst at: ix put: (aBlock value: (self at: ix))].
  ^ dst

! copyFrom: start to: stop
  | dst |
  dst := self species new: stop - start + 1.
  start to: stop do: [:ix | dst at: ix - start + 1 put: (self at: ix)].
  ^ dst

! indexOf: anElement
  ^ self indexOf: anElement ifAbsent: [0]
! indexOf: anElement ifAbsent: exceptionBlock
  1 to: self size do: [:ix | (self at: ix) = anElement ifTrue: [^ix]].
  ^ exceptionBlock value

! indexOfSubCollection: aSubCollection startingAt: anIndex
  ^ self indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: [0]
! indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: exBlock
  | subSize |
  subSize := aSubCollection size.
  anIndex to: self size - subSize + 1 do: [:ix |
    (self copyFrom: ix to: ix + subSize - 1) = aSubCollection ifTrue: [^ix]].
  ^ exBlock value

! putContentsOn: aStream
  "Optimization over the readStream-based version on Collection."
  1 to: (self size min: 20) do: [:i |
    aStream print: (self at: i).
    aStream nextPutAll: self privStreamSeparator].
  self size > 20
    ifTrue: [aStream nextPut: '('; << (self size - 20); << ' more)'].
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
!!


