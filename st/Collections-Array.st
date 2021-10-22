"Array was already defined in Collection.st"

! Array class
! basicNew
  ^ self basicNew: 0
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
    ifTrue:  [self basicAt: index]
    ifFalse: exceptionBlock

! shouldBePrintedAsLiteral
  ^ self class == Array and: [
    self allSatisfy: [:each | each shouldBePrintedAsLiteral]]

! isLiteral
  ^ self class == Array and: [
    self allSatisfy: [:each | each isLiteral]]

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

!!


! WordArray class
! basicNew
  ^ self basicNew: 0
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


! SequenceableCollection class
! new: size streamContents: aBlock
  | s |
  s := (self new: size) writeStream.
  aBlock value: s.
  ^ s contents
!!

! SequenceableCollection
! initialize: aSize
  self initialize

! species
  ^ self class
! isSequenceable
  ^ true
! remove: anObject ifAbsent: aBlock
  self shouldNotImplement

! at: anIndex
  ^ self at: anIndex ifAbsent: [self errorNotFound: anIndex]

! first
  ^ self at: 1
! second
  ^ self at: 2
! third
  ^ self at: 3
! last
  ^ self at: self size

! readStream
  ^ ReadStream on: self

! writeStream
  ^ WriteStream on: self

! , aSequenceableCollection
  | dst |
  dst := WriteStream on: (self species new: self size + aSequenceableCollection size).
  dst nextPutAll: self.
  dst nextPutAll: aSequenceableCollection.
  ^ dst contents

! shallowCopy
  | dst |
  dst := self species new: self size.
  dst replaceFrom: 1 to: dst size with: self.
  ^ dst

! do: aBlock
  1 to: self size do: [:ix | aBlock value: (self at: ix)]

! do: aBlock separatedBy: sepBlock
  self isEmpty ifTrue: [^self].
  aBlock value: self first.
  2 to: self size do: [:idx | sepBlock value. aBlock value: (self at: idx)]

! reverseDo: aBlock
  self size to: 1 by: -1 do: [:ix | aBlock value: (self at: ix)]

! with: aSequenceableCollection do: aBinaryBlock
  self size = aSequenceableCollection size ifFalse:
    [self error: 'lengths must match for with:do:'].
  1 to: self size do: [:ix |
    aBinaryBlock value: (self at: ix) value: (aSequenceableCollection at: ix)]

! keysAndValuesDo: aBinaryBlock
  1 to: self size do: [:ix |
    aBinaryBlock value: ix value: (self at: ix)]

! copyFrom: start to: stop
  | dst |
  dst := self species new: stop - start + 1.
  start to: stop do: [:ix | dst at: ix - start + 1 put: (self at: ix)].
  ^ dst

! from: start to: end put: aValue
  start to: end do: [:ix | self at: ix put: aValue]

! indexOf: anElement
  ^ self indexOf: anElement ifAbsent: [0]
! indexOf: anElement ifAbsent: exceptionBlock
  1 to: self size do: [:ix | (self at: ix) = anElement ifTrue: [^ix]].
  ^ exceptionBlock value

! indexOfSubCollection: aSubCollection startingAt: anIndex
  ^ self indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: [0]
! indexOfSubCollection: aSubCollection startingAt: anIndex ifAbsent: exBlock
  anIndex to: self size - aSubCollection size + 1 do: [:ix |
    (self compareWith: aSubCollection startingAt: ix) ifTrue: [^ ix]].
  ^ exBlock value

! compareWith: aSequenceableCollection startingAt: anIndex
  1 to: aSequenceableCollection size do: [:ix |
    (self at: anIndex + ix - 1) = (aSequenceableCollection at: ix)
      ifFalse: [^ false]].
  ^ true

! swap: i with: j
  | tmp |
  tmp := self at: i.
  self at: i put: (self at: j).
  self at: j put: tmp

! sort
  "Actually does a sort on the Array, using Smalltalk <."
  ^ self mergeSort: 1 to: self size on: [:a :b | a < b]

! sorted
  ^ self copy sort

! mergeSort: lo to: hi on: aBlock
  | mid |
  "Only one value: done."
  lo >= hi ifTrue: [^nil].
  "Two values: swap into order."
  lo + 1 = hi
      ifTrue: [(aBlock value: (self at: hi) value: (self at: lo))
          ifTrue: [self swap: lo with: hi].
                  ^self].
  "More values: divide and sub-sort, then merge."
  mid := hi + lo // 2.
  self mergeSort: lo to: mid on: aBlock.
  self mergeSort: mid + 1 to: hi on: aBlock.
  self merge: lo thru: mid to: hi on: aBlock

! merge: lo thru: mid to: hi on: aBlock
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
      x := (aBlock value: l value: h)
        ifTrue: [i := i + 1. l]
        ifFalse: [j := j + 1. h].
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

! replaceFrom: lo to: hi with: src
  hi - lo + 1 = src size
    ifFalse: [self error: 'size of replacement does not match'].
  self replaceFrom: lo to: hi with: src startingAt: 1

! replaceFrom: lo to: hi with: src startingAt: start
  0 to: hi - lo do: [:ix |
    self at: lo + ix put: (src at: start + ix)]

! copyReplaceAll: oldSubCollection with: newSubCollection
  | s ix next |
  s := WriteStream on: self species new.
  ix := 1.
  [(next := self indexOfSubCollection: oldSubCollection startingAt: ix) = 0]
    whileFalse: [
      "First, copy the shared portion."
      ix to: next - 1 do: [:i | s nextPut: (self at: i)].
      "Then copy newSubCollection into place."
      s nextPutAll: newSubCollection.
      "Then ix should be advanced to next + oldSubCollection size."
      ix := next + oldSubCollection size].
  "At the tail end, copy the rest from ix to the end of self."
  ix to: self size do: [:i | s nextPut: (self at: i)].
  ^ s contents

! copyReplaceFrom: start to: stop with: aCollection
  "If stop < start, with stop = start - 1, this is an insertion at start
  without replacing anything. The code is actually the same for these two
  cases, since the prefix is 1 to start - 1, suffix stop + 1 to self size."
  | s |
  s := WriteStream on: (self species new: self size).
  "First the portion before the replacement."
  1 to: start - 1 do: [:ix | s nextPut: (self at: ix)].
  "Then the replacement collection."
  s nextPutAll: aCollection.
  "Then the suffix."
  stop + 1 to: self size do: [:ix | s nextPut: (self at: ix)].
  ^ s contents

! copyWith: anElement
  "TODO: Lazy and inefficient; optimize this if it's being used much."
  ^ self copyReplaceFrom: self size + 1 to: self size with: {anElement}

! findFirst: aBlock
  1 to: self size do: [:ix |
    (aBlock value: (self at: ix)) ifTrue: [^ ix]].
  ^ 0

! findLast: aBlock
  self size to: 1 by: -1 do: [:ix |
    (aBlock value: (self at: ix)) ifTrue: [^ ix]].
  ^ 0

! putContentsOn: aStream
  "Optimization over the readStream-based version on Collection."
  1 to: (self size min: 20) do: [:i |
    aStream print: (self at: i).
    aStream nextPutAll: self privStreamSeparator].
  self size > 20
    ifTrue: [aStream nextPut: '('; << (self size - 20); << ' more)'].

! atAll: aKeyCollection put: anObject
  aKeyCollection do: [:key | self at: key put: anObject]
! atAllPut: anObject
  1 to: self size do: [:ix | self at: ix put: anObject]
!!


ArrayedCollection subclass: #OrderedCollection
    instanceVariableNames: 'array startIndex finalIndex'.

! OrderedCollection class
! new: aSize
  | oc |
  oc := self basicNew.
  oc initialize: aSize.
  ^ oc

! new
  ^ self new: 10
!!

! OrderedCollection
! initialize: aSize
  super initialize: aSize.
  array := Array new: (aSize max: 10).
  startIndex := 3.
  finalIndex := 2

! privAdjustIndex: anIndex
  "Turns an external index into an internal array index."
  ^ anIndex + startIndex - 1

! at: anIndex ifAbsent: aBlock
  ^ (anIndex between: 1 and: self size)
    ifTrue: [array at: (self privAdjustIndex: anIndex)]
    ifFalse: aBlock

! at: anIndex put: aValue
  | ix |
  ix := self privAdjustIndex: anIndex.
  self ensureCapacity: ix.
  array at: ix put: aValue.
  finalIndex := finalIndex max: ix

! privNextSize: minimum
  "The new size is at least 10 longer than the requested total, and to at least
  double its original size, whichever is larger."
  ^ minimum + startIndex + 10 max: 2 * array size

! ensureCapacity: targetIndex
  array size < targetIndex ifTrue: [
    | arr |
    arr := Array new: (self privNextSize: targetIndex).
    startIndex to: finalIndex do: [:ix |
      arr at: ix put: (array at: ix)].
    array := arr]

! add: aValue
  self at: self size + 1 put: aValue

! remove: anObject ifAbsent: aBlock
  | ix |
  ix := startIndex.
  [ ix <= finalIndex ] whileTrue: [
    anObject = (array at: ix)
      ifTrue: [
        self removeIndex: ix.
        ^ anObject]
      ifFalse: [ix := ix + 1]].
  ^ aBlock value

! removeIndex: internalIndex
  "INTERNAL: Use removeAt: instead."
  array replaceFrom: internalIndex to: finalIndex - 1
    with: array startingAt: internalIndex + 1.
  array at: finalIndex put: nil.
  finalIndex := finalIndex - 1

! removeAt: anIndex
  | ix removed |
  ix := self privAdjustIndex: anIndex.
  removed := array at: ix.
  self removeIndex: ix.
  ^ removed

! after: anObject
  | ix |
  ix := self indexOf: anObject ifAbsent: [self errorNotFound: anObject].
  ^ self at: ix + 1

! before: anObject
  | ix |
  ix := self indexOf: anObject ifAbsent: [self errorNotFound: anObject].
  ^ self at: ix - 1

! add: newObject after: oldObject
  | index |
  index := self indexOf: oldObject ifAbsent: [self errorNotFound: oldObject].
  index + 1 to: self size do: [:ix |
    self at: ix + 1 put: (self at: ix)].
  self at: index + 1 put: newObject.
  ^ newObject

! add: newObject before: oldObject
  | index |
  index := self indexOf: oldObject ifAbsent: [self errorNotFound: oldObject].
  index = 1 ifTrue: [^ self addFirst: newObject].
  self size to: index by: -1 do: [:ix |
    self at: ix + 1 put: (self at: ix)].
  self at: index put: newObject.
  ^ newObject

! addFirst: anObject
  | dst |
  startIndex > 1 ifTrue: [
    startIndex := startIndex - 1.
    array at: startIndex put: anObject.
    ^ anObject].

  dst := Array new: self capacity + 8.
  dst at: 4 put: anObject.
  dst replaceFrom: 5 to: self size + 4 with: self startingAt: 1.
  array := dst.
  finalIndex := self size + 4.
  startIndex := 4.
  ^ anObject

! addAllFirst: aSequenceableCollection
  "If there's room for the collection before startIndex, just addFirst: them
  all in reverse order. If not, this is a lot more efficient as a concat of the
  two lists into a new inner array."
  | newArray touch |
  startIndex > aSequenceableCollection size ifTrue: [
    aSequenceableCollection reverseDo: [:each | self addFirst: each].
    ^ aSequenceableCollection].

  newArray := Array new: self capacity + aSequenceableCollection size.
  "This is the first index of the original self."
  touch := startIndex + aSequenceableCollection size.
  newArray replaceFrom: startIndex
    to: touch - 1
    with: aSequenceableCollection.
  newArray replaceFrom: touch
    to: touch + self size - 1
    with: array
    startingAt: startIndex.

  "StartIndex doesn't change."
  finalIndex := finalIndex + aSequenceableCollection size.
  array := newArray.
  ^ aSequenceableCollection

! addAllLast: aSequenceableCollection
  ^ self addAll: aSequenceableCollection

! removeFirst
  | oldObject |
  self isEmpty ifTrue: [self errorNotFound: 1].
  oldObject := array at: startIndex.
  array at: startIndex put: nil. "Drop the reference for GC!"
  startIndex := startIndex + 1.
  ^ oldObject

! removeLast
  | oldObject |
  self isEmpty ifTrue: [self errorNotFound: 1].
  oldObject := array at: finalIndex.
  array at: finalIndex put: nil. "Drop the reference for GC!"
  finalIndex := finalIndex - 1.
  ^ oldObject


! size
  ^ finalIndex - startIndex + 1

! capacity
  ^ array size
!!



OrderedCollection subclass: #SortedCollection
  instanceVariableNames: 'sortBlock'.

! SortedCollection class
! sortBlock: aBinaryBlock
  ^ SortedCollection new sortBlock: aBinaryBlock; yourself
!!

! SortedCollection
"Ban all the index-specific writes."
! at: anIndex put: aValue
  self shouldNotImplement
! addFirst: anObject
  self shouldNotImplement
! addLast: anObject
  self shouldNotImplement
! addAllFirst: aSequenceableCollection
  self shouldNotImplement
! addAllLast: aSequenceableCollection
  self shouldNotImplement

! initialize
  super initialize.
  sortBlock := [:a :b | a <= b]

! sortBlock
  ^ sortBlock

! sortBlock: aBinaryBlock
  sortBlock := aBinaryBlock.
  array mergeSort: startIndex to: finalIndex on: sortBlock

! privIndexFor: anObject
  "Returns the INTERNAL index for the given object, based on our sortBlock."
  "TODO: This can be a binary search for much better efficiency of lookup!"
  startIndex to: finalIndex do: [:ix |
    "Our target index is the leftmost one which sorts reversed."
    (sortBlock value: anObject value: (array at: ix)) ifTrue: [^ix]].
  "If we didn't return yet, it goes at the end."
  ^ finalIndex + 1

! add: anObject
  | ix |
  ix := self privIndexFor: anObject.
  (ix = startIndex and: [startIndex > 1]) ifTrue: [
    "Special case: it goes at the beginning, and there's room."
    array at: (startIndex := startIndex - 1) put: anObject.
    ^ anObject].
  (ix > finalIndex and: [finalIndex < self capacity]) ifTrue: [
    "Special case: it goes at the end, and there's room."
    array at: (finalIndex := finalIndex + 1) put: anObject.
    ^ anObject].

  self size = self capacity ifTrue: [
    "Special case: capacity currently filled, so we have to copy anyway.
    It's more efficient to do this in chunks."
    | newArray |
    newArray := Array new: (self privNextSize: self size + 1).
    "Copy the prefix, then suffix."
    newArray replaceFrom: 4 to: ix + 2 "+ 3 - 1" with: array startingAt: startIndex.
    newArray at: ix + 3 put: anObject.
    newArray replaceFrom: ix + 4 to: self size + 3 with: array startingAt: ix.
    array := newArray.
    startIndex := 4.
    finalIndex := self size + 4.
    ^ anObject].

  "If there's only room at one end, copy that way. If there's room at either
  end, copy the shorter way."
  (startIndex = 1 or: [ix - startIndex > (finalIndex - ix)]) ifTrue: [
    self privMakeSpaceUpwardFrom: ix.
    array at: ix put: anObject.
    ^ anObject].

  self privMakeSpaceDownwardFrom: ix.
  array at: ix - 1 put: anObject.
  ^ anObject

! privMakeSpaceDownwardFrom: index
  "index here is where the new item goes, so it's actually not being moved."
  array replaceFrom: startIndex - 1 to: index - 2 with: array startingAt: startIndex.
  startIndex := startIndex - 1

! privMakeSpaceUpwardFrom: index
  finalIndex to: index by: -1 do: [:ix |
    array at: ix + 1 put: (array at: ix)].
  finalIndex := finalIndex + 1

! asSortedCollection
  ^ self
! asSortedCollection: aBinaryBlock
  sortBlock == aBinaryBlock ifTrue: [^ self].
  ^ self species new sortBlock: aBinaryBlock; addAll: self; yourself

! sort
  ^ self "Nothing to do; it's already sorted in-place."

! sorted
  ^ self copy
!!

