"Array was already defined in Collection.st"

! Array class
! basicNew
  <builtin: 'new array'>
! new: aNumber
  <builtin: 'new array'>
!!

! Array
! asArray
  ^ self
! species
  ^ Array
! size
  <builtin: 'array_length'>
! rawAt: index
  "0-based Javascript index!"
  <builtin: 'array_at:'>
! rawAt: index put: value
  "0-based Javascript index"
  <builtin: 'array_at:put:'>
! add: aValue
  self rawAt: self size put: aValue
  "TODO This is not super fast, and could be sped up with a push builtin."

! at: index
  ^ self at: index ifAbsent: [(Error signal: 'bad array index') signal]

! at: index ifAbsent: exceptionBlock
  "1-based Smalltalk index."
  ^ (index between: 1 and: self size)
    ifTrue: [ self rawAt: index - 1]
    ifFalse: exceptionBlock

! at: index put: value
  "1-based Smalltalk index."
  self rawAt: index - 1 put: value

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
  Transcript show: {loSide. hiSide}.
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

