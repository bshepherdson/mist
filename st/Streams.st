Object subclass: #Stream.
Stream subclass: #PositionableStream instanceVariableNames: 'index coll'.

! Stream
! print: anObject
  anObject printOn: self
!!

! PositionableStream
! initialize
  super initialize.
  index := 0

! collection: aCollection
  coll := aCollection

! next
  self subclassResponsibilty
! nextPut: aValue
  self subclassResponsibilty

! peek
  | x |
  self atEnd ifTrue: [^nil].
  x := self next.
  self skip: -1.
  ^ x

! next: aCount
  | dst |
  aCount = 0 ifTrue: [^Array new].
  dst := Array new: aCount.
  1 to: aCount do: [:ix | dst at: ix put: self next]

! nextAvailable: aCount
  | dst |
  aCount = 0 ifTrue: [^Array new].
  dst := OrderedCollection new: aCount.
  1 to: aCount do: [:ix |
    self atEnd: [^dst asArray].
    dst add: self next].
  ^ dst asArray

! nextPutAll: aCollection
  "The element type is assumed to be the same as the stream."
  aCollection do: [:each | self nextPut: each]

! << anObject
  "If the input is the same class as the streamed collection (eg. Strings) then
  do nextPutAll:. If not, delegate to anObject putOn: self.
  Returns self for chaining."
  anObject class == coll class
    ifTrue:  [self nextPutAll: anObject]
    ifFalse: [anObject putOn: self]

! skip: aDelta
  index := index + aDelta

! atEnd
  ^ index >= coll size

! contents
  "Collections like OrderedCollection that expand are fine. Arrays and Strings
  need to be clipped and copied."
  ^ coll size = index ifTrue: [coll] ifFalse: [coll copyFrom: 1 to: index]
!!

PositionableStream subclass: #WriteStream.

! WriteStream
! next
  self shouldNotImplement
! nextPut: aValue
  index >= coll size ifTrue: [self privGrowCollection].
  coll at: (index := index + 1) put: aValue

! privGrowCollection
  "Doubles the size of the target string."
  | s |
  s := String new: coll size * 2.
  1 to: coll size do: [:ix | s at: ix put: (coll at: ix)].
  coll := s
!!

! WriteStream class
! on: aCollection
  "Creates a WriteStream aimed at the *start* of this collection, overwriting
  its existing elements."
  ^ self new collection: aCollection; yourself
!!

WriteStream subclass: #ReadWriteStream.

! ReadWriteStream
! next
  ^ coll at: (index := index + 1)
!!


PositionableStream subclass: #ReadStream.

! ReadStream
! next
  ^ coll at: (index := index + 1)
! nextPut: aValue
  self shouldNotImplement
!!

! ReadStream class
! on: aCollection
  ^ self new collection: aCollection; yourself
!!

