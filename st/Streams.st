Object subclass: #Stream.
Stream subclass: #PositionableStream instanceVariableNames: 'index coll'.

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

! skip: aDelta
  index := index + aDelta

! atEnd
  ^ index >= coll size

! contents
  ^ coll
!!

PositionableStream subclass: #WriteStream.

! WriteStream
! next
  self shouldNotImplement
! nextPut: aValue
  coll at: (index := index + 1) put: aValue
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
! nextPut: aValue
  coll at: (index := index + 1) put: aValue
!!

