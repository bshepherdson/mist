SequenceableCollection subclass: #Interval
  instanceVariableNames: 'start end step'.

! Interval class
! from: start to: stop
  ^ Interval new from: start; to: stop; by: 1; yourself
! from: start to: stop by: step
  ^ Interval new from: start; to: stop; by: step; yourself
!!

! Interval
! from: newStart
  start := newStart
! to: newEnd
  end := newEnd
! by: newStep
  step := newStep

! species
  "Special case for species: Intervals aren't mutable or addable, so they turn
  into OrderedCollections."
  ^ OrderedCollection

! size
  step < 0
    ifTrue:  [start < end ifTrue: [^0]]
    ifFalse: [start > end ifTrue: [^0]].
  ^ end - start + step // step
! capacity
  ^ self size

! add: anElement
  self shouldNotImplement
! at: anIndex put: aValue
  self shouldNotImplement
! remove: anObject
  self shouldNotImplement

! at: anIndex ifAbsent: aBlock
  | x |
  x := start + (anIndex - 1 * step).
  ^ x <= end ifTrue: [x] ifFalse: aBlock

! do: aBlock
  start to: end by: step do: aBlock
!!

