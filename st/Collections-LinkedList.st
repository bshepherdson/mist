Object subclass: #Link instanceVariableNames: 'link'.
"TODO: Make Process and ProcessTable subclasses of this."

! Link class
! nextLink: aLink
  ^ self new nextLink: aLink; yourself
!!

! Link
! nextLink: aLink
  link := aLink
! nextLink
  ^ link
!!


SequenceableCollection subclass: #LinkedList
  instanceVariableNames: 'head tail count'.

! LinkedList class
! new: aSize
  ^ self new
!!

! LinkedList
! initialize
  count := 0.

! at: anIndex ifAbsent: aBlock
  | link ix |
  anIndex > self size ifTrue: [^ aBlock value].
  link := head.
  ix := anIndex.
  [ ix > 1 and: [link isNotNil]] whileTrue: [
    ix := ix - 1.
    link := link nextLink].
  ^ link ifNil: [aBlock value]

! at: anIndex put: aLink
  self shouldNotImplement

! addFirst: aLink
  head ifNil: [
    head := tail := aLink.
    count := 1.
    ^ aLink].
  aLink nextLink: head.
  head := aLink.
  count := count + 1.
  ^ aLink

! addLast: aLink
  tail ifNil: [
    head := tail := aLink.
    count := 1.
    ^ aLink].
  aLink nextLink: nil.
  tail nextLink: aLink.
  tail := aLink.
  count := count + 1.
  ^ aLink

! add: aLink
  self addLast: aLink

! removeFirst
  | el |
  head ifNil: [self errorNotFound: 1].
  el := head.
  head := head nextLink.
  el nextLink: nil.
  count := count - 1.
  ^ el
! removeLast
  | el ret |
  tail ifNil: [self errorNotFound: 1].
  el := head.
  [ el nextLink == tail ] whileFalse: [ el := el nextLink ].
  "el is now right before the tail."
  el nextLink: nil.
  ret := tail.
  count := count - 1.
  tail := el.
  ^ ret

! do: aBlock
  | el |
  el := head.
  [ el isNil ] whileFalse: [
    aBlock value: el copy.
    el := el nextLink]

! size
  ^ count
!!

