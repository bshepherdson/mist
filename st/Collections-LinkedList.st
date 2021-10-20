"Object subclass: #Link instanceVariableNames: 'link'."

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


"SequenceableCollection subclass: #LinkedList
  instanceVariableNames: 'head tail'."

! LinkedList class
! new: aSize
  ^ self new
!!

! LinkedList
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
    ^ aLink].
  aLink nextLink: head.
  head := aLink.
  ^ aLink

! addLast: aLink
  tail ifNil: [
    head := tail := aLink.
    ^ aLink].
  aLink nextLink: nil.
  tail nextLink: aLink.
  tail := aLink.
  ^ aLink

! add: aLink
  self addLast: aLink

! removeFirst
  | el |
  head ifNil: [self errorNotFound: 1].
  el := head.
  head := head nextLink.
  el nextLink: nil.
  ^ el
! removeLast
  | el ret |
  tail ifNil: [self errorNotFound: 1].
  el := head.
  [ el nextLink == tail ] whileFalse: [ el := el nextLink ].
  "el is now right before the tail."
  el nextLink: nil.
  ret := tail.
  tail := el.
  ^ ret

! removeLink: aLink
  ^ self removeLink: aLink ifAbsent: [self error: 'no such method!']

! removeLink: aLink ifAbsent: aBlock
  | tempLink |
  aLink == head
    ifTrue: [
      head := aLink nextLink.
      aLink == tail ifTrue: [tail := nil]]
    ifFalse: [
      tempLink := head.
      [ tempLink ifNil: [^aBlock value].
        tempLink nextLink == aLink ]
          whileFalse: [tempLink := tempLink nextLink].
      tempLink nextLink: aLink nextLink.
      aLink == tail ifTrue: [tail := tempLink]].
  "Not nilling the link enables us to delete while iterating."
  ^ aLink


! do: aBlock
  | el |
  el := head.
  [ el isNil ] whileFalse: [
    aBlock value: el copy.
    el := el nextLink]

! size
  | count |
  count := 0.
  self do: [:each | count := count + 1].
  ^ count
!!

