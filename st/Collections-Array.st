"Array was already defined in Collection.st"

! Array class
! basicNew
  <builtin: 'new array'>
!!

! Array
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
!!

