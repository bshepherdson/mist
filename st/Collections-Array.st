"Array was already defined in Collection.st"

Array class >> ! new
  ^ <builtin: 'new array'>
!.

Array >> ! size
  ^ <builtin: 'array_length'>
!.

Array >> ! rawAt: index
  "0-based Javascript index!"
  ^ <builtin: 'array_at:'>
!.
Array >> ! rawAt: index put: value
  "0-based Javascript index"
  <builtin: 'array_at:put:'>
!.

Array >> ! at: index ifAbsent: exceptionBlock
  "1-based Smalltalk index."
  ^ (index between: 1 and: self size)
    ifTrue: [ self rawAt: index - 1]
    ifFalse: exceptionBlock
!.

Array >> ! at: index put: value
  "1-based Smalltalk index."
  self rawAt: index - 1 put: value
!.

Array >> ! do: aBlock
  1 to: self size do: [ :idx | aBlock value: (self at: idx) ]
!.

