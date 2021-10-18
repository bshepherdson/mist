Object subclass: #Color instanceVariableNames: 'str'.

! Color class
! transparent
  ^ Color new jsString: 'transparent'
! black
  ^ Color new jsString: 'black'
! white
  ^ Color new jsString: 'white'
! red
  ^ Color new jsString: 'red'
! blue
  ^ Color new jsString: 'blue'
! green
  ^ Color new jsString: 'green'
! yellow
  ^ Color new jsString: 'yellow'
!!

! Color
! jsString
  ^ str
! jsString: aString
  str := aString
! = aColor
  self class == aColor class ifFalse: [^false].
  str = aColor jsString

! isTransparent
  ^ str = 'transparent'

! printOn: aStream
  aStream << 'Color(' << str << ')'
!!


Object subclass: #Point instanceVariableNames: 'x y'.

! Point
! x
  ^ x
! y
  ^ y
! privX: aNumber
  x := aNumber
! privY: aNumber
  y := aNumber

! printOn: aStream
  aStream << x << '@' << y

! putOn: aStream
  self printOn: aStream

! + aPointOrNumber
  aPointOrNumber class == Point
    ifTrue: [^(x + aPointOrNumber x) @ (y + aPointOrNumber y)].
  ^ (x + aPointOrNumber) @ (y + aPointOrNumber)
! - aPointOrNumber
  ^ self + aPointOrNumber negated
! * aPointOrNumber
  aPointOrNumber class == Point
    ifTrue: [^(x * aPointOrNumber x) @ (y * aPointOrNumber y)].
  ^ (x * aPointOrNumber) @ (y * aPointOrNumber)

! negated
  ^ x negated @ y negated

! extent: aPoint
  ^Rectangle new origin: self; extent: aPoint; yourself
! corner: aPoint
  ^Rectangle new origin: self; corner: aPoint; yourself
!!

! Number
! @ aNumber
  ^ Point new privX: self; privY: aNumber; yourself
!!

Object subclass: #Rectangle instanceVariableNames: 'origin corner'.

! Rectangle
! origin
  ^ origin
! extent
  ^ corner - origin
! corner
  ^ corner

! origin: aPoint
  origin := aPoint
! corner: aPoint
  corner := aPoint
! extent: aPoint
  corner := origin + aPoint

! insetBy: aPoint
  ^ origin + aPoint corner: corner - aPoint

! intersects: aRect
  | rOrigin rCorner |
  rOrigin := aRect origin.
  rCorner := aRect corner.
  corner x <= origin x ifTrue: [^false].
  corner y <= origin y ifTrue: [^false].
  rOrigin x >= corner x ifTrue: [^false].
  rOrigin y >= corner y ifTrue: [^false].
  ^ true

! = aRect
  self class == aRect class ifFalse: [^false].
  ^origin = aRect origin and: [corner = aRect corner]

! areasOutside: aRect
  "Returns an Array of Rectangles for all the areas of self that don't overlap
  with aRect. Returns an array of just self if there's no overlap."
  (self intersects: aRect) ifFalse: [^Array with: self].
  ^ Array new: 4 streamContents: [:aStream |
    | yOrigin yCorner |
    aRect origin y > origin y
      ifTrue: [
        aStream nextPut: (origin corner: corner x @ (yOrigin := aRect origin y))]
      ifFalse: [yOrigin := origin y].
    aRect corner y < corner y
      ifTrue: [
        aStream nextPut: (origin x @ (yCorner := aRect corner y) corner: corner)]
      ifFalse: [yCorner := corner y].

    aRect origin x > origin x ifTrue: [
      aStream nextPut: (origin x @ yOrigin corner: aRect origin x @ yCorner)].
    aRect corner x < corner x ifTrue: [
      aStream nextPut: (aRect corner x @ yOrigin corner: corner x @ yCorner)] ]

! printOn: aStream
  aStream << '(' << origin << ' corner: ' << corner << ')'
! putOn: aStream
  self printOn: aStream
!!



Object subclass: #Canvas.

! Canvas
! draw: anObject
  ^anObject drawOn: self
! drawMorph: aMorph
  "TODO: Maybe check if the Morph intersects with my bounds?"
  ^self draw: aMorph

! fullDrawMorph: aMorph
  "This is a hook for possible overrides."
  self fullDraw: aMorph

! fullDraw: anObject
  anObject fullDrawOn: self

! drawPolygon: vertices fillStyle: aFillStyle
  self drawPolygon: vertices fillStyle: aFillStyle borderWidth: 0
      borderColor: Color transparent
! drawPolygon: vertices fillStyle: aFillStyle borderWidth: bw borderColor: bc
  self drawPolygon: vertices color: aFillStyle asColor
      borderWidth: bw borderColor: bc
! drawPolygon: vertices color: aColor borderWidth: bw borderColor: bc
  self privDrawPolygon: vertices asArray color: aColor
      borderWidth: bw borderColor: bc
! privDrawPolygon: vertices color: aColor borderWidth: bw borderColor: bc
  <primitive: 60>


! fillRectangle: aRect color: aColor
  self privFillRectX: aRect origin x asSmallInteger
    y: aRect origin y asSmallInteger
    width: aRect extent x asSmallInteger
    height: aRect extent y asSmallInteger
    color: aColor
! privFillRectX: x y: y width: w height: h color: aColor
  <primitive: 61>

! drawString: s at: aPoint
  self drawString: s from: 1 to: s size in: (aPoint extent: 10000@10000)
    font: nil color: Color black
! drawString: s at: aPoint font: aFontOrNil
  self drawString: s from: 1 to: s size in: (aPoint extent: 10000@10000)
    font: aFontOrNil color: Color black
! drawString: s at: aPoint color: aColor
  self drawString: s from: 1 to: s size in: (aPoint extent: 10000@10000)
    font: nil color: aColor
! drawString: s at: aPoint font: aFontOrNil color: aColor
  self drawString: s from: 1 to: s size in: (aPoint extent: 10000@10000)
    font: aFontOrNil color: aColor
! drawString: s from: start to: end in: boundsRect font: aFontOrNil color: aColor
  <primitive: 63>
!!

