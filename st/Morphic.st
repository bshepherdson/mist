Object subclass: #Morph
  instanceVariableNames: 'bounds color submorphs owner'.

! Morph
! initialize
  super initialize.
  bounds := 0@0 extent: 20@20.
  color := Color red.
  submorphs := OrderedCollection new

! position
  ^ bounds origin
! bounds
  ^ bounds

! position: aPoint
  bounds := aPoint extent: bounds extent
! bounds: aRect
  bounds := aRect
! extent: aPoint
  bounds := bounds origin extent: aPoint


! color: aColor
  color := aColor

! addMorph: aMorph
  submorphs add: aMorph

! changed
  "TODO"

! drawOn: aCanvas
  ^aCanvas fillRectangle: bounds color: color

!!



! Object
! asMorph
  ^self printString asMorph
!!


Morph subclass: #StringMorph
  instanceVariableNames: 'contents font bgColor'.

! StringMorph
! initialize
  super initialize.
  bgColor := Color transparent.
  font := '10px sans-serif'.
  contents := ''

! contents
  ^ contents
! font
  ^ font
! backgroundColor
  ^ bgColor
! contents: aString
  contents := aString.
  self fitContents.
  self changed.
! font: aString
  font := aString
! backgroundColor: aColor
  bgColor := aColor

! measureContents
  | measured |
  measured := self privMeasureText: contents font: font.
  ^ measured first @ measured second

! fitContents
  | newBounds changed |
  newBounds := self measureContents.
  changed := bounds extent ~= newBounds.
  self extent: newBounds.
  changed ifTrue: [self changed]

! privMeasureText: aString font: fontString
  "NB: This actually returns an array like {width. height}."
  <primitive: 62>

! drawOn: aCanvas
  bgColor isTransparent ifFalse: [
    aCanvas fillRectangle: bounds color: bgColor].
  aCanvas drawString: contents at: bounds origin color: color
!!

! String
! asMorph
  ^ StringMorph new contents: self; yourself
!!

