Object subclass: #Morph
  instanceVariableNames: 'bounds color submorphs owner drawError'.

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

! width
  ^ bounds extent x
! height
  ^ bounds extent y

! color
  ^ color

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

! fullDrawOn: aCanvas
  self visible ifFalse: [^ self].
  (aCanvas isVisible: self fullBounds) ifFalse: [^ self].
  [
    (aCanvas isVisible: self bounds) ifTrue: [aCanvas drawMorph: self].
    self drawSubmorphsOn: aCanvas
    ] on: Error do: [:e |
      drawError := e freeze.
      self drawErrorOn: aCanvas]
  "TODO Drag and drop is highlighted here if I ever add that."

! drawErrorOn: aCanvas
  Transcript show: self class name, ' error: ', drawError printString.

! drawOn: aCanvas
  ^aCanvas fillRectangle: bounds color: color

! containsPoint: aPoint
  ^ bounds containsPoint: aPoint

! fullContainsPoint: aPoint
  ^ bounds containsPoint: aPoint


! handlesMouseDown: anEvent
  ^ false
! mouseDown: anEvent
  "Do nothing."
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


Morph subclass: #BorderedMorph
  instanceVariableNames: 'borderWidth borderColor'.


BorderedMorph subclass: #PasteUpMorph
  instanceVariableNames: 'backgroundMorph worldState'.

PasteUpMorph subclass: #WorldMorph.

! PasteUpMorph
! drawOn: aCanvas
  super drawOn: aCanvas.
  backgroundMorph ifNotNil: [backgroundMorph drawOn: aCanvas]

! drawSubmorphsOn: aCanvas
  submorphs isEmpty ifTrue: [^ self].
  submorphs reverseDo: [:m |
    m ~~ backgroundMorph ifTrue: [aCanvas fullDrawMorph: m]]
!!


! WorldMorph
! isWorld
  ^ true
! isWorldMorph
  ^ true
!!

! WorldMorph class
! theWorld
  self error: 'I should be a global variable - fix that!'
!!

"The top-level flow is:
World doOneCycle
   |
   v
WorldState >> doOneCycleFor: aWorld    (waits a bit before calling)
   |
   allHands do: [:h |
     ActiveHand := h.
     h processEvents].
   aWorld runStepMethods.
   self displayWorldSafely: aWorld

HandMorph queues up events; processEvents drains that queue."


