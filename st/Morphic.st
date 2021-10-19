Object subclass: #Morph
  instanceVariableNames:
    'bounds fullBounds color submorphs owner drawError extension'.

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

! name
  ^self printString

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

! removeMorph: aMorph
  submorphs remove: aMorph

! submorphs
  ^ submorphs asArray

! submorphCount
  ^ submorphs size

! morphsDo: aBlock
  submorphs do: aBlock

! allMorphsDo: aBlock
  aBlock value: self.
  submorphs do: aBlock

! isInWorld
  ^ owner isNil not
! world
  ^ self isWorldMorph ifTrue: [self] ifFalse: [owner world]

! delete
  "Removes this morph from its owner."
  owner removeMorph: self

! copy
  ^ self class new bounds: bounds; color: color; yourself
! fullcopy
  | copy |
  copy := self copy.
  submorphs do: [:m | copy addMorph: m fullcopy].
  ^ copy

! changed
  "TODO"


"Layout"
! layoutChanged
  | layout |
  fullBounds := nil.
  layout := self layoutPolicy.
  layout ifNotNil: [layout flushLayoutCache].
  owner ifNotNil: [owner layoutChanged]

! layoutProperties
  ^ extension ifNotNil: [extension layoutProperties]
! layoutPolicy
  ^ extension ifNotNil: [extension layoutPolicy]
! layoutInset
  self layoutProperties ifNil: [0] ifNotNil: [:props | props layoutInset]

! layoutProperties: aProperties
  self layoutProperties == aProperties ifTrue: [^self].
  self assureExtension layoutProperties: aProperties

! layoutPolicy: aLayoutPolicy
  self layoutPolicy == aLayoutPolicy ifTrue: [^self].
  self assureExtension layoutPolicy: aLayoutPolicy.
  self layoutChanged

! assureExtension
  ^ extension ifNil: [extension := MorphExtension new]

! fullBounds
  fullBounds ifNotNil: [^fullBounds].
  self computeFullBounds.
  ^ fullBounds




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


Object subclass: #MorphExtension
  instanceVariableNames: 'layoutPolicy'.

! MorphExtension
! layoutPolicy
  ^ layoutPolicy
! layoutPolicy: aLayoutPolicy
  layoutPolicy := aLayoutPolicy

! layoutProperties
  ^ layoutProperties
! layoutProperties: aProperties
  layoutProperties := aProperties
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

! owner
  ^ nil
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


