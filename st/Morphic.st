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

! owner
  ^ owner
! owner: aMorph
  owner := aMorph

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
  "Reports that redrawing is necessary."
  ^ fullBounds
    ifNil: [self invalidRect: self outerBounds]
    ifNotNil: [self invalidRect: fullBounds]

! fullBounds
  fullBounds ifNotNil: [^fullBounds].
  self computeFullBounds.
  ^ fullBounds

! ownerChanged
  "Do nothing by default."

! invalidRect: aRect from: aMorph
  | damageRect |
  self visible ifFalse: [^self].
  aRect hasPositiveExtent ifFalse: [^self].
  damageRect := aRect.
  aMorph == self ifFalse: [
    self clipSubmorphs
      ifTrue: [damageRect := aRect intersect: self clippingBounds]].
  owner ifNotNil: [owner invalidRect: damageRect from: self]

! clipSubmorphs
  ^ false
! clippingBounds
  ^ self bounds


! assureExtension
  ^ extension ifNil: [extension := MorphExtension new]

! borderStyle
  ^ extension ifNotNil: [extension borderStyle]
! borderStyle: anObject
  self borderStyle == anObject ifTrue: [^self].
  self assureExtension borderStyle: anObject


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
  instanceVariableNames: 'layoutPolicy layoutProperties borderStyle'.

! MorphExtension
! layoutPolicy
  ^ layoutPolicy
! layoutPolicy: aLayoutPolicy
  layoutPolicy := aLayoutPolicy

! layoutProperties
  ^ layoutProperties ifNil: [layoutProperties := IdentityDictionary new]
! layoutProperties: aProperties
  layoutProperties := aProperties

! borderStyle
  ^ borderStyle
! borderStyle: anObject
  borderStyle := anObject
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
! initialize
  backgroundMorph := Morph new
    bounds: (0@0 corner: 10000@10000);
    color: Color grey;
    yourself.
  worldState := WorldState new

! isWorld
  ^ true
! isWorldMorph
  ^ true
!!

Object subclass: #WorldState instanceVariableNames: 'hands cursor'.

! WorldState
! cursor
  ^ cursor ifNil: [cursor := HandMorph new owner: self; yourself]
!!

Morph subclass: #HandMorph instanceVariableNames: 'eventQueue'.

! HandMorph
! initialize
  eventQueue := OrderedCollection new
! handleEvent: anEvent
  eventQueue add: anEvent
!!

SystemDictionary at: #World put: WorldMorph new.

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


