"This needs to be kept in sync with memory.ts"
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

! visible
  extension ifNil: [^ true].
  ^ extension visible

! visible: aBoolean
  (extension isNil and: [aBoolean]) ifTrue: [^ self].
  self visible == aBoolean ifTrue: [^ self].

  self changed.
  self assureExtension visible: aBoolean.
  self changed

! addMorph: aMorph
  self addMorphFront: aMorph

! addMorphFront: aMorph
  submorphs addFirst: aMorph
! addMorphBack: aMorph
  submorphs addLast: aMorph

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

! isWorldMorph
  ^ false

! currentWorld
  ^ World

! openInWorld
  self openInWorld: self currentWorld
! openInWorld: aWorld
  aWorld addMorph: self.
  aWorld startSteppingSubmorphsOf: self

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

! computeFullBounds
  submorphs ifNil: [^ fullBounds := self bounds].
  ^ fullBounds := submorphs
    inject: self bounds
    into: [:rect :child | rect merge: child fullBounds]

! ownerChanged
  "Do nothing by default."

! invalidRect: aRect
  self invalidRect: aRect from: self

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

! drawSubmorphsOn: aCanvas
  submorphs isEmpty ifTrue: [^ self].
  submorphs reverseDo: [:m | aCanvas fullDrawMorph: m]

! drawErrorOn: aCanvas
  Transcript show: self class name, ' error: ', drawError printString.

! drawOn: aCanvas
  ^aCanvas fillRectangle: bounds color: color

! containsPoint: aPoint
  ^ bounds containsPoint: aPoint

! fullContainsPoint: aPoint
  ^ fullBounds containsPoint: aPoint

! containsPoint: aPoint event: anEvent
  ^ self containsPoint: aPoint

! click: anEvent
  "Do nothing by default."
!!


! Object
! asMorph
  ^self printString asMorph
!!


Object subclass: #MorphExtension
  instanceVariableNames: 'layoutPolicy layoutProperties borderStyle visible'.

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

! visible
  ^ visible
! visible: aBoolean
  visible := aBoolean
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
! world
  worldState ifNil: [^super world].
  ^self

! addHand: aHandMorph
  aHandMorph owner: self.
  worldState addHand: aHandMorph

! drawOn: aCanvas
  super drawOn: aCanvas.
  backgroundMorph ifNotNil: [backgroundMorph drawOn: aCanvas]

! drawSubmorphsOn: aCanvas
  submorphs isEmpty ifTrue: [^ self].
  submorphs reverseDo: [:m |
    m ~~ backgroundMorph ifTrue: [aCanvas fullDrawMorph: m]]

! runStepMethods
  "TODO Implement these steps."

! owner
  ^ nil
!!


! WorldMorph
! initialize
  super initialize.
  backgroundMorph := Morph new
    bounds: (0@0 corner: 10000@10000);
    color: Color grey;
    yourself.
  worldState := WorldState new

! doOneCycle
  worldState doOneCycleFor: self

! cursor
  ^ worldState cursor

! isWorld
  ^ true
! isWorldMorph
  ^ true

! startSteppingSubmorphsOf: aMorph
  "TODO"
!!

Object subclass: #WorldState
  instanceVariableNames: 'hands cursor cycleQueue'.

! WorldState
! initialize
  super initialize.
  self privUiUpdateProcess

! addHand: aHandMorph
  (hands ifNil: [hands := OrderedCollection new]) add: aHandMorph

! privUiUpdateProcess
  cycleQueue := SharedQueue new.
  [[self doOneCycleNowFor: cycleQueue next.
    true] whileTrue ]
      forkAt: Processor userInterruptPriority

! cursor
  ^ cursor ifNil: [
    cursor := HandMorph new owner: self; yourself.
    self addHand: cursor.
    cursor]

! doOneCycleFor: aWorld
  "The frame counter is coming from the outside, so call right through."
  cycleQueue nextPut: aWorld.

! doOneCycleNowFor: aWorld
  hands do: [:h | ActiveHand := h. h processEvents].
  aWorld runStepMethods.
  self displayWorldSafely: aWorld

! displayWorldSafely: aWorld
  aWorld fullDrawOn: Display
!!

"These are in memory.ts, so keep them in sync!"
Morph subclass: #HandMorph
  instanceVariableNames: 'frameSem frameCount lastKey keysSem lastMouse mouseSem eventQueue'.

! HandMorph
! initialize
  super initialize.
  eventQueue := SharedQueue new

! handleListenEvent: anEvent
  "Does nothing for now. This is a global event listener mechanism, but I
  have no use for it currently."

! processEvents
  | evt |
  [eventQueue isEmpty] whileFalse: [
    evt := eventQueue next.
    self handleListenEvent: evt.
    "TODO: Focus. If there's a notion of a focus for events, the focus would
    be sent the event here, rather than the general dispatch. Eg. drag and
    drop."

    "Generic event dispatch from the World downward."
    self world processEvent: evt]

! privMakeInputHand
  frameCount := 0.
  frameSem := Semaphore new.
  keysSem := Semaphore new.
  mouseSem := Semaphore new.

  self privSpawnFrameThread.
  self privSpawnKeysThread.
  self privSpawnMouseThread.

! privSpawnFrameThread
  [[frameSem wait.
    frameCount := frameCount + 1.
    World doOneCycle.
    true] whileTrue ] forkAt: Processor timingPriority.
! privSpawnKeysThread
  [[keysSem wait.
    Transcript show: 'key event: ', lastKey printString.
    eventQueue nextPut: lastKey.
    true] whileTrue ] forkAt: Processor highIOPriority.
! privSpawnMouseThread
  [[mouseSem wait.
    Transcript show: 'mouse event: ', lastMouse printString.
    eventQueue nextPut: lastMouse.
    true] whileTrue ] forkAt: Processor highIOPriority.
!!

SystemDictionary at: #World put: WorldMorph new.
SystemDictionary at: #InputHand put: World cursor.
(SystemDictionary at: #InputHand) privMakeInputHand.
SystemDictionary at: #ActiveHand put: 1.
SystemDictionary at: #Display put: Canvas new.

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

