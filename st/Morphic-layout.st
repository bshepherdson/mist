! Morph
"Layout"
! layoutChanged
  | layout |
  fullBounds := nil.
  "layout := self layoutPolicy.
  layout ifNotNil: [layout flushLayoutCache]."
  owner ifNotNil: [owner layoutChanged]

! layoutProperties
  ^ extension ifNotNil: [extension layoutProperties]
! layoutPolicy
  ^ extension ifNotNil: [extension layoutPolicy]

! layoutProperties: aProperties
  self layoutProperties == aProperties ifTrue: [^self].
  self assureExtension layoutProperties: aProperties

! layoutPolicy: aLayoutPolicy
  self layoutPolicy == aLayoutPolicy ifTrue: [^self].
  self assureExtension layoutPolicy: aLayoutPolicy.
  self layoutChanged

! fullBounds
  fullBounds ifNotNil: [^fullBounds].
  self computeFullBounds.
  ^ fullBounds

! computeFullBounds
  "Errors here can make a huge mess, so we catch them."
  [self doLayoutIn: self layoutBounds] on: Error, Warning, Halt do: [:ex |
    fullBounds := bounds.
    ex pass]

! layoutBounds
  | inset box |
  inset := self layoutInset.
  box := self innerBounds.
  inset isZero ifTrue: [^box].
  ^ box insetBy: inset.

! innerBounds
  ^ self bounds insetBy: self borderStyle inset

! doLayoutIn: layoutBounds
  "Compute a new layout based on the given layout bounds."
  | box priorBounds |
  "0. Quick return. No children means nothing to lay out. #shrinkWrap doesn't
  work for childless morphs; use #minWidth and #minHeight instead."
  self hasSubmorphs ifFalse: [^ fullBounds := self outerBounds].

  "X.1. Prepare redraw. Testing #bounds or #layoutBounds would be sufficient for
  the parent, but outerBounds is what we need for all leaf nodes to be checked,
  so we use that."
  priorBounds := self outerBounds.

  "1. Give the children a chance to manually adjust *before* layout computation.
  This allows morphs to layout in their owner without having to use a layout
  policy."
  self submorphsDo: [:m | m ownerChanged].

  "2. Compute the new layout. This goes down the entire hierarchy. See
  #layoutInBounds: and #minExtent, which are the usual layout-policy callbacks."
  self layoutPolicy ifNotNil: [:layout |
    "2.1. Compute the new layout."
    self remoteProperty: #doLayoutAgain.
    layout layout: self in: layoutBounds.

    "2.2. Do one additional run on the layout if requested in #layoutInBounds."
    (self hasProperty: #doLayoutAgain) ifTrue: [
      self removeProperty: #doLayoutAgain.
      layout flushLayoutCache.
      layout layout: self in: layoutBounds].
    self assert: (self hasProperty: #doLayoutAgain) not].

  "3. Watch out for minimal extent and apply #shrinkWrap constraints."
  self adjustLayoutBounds.

  "4. Compute and set the new full bounds. IMPORTANT to finish layout
  computation."
  fullBounds := self privateFullBounds.

  "X.2. Redraw."
  box := self outerBounds.
  box = priorBounds ifFalse: [
    self invalidRect: (priorBounds quickMerge: box)]
!!

