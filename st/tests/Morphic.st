Morph subclass: #CrossMorph.

! CrossMorph
! horizontalBar
  ^ self bounds insetBy: 0 @ (self height / 3)
! verticalBar
  ^ self bounds insetBy: (self width / 3) @ 0

! drawOn: aCanvas
  aCanvas fillRectangle: self horizontalBar color: self color.
  (self verticalBar areasOutside: self horizontalBar) do: [:rect |
    aCanvas fillRectangle: rect color: self color]

! containsPoint: aPoint
  ^ (self horizontalBar containsPoint: aPoint) or: [
    self verticalBar containsPoint: aPoint]
!!

CrossMorph new
  bounds: (20@20 extent: 60@60);
  color: (Color new jsString: 'rgba(0, 0, 255, 0.6)'; yourself);
  drawOn: Canvas new.

