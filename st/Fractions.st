Number subclass: #Fraction instanceVariableNames: 'n d'.

! Integer
! asFraction
  ^ Fraction rawNumerator: self denominator: 1

! gcd: anInteger
  | a b tmp |
  a := self.
  b := anInteger.
  [ b = 0 ] whileFalse: [
    tmp := b.
    b := a \\ b.
    a := tmp].
  ^ a

! lcm: anInteger
  ^ self // (self gcd: anInteger) * anInteger
!!


! Fraction class
! rawNumerator: n denominator: d
  "Raw, no-checks Fraction construction.
  You probably want n / d for the various checks or numerator:denominator:"
  ^ self new rawNum: n denom: d

! numerator: n denominator: d
  d < 0 ifTrue: [^Fraction numerator: 0-n denominator: 0-d].
  ^ (self new rawNum: n denom: d) simplified
!!

! Fraction
! rawNum: newN denom: newD
  n := newN.
  d := newD

! = aFraction
  (aFraction isKindOf: Fraction) ifFalse: [^false].
  ^ n = aFraction numerator and: [d = aFraction denominator]

! printOn: aStream
  aStream nextPut: $(; << n; nextPut: $/; << d; nextPut: $)

! simplified
  "Canonical form is lowest terms, and either both positive or just numerator
  negative."
  | gcd sign nn dd |
  sign := n sign * d sign.
  nn := n abs.
  dd := d abs.
  dd = 1 ifTrue: [^ nn * sign].
  gcd := nn gcd: dd.
  gcd = 1 ifTrue: [^ self].
  gcd = dd ifTrue: [^ nn // dd * sign].
  ^ Fraction rawNumerator: nn // gcd * sign denominator: dd // gcd

! numerator
  ^ n
! denominator
  ^ d
! asFraction
  ^ self
! negated
  ^ Fraction numerator: 0 - n denominator: d
! reciprocal
  ^ Fraction numerator: d denominator: n

! + aNumber
  | aFraction newN |
  aFraction := aNumber asFraction.
  newN := n * aFraction denominator + (d * aFraction numerator).
  ^ Fraction numerator: newN denominator: d * aFraction denominator

! - aFraction
  ^ self + aFraction negated

! * aFraction
  ^ (aFraction isKindOf: Fraction)
    ifTrue: [Fraction numerator: n * aFraction numerator
        denominator: d * aFraction denominator]
    ifFalse: [Fraction numerator: n * aFraction denominator: d]

! / aNumber
  ^ (aNumber isKindOf: Fraction)
    ifTrue: [self * aNumber reciprocal]
    ifFalse: [Fraction numerator: n denominator: d * aNumber]

! < aNumber
  | f |
  f := aNumber asFraction.
  ^ n * f denominator < (d * f numerator)

!!

