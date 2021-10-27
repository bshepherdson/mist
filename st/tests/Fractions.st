TestCase subclass: #FractionsTests.

! FractionsTests
! testGCD
  self assert: (12 gcd: 14) equals: 2.
  self assert: (12 gcd: 15) equals: 3.
  self assert: (12 gcd:  7) equals: 1.
  self assert: (14 gcd:  7) equals: 7.
  self assert: ( 7 gcd: 14) equals: 7.

! testLCM
  self assert: (12 lcm: 14) equals: 2 * 6 * 7.
  self assert: (12 lcm: 15) equals: 3 * 4 * 5.
  self assert: (12 lcm:  7) equals: 84.
  self assert: (14 lcm:  7) equals: 14.
  self assert: ( 7 lcm: 14) equals: 14.

! testDivisions
  self assert: 3 / 4 equals: (Fraction numerator: 3 denominator: 4).
  self assert: 3 / 6 equals: (Fraction numerator: 1 denominator: 2).
  self assert: 30 / 6 equals: 5.
  self assert: 0 / 6 equals: 0.

! testAdd
  self assert: (3/4) + (1/4) equals: 1.
  self assert: (3/4) + (1/2) equals: 5/4.
  self assert: (3/4) + (2/7) equals: 29/28.
  self assert: 3     + (8/3) equals: 17/3.
  self assert: (8/3) + 3     equals: 17/3.

! testSubtract
  self assert: (3/4) - (1/4) equals: 1/2.
  self assert: (1/4) - (1/2) equals: -1/4.
  self assert: (3/4) - (2/7) equals: 13/28.
  self assert: 3     - (8/3) equals: 1/3.
  self assert: (8/3) - 3     equals: (-1)/3.

! testMultiply
  self assert: (1/8) * (7/3) equals: 7/24.
  self assert: (7/3) * (7/3) equals: 49/9.
  self assert: (2/3) * (4/5) equals: 8/15.
  self assert: 3     * (8/3) equals: 8.
  self assert: (8/3) * 3     equals: 8.

! testDivide
  self assert: (1/8) / (7/3) equals: 3/56.
  self assert: (7/3) / (7/3) equals: 1.
  self assert: (2/3) / (4/5) equals: 5/6.
  self assert: 3     / (8/3) equals: 9/8.
  self assert: (8/3) / 3     equals: 8/9.

! testLessThan
  self assert: (1/8) < (7/3) equals: true.
  self assert: (7/3) < (7/3) equals: false.
  self assert: (2/3) < (4/5) equals: true.
  self assert: 3     < (8/3) equals: false.
  self assert: (8/3) < 3     equals: true.

! testMin
  self assert: ( 7/3 min:  0)   equals:  0.
  self assert: ( 0   min:  7/3) equals:  0.
  self assert: (-7/3 min:  0)   equals:  -7/3.
  self assert: ( 7/3 min:  7/3) equals:  7/3.
  self assert: (-7/3 min:  7/3) equals: -7/3.
  self assert: (-7/3 min: -4/3) equals: -7/3.
  self assert: ( 7/3 min:  4/3) equals:  4/3.
  self assert: ( 7/3 min:  7/4) equals:  7/4.

! testMax
  self assert: ( 7/3 max:  0)   equals:  7/3.
  self assert: ( 0   max:  7/3) equals:  7/3.
  self assert: (-7/3 max:  0)   equals:  0.
  self assert: ( 7/3 max:  7/3) equals:  7/3.
  self assert: (-7/3 max:  7/3) equals:  7/3.
  self assert: (-7/3 max: -4/3) equals: -4/3.
  self assert: ( 7/3 max:  4/3) equals:  7/3.
  self assert: ( 7/3 max:  7/4) equals:  7/3.

! testBetween
  self assert: ( 6/4 between: 3/7 and: 9/2).
  self deny:   ( 1/4 between: 3/7 and: 9/2).
  self assert: ( 9/2 between: 3/7 and: 9/2).
  self assert: ( 2/4 between: 3/7 and: 9/2).
  self assert: (10/6 between: 3/7 and: 9/2).
  self deny:   ( 2/9 between: 3/7 and: 9/2).
  self deny:   (10/2 between: 3/7 and: 9/2).

  self assert: ( 6/4 between: -3/7 and: 9/2).
  self assert: ( 3/4 between: -3/7 and: 9/2).
  self assert: ( 9/4 between: -3/7 and: 9/2).
  self assert: ( 2/4 between: -3/7 and: 9/2).
  self assert: ( 0/4 between: -3/7 and: 9/2).
  self deny:   (-2/4 between: -3/7 and: 9/2).
  self deny:   (-3/4 between: -3/7 and: 9/2).
  self deny:   (-4/4 between: -3/7 and: 9/2)
!!




Object subclass: #GCStressor instanceVariableNames: 'suite'.

! GCStressor
! suite: aTestSuite
  suite := aTestSuite

! runGCTimes: anInteger
  anInteger timesRepeat: [
    Transcript show: suite run printString]
!!


GCStressor new suite: (
  ArithmeticBasics suite,
  LogicBasics suite,
  ReflectionBasics suite,
  CollectionsGeneralArrays suite,
  CollectionsGeneralSets suite,
  CollectionsGeneralBags suite,
  CollectionsGeneralDictionary suite,
  CollectionsGeneralOrderedCollection suite,
  CollectionsGeneralLinkedList suite,
  CollectionsGeneralInterval suite,
  DictionariesTests suite,
  SequenceableCollectionArrays suite,
  SequenceableCollectionOCs suite,
  OrderedCollectionTests suite,
  SortedCollectionTests suite,
  FractionsTests suite
); runGCTimes: 1.

