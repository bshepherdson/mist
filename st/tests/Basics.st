TestCase subclass: #Basics1.

! Basics1
! testAssert
  self assert: true
! testAssertFail
  self assert: false
! testError
  Error new signal
!!

Transcript show: Basics1 run printString.
99 halt.

