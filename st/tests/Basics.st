TestCase subclass: #Basics1.

! Basics1
! testPrint
  Transcript show: 7
! testSums
  Transcript show: 7
! testArgs
  Transcript show: 7
!!

"Transcript show: Basics1 run."

Object subclass: #Playground.
! Playground

! foo
  Error signal: 'some message'

! bar
  ^ self foo

! baz
  ^ [self bar] on: Error do: [Transcript show: 'caught!']

! holla: value
  ^ [^9]
!!

"Playground new baz."
Transcript show: (Playground new holla: 6) value.
99 halt.

"START HERE: This is better, but the answerBlock is slightly busted. Needs
more investigation."

