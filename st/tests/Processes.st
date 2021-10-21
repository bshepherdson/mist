TestCase subclass: #ProcessesTests.

Object subclass: #Chainer instanceVariableNames: 'in out count'.

! Chainer
! receive: from
  in := from.
  out := Association key: Semaphore new value: nil.
  count := 10.
  self spawn.
  ^ out

! spawn
  [
    [ in key wait.
      count := count - 1.
      out value: in value.
      out key signal.
      count = 0 ] whileFalse.
    ] fork
!!

! ProcessesTests
! testFork
  | sem1 sem2 value |
  sem1 := Semaphore new.
  sem2 := Semaphore new.
  [ sem1 wait.
    value := 7.
    sem2 signal
  ] fork.

  self assert: value equals: nil.
  sem1 signal.
  sem2 wait.
  self assert: value equals: 7

! testDance
  | sem1 sem2 count |
  sem1 := Semaphore new.
  sem2 := Semaphore new.
  [ 1 to: 30 do: [:ix | sem1 wait. count := ix. sem2 signal] ] fork.

  1 to: 30 do: [:ix | sem1 signal. sem2 wait. self assert: count equals: ix]

! testRing
  "Builds a ring of 20 threads and passes a series of messages around the ring."
  | head tail |
  tail := head := Association key: Semaphore new value: nil.
  20 timesRepeat: [tail := Chainer new receive: tail].
  1 to: 10 do: [:ix |
    head value: ix.
    head key signal.
    tail key wait.
    self assert: tail value equals: ix ]
!!

Transcript show: ProcessesTests suite run printString.
