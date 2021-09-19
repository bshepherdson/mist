"SUnit design: Pretty standard. TestCases subclasses contain methods, we call
all the ones that start with 'test'.

There are *three* possible outcomes from each test method:
- pass
- fail (assertions failed)
- error (errors signaled during the test run)
"

Object subclass: #TestAsserter.

TestAsserter subclass: #TestCase
    instanceVariableNames: 'testSelector expectedFails'.



! TestAsserter
! assert: aBoolOrBlock description: aStringOrBlock
  self assert: aBoolOrBlock description: aStringOrBlock resumable: false

! assert: aBoolOrBlock description: aStringOrBlock resumable: canResume
  | exception aString |
  aBoolOrBlock value ifTrue: [^self].

  aString := aStringOrBlock value.
  self logFailure: aString
  "TODO: Keep copying here, but we need working exceptions first."
!!



