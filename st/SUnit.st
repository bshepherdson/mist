"SUnit design: Pretty standard. TestCases subclasses contain methods, we call
all the ones that start with 'test'.

There are *three* possible outcomes from each test method:
- pass
- fail (assertions failed)
- error (errors signaled during the test run)
"

Object subclass: #TestAsserter.

TestAsserter subclass: #TestCase
    instanceVariableNames: 'testSelector'.


! TestAsserter class
! classForTestResult
  ^ TestResult
! suiteClass
  ^ TestSuite
!!


! TestAsserter
! classForTestResult
  ^ self class classForTestResult

! assert: aBooleanOrBlock
  ^ self assert: aBooleanOrBlock description: 'Assertion failed'

! assert: aBooleanOrBlock description: aStringOrBlock
  aBooleanOrBlock value ifTrue: [^self].
  "We failed, so signal it."
  self classForTestResult failure signal: aStringOrBlock value

! assert: actual equals: expected
  "Raises an AssertionFailure if actual is different (using #=) from expected."
  ^ self assert: actual = expected
      description: [self comparingStringBetween: actual and: expected]

! assert: actual identicalTo: expected
  "Raises an AssertionFailure if actual is different (using #==) from expected."
  ^ self assert: actual == expected
      description: [self comparingIdentityStringBetween: actual and: expected]

! deny: aBooleanOrBlock
  self deny: aBooleanOrBlock description: 'Denial failed'

! deny: aBooleanOrBlock description: aStringOrBlock
  self assert: aBooleanOrBlock value not
      description: aStringOrBlock

! should: aBlock
  self assert: aBlock value

! should: aBlock description: aString
  self assert: aBlock value description: aString

! should: aBlock raise: anExceptionalEvent
  ^ self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
! should: aBlock raise: anExceptionalEvent description: aString
  ^ self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
      description: aString

! executeShould: aBlock inScopeOf: anExceptionalEvent
  ^[aBlock value. false] on: anExceptionalEvent do: [:ex | ex return: true]

! comparingStringBetween: actual and: expected
  ^ 'Got ', actual fullPrintString, ' instead of ', expected fullPrintString
! comparingIdenticalStringBetween: actual and: expected
  ^ actual fullPrintString, ' is not identical to ', expected fullPrintString

! fail
  ^ self assert: false
! fail: aDescriptionString
  ^ self assert: false description: aDescriptionString

! signalFailure: aString
  self classForTestResult failure signal: aString

! skip
  "Don't run this test, but also don't mark it as a failure."
  TestSkipped signal

! skip: aComment
  "Don't run this test, but also don't mark it as a failure."
  TestSkipped signal: aComment
!!


TestAsserter subclass: #TestCase
    instanceVariableNames: 'testSelector expectedFails'.

! TestCase class
! testSelectors
  ^self methods
    select: [:each | (each selector beginsWith: 'test') and: [each numArgs = 0]]

! buildSuite
  "Returns a suite of test cases. This suite can be composed of local and
  inherited methods if the superclass is abstract."
  "TODO Inheritance for abstract test cases/suites?"
  ^self buildSuiteFromSelectors

! buildSuiteFromSelectors
  ^ self buildSuiteFromMethods: self testSelectors

! buildSuiteFromMethods: testMethods
  ^testMethods
    inject: (self suiteClass named: self name asString)
    into: [:suite :selector | suite addTest: (self selector: selector). suite]

! isAbstract
  ^self name = #TestCase

! isTestCase
  ^true
! isUnitTest
  ^true

! run
  "Runs all test* methods on this class as a test suite."
  ^self buildSuite run

! run: aSymbol
  "Execute a testcase name, aSymbol, and return a test result."
  ^(self selector: aSymbol) run

! selector: aSymbol
  "Return a test case with aSymbol as its selector, but don't execute it."
  ^self new setTestSelector: aSymbol

! suite
  ^self buildSuite
!!

! TestCase
! performTest
  self perform: testSelector asSymbol
! run
  | result |
  result := self classForTestResult new.
  result runCase: self. "TODO Resources need an ensure: with reset here."
  ^ result
! run: aResult
  aResult runCase: self
! runCase
  [self setUp.
  self performTest]
    ensure: [self tearDown.
        self cleanUpInstanceVariables]

! setUp
  "Does nothing. Override for per-test setup."
! tearDown
  "Does nothing. Override for per-test setup."

! cleanUpInstanceVariables
  "TODO Expected failures need this."

! selector
  ^ testSelector

! setTestSelector: aSymbol
  testSelector := aSymbol
!!



Exception subclass: #TestFailure.

! TestFailure
! sunitAnnounce: aTestCase toResult: aTestResult
  aTestResult addFailure: aTestCase
!!



Object subclass: #TestResult
    instanceVariableNames: 'failures errors passed skipped'.

! TestResult
! initialize
  super initialize.
  passed := Array new.
  failures := Set new.
  errors := Array new.
  skipped := Array new

! addError: aTestCase
  ^errors add: aTestCase
! addFailure: aTestCase
  ^failures add: aTestCase
! addPass: aTestCase
  ^passed add: aTestCase
! addSkip: aTestCase
  ^skipped add: aTestCase

! runCase: aTestCase
  [aTestCase runCase.
  self addPass: aTestCase]
    on: self class failure , self class skip, self class warning, self class error
    do: [:ex | ex sunitAnnounce: aTestCase toResult: self]

!!

! TestResult class
! error
  ^self exError
! exError
  ^Error
! failure
  ^TestFailure
! skip
  ^TestSkipped
! warning
  "Warning that should be treated as a test failure."
  ^Deprecated
!!


Object subclass: #TestSuite
    instanceVariableNames: 'tests name'.

! TestSuite class
! named: aString
  | ts |
  ts := self new.
  ts name: aString.
  ^ts
!!

! TestSuite
! , aTestSuite
  | ts |
  ts := TestSuite new.
  ts addTests: self tests.
  ts addTests: aTestSuite tests.
  ^ ts

! name
  ^ name
! name: aString
  name := aString

! addTest: aTest
  self tests add: aTest
! addTests: aTestCollection
  aTestCollection do: [:each | self addTest: each]

! resultClass
  ^ TestAsserter classForTestResult

! run
  | result |
  result := self resultClass new.
  self run: result.
  ^result

! run: aTestResult
  self setUp.
  [ self tests
    do: [:each |
      each run: aResult.
      self changed: each ] ]
  ensure: [self tearDown]

! setUp
  "Does nothing, but can be overridden."
  "TODO Resources need to be initialized here."
! tearDown
  "Does nothing, but can be overridden."
  "TODO Resources need to be cleaned up here."

! tests
  ^ tests ifNil: [ tests := Array new ]
!!


