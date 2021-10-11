TestCase subclass: #DictionariesTests.

! DictionariesTests
! sampleDict
  ^ Dictionary new addAll: {7. 8. 9}; yourself

! testAt
  | d |
  d := self sampleDict.
  self assert: (d at: 1) equals: 7.
  self assert: (d at: 2) equals: 8.
  self assert: (d at: 3) equals: 9.
  self assert: (d at: 4 ifAbsent: [12]) equals: 12.
  self should: [d at: 4] raise: KeyNotFound.
  self assert: d size equals: 3.
  self assert: d capacity >= d size

! testAssociationAt
  | d assoc |
  d := self sampleDict.
  assoc := d associationAt: 2.
  self assert: (d at: 2) equals: 8.
  self assert: assoc equals: (Association key: 2 value: 8).
  self assert: assoc key equals: 2.
  self assert: assoc value equals: 8.

  "They work like pointers."
  assoc value: 22.
  self assert: (d at: 2) equals: 22

! testKeyAtValue
  | d |
  d := self sampleDict.
  self assert: (d keyAtValue: 8) equals: 2.
  self assert: (d keyAtValue: 8 ifAbsent: [6]) equals: 2.
  self should: [d keyAtValue: 5] raise: ValueNotFound.
  self assert: (d keyAtValue: 5 ifAbsent: [6]) equals: 6.

! testKeys
  self assert: (Dictionary new addAll: {5. 6. 7. 8. 9}; yourself) keys
    equals: {1. 2. 3. 4. 5} asSet.
  self assert: (Dictionary new
      at: 'some'  put: 4;
      at: 'words' put: 8;
      at: 'as'    put: 9;
      at: 'keys'  put: 2;
      yourself) keys
    equals: {'some'. 'words'. 'as'. 'keys'} asSet.
  self assert: Dictionary new keys equals: Set new.

! testValues
  self assert: (Dictionary new addAll: {5. 6. 7. 8. 9}; yourself) values
    equals: {5. 6. 7. 8. 9} asSet.
  self assert: (Dictionary new
      at: 'some'  put: 4;
      at: 'words' put: 8;
      at: 'as'    put: 9;
      at: 'keys'  put: 2;
      yourself) values
    equals: {4. 8. 9. 2} asSet.
  self assert: Dictionary new values equals: Set new.

! testIncludesAssociation
  | d |
  d := self sampleDict.
  self assert: (d includesAssociation: (Association key: 2 value: 8)).
  self deny:   (d includesAssociation: (Association key: 2 value: 5)).
  self deny:   (d includesAssociation: (Association key: 6 value: 8)).

! testIncludesKey
  | d |
  d := self sampleDict.
  self assert: (d includesKey: 2).
  self deny:   (d includesKey: 6).

! testRemoveKey
  | d |
  d := self sampleDict.
  self assert: (d removeKey: 2) equals: 8.
  self assert: d equals: (Dictionary new at: 1 put: 7; at: 3 put: 9; yourself).
  self should: [d removeKey: 5] raise: KeyNotFound.
  self assert: (d removeKey: 5 ifAbsent: [6]) equals: 6.
  self assert: d equals: (Dictionary new at: 1 put: 7; at: 3 put: 9; yourself).
!!


