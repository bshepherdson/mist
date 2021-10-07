Object subclass: Parser [
  | s |

  onStream: aStream [s := aStream]
  stream [^ s]

  error: aString [
    super error: aString, '  (', (s nextAvailable: 20), ')'
  ]

  parseWhile: aBlock [
    | ds |
    ds := OrderedCollection new.
    [ s atEnd not and: [aBlock value: s peek] ] whileTrue: [ ds add: s next ].
    ^ ds
  ]

  drainSpaces [
    [ s atEnd not and: [s peek isSeparator or: [s peek = $"]] ] whileTrue: [
      s peek = $"
        ifTrue: [self parseComment]
        ifFalse: [s next] ]
  ]

  parseDigits [
    ^ self parseWhile: [:ch | ch isDigit]
  ]

  parseDigitsBase [
    ^ self parseWhile: [:ch | ch isAlphaNumeric]
  ]

  parseNumber [
    "Number: (digits 'r')? '-'? digits
      - would be fractional and exps, but no floats."
    | base neg n |
    (s peekFor: $-) ifTrue: [
      (s atEnd not and: [s peek isDigit])
        ifTrue: [^ self parseDigits asString asInteger negated].
      "Otherwise return nil without consuming the -."
      s skip: -1.
      ^ nil].

    base := self parseDigits.
    base isEmpty ifTrue: [^nil].
    base := base asString asInteger.
    (s peekFor: $r) ifFalse: [^base]. "No r, so that's the number."

    neg := s peekFor: $-.
    n := 0.
    self parseDigitsBase asString asUppercase do: [:ch |
      | d |
      d := ch digitValue.
      d >= base ifTrue: [
        self error: 'Digit too large: ', ch asString,
          ' for base ', base printString].
      n := n * base + d].
    ^ neg ifTrue: [n negated] ifFalse: [n]
  ]

  parseSpecialChar [
    "I've removed ! from this set of special characters; we don't want ! to be
    an eligible special character."
    ('+/\*~<>=@%|&?,' asSet includes: s peek) ifTrue: [^s next].
    ^nil
  ]

  parseCharacter [
    "This is not a character constant with $, rather the inner portion."
    s peek isAlphaNumeric ifTrue: [^s next].
    ('[]{}()-^,;$!#:' asSet includes: s peek) ifTrue: [^s next].
    ^ self parseSpecialChar
  ]

  parseCharConstant [
    (s peekFor: $$) ifFalse: [^nil].
    (s peek = $' or: [s peek = $"]) ifTrue: [^s next].
    ^self parseCharacter
  ]

  parseIdentifier [
    s atEnd ifTrue: [^nil].
    s peek isLetter ifFalse: [^nil].
    ^ STIdentifier for: (self parseWhile: [:ch | ch isAlphaNumeric]) asString
  ]

  parseBinarySelector [
    | ch1 ch2 |
    (s peekFor: $-) ifTrue: [^ STBinarySelector for: '-'].
    ch1 := self parseSpecialChar.
    ch1 ifNil: [^nil]. "No match."
    ch2 := self parseSpecialChar.
    ch2 ifNil: [^ STBinarySelector for: ch1 asString].
    ^ STBinarySelector for: (String with: ch1 with: ch2)
  ]

  parseSymbol [
    | p |
    p := self parseBinarySelector.
    p isNil ifFalse: [^STSymbol for: p].
    p := self parseIdentifier.
    p ifNil: [^nil].
    (s peekFor: $:) ifTrue: [^STSymbol for: (self parseKeywords: p)].
    ^ STSymbol for: p
  ]

  parseKeyword [
    | pos id |
    pos := s position.
    id := self parseIdentifier ifNil: [^nil].
    (s peekFor: $:) ifTrue: [^id].
    s position: pos.
    ^ nil
  ]

  parseKeywords: firstIdent [
    | ks k |
    ks := OrderedCollection new.
    ks add: firstIdent x.
    [ k := self parseIdentifier. k isNil ] whileFalse: [
      ks add: k x.
      (s peekFor: $:) ifFalse: [^nil]].
    ^ STKeyword for: ks asArray
  ]

  parseSymbolConstant [
    (s peekFor: $#) ifFalse: [^nil].
    ^ self parseSymbol
  ]

  parseString [
    | chs ch |
    (s peekFor: $') ifFalse: [^nil].
    chs := ReadWriteStream on: Array new.
    [ ch := s next.
      ch = $'
        ifTrue: [
          "Got a quote - check if the next is a quote also."
          s peek = $'
            ifTrue:  ["Push one quote." s next. chs nextPut: ch. true]
            ifFalse: [false]]
        ifFalse: [
          chs nextPut: ch.
          true] ] whileTrue.
    ^ STString for: chs reset contents asString
  ]

  parseComment [
    (s peekFor: $") ifFalse: [^nil].
    s next.
    s upTo: $".
    s peek = $" ifTrue: [self parseComment]
  ]

  parseArray [
    "Parses an inner array, without the #."
    | vals v |
    vals := OrderedCollection new.
    (s peekFor: $() ifFalse: [^nil].
    self drainSpaces.
    [ s peek = $) ] whileFalse: [
      vals add: (self parseArrayElement
        ifNil: [self error: 'Could not parse array element']).
      self drainSpaces].
    s next. "Consume the closing paren"
    ^ STArray for: vals asArray
  ]

  parseArrayElement [
    "Parses a single array element: number, symbol (no #), string, char constant
    (with $) or nested array (just parens, no #)."
    s peek = $( ifTrue: [^self parseArray].
    s peek = $$ ifTrue: [^self parseCharConstant].
    s peek = $' ifTrue: [^self parseString].
    ^ self parseNumber ifNil: [self parseSymbol]
  ]

  parseLiteral [
    "Numbers, symbol constants, character constants, strings, array constants."
    | ch |
    ch := s peek.
    ch = $# ifTrue: [
      s next.
      s peek = $( ifTrue: [^self parseArray] ifFalse: [^self parseSymbol]].
    ch = $$ ifTrue: [^self parseCharConstant].
    ch = $' ifTrue: [^self parseString].
    ^ self parseNumber
  ]

  parsePrimary [
    "Variable name, literal, block, or parenthesized expression."
    | p |
    s peek = $[ ifTrue: [^ self parseBlock].
    s peek = ${ ifTrue: [^ self parseDynamicArray].
    s peek = $( ifTrue: [
      s next.
      p := self parseExpr.
      (s peekFor: $)) ifFalse: [self error: 'bad parenthesized expression'].
      ^ p].

    p := self parseLiteral.
    p isNil ifFalse: [^p].
    ^ self parseIdentifier
  ]

  parseDynamicArray [
    | elems |
    (s peekFor: ${) ifFalse: [^nil].
    elems := self parseMany: [self drainSpaces. self parseCascade]
      separatedOrEndedBy: [self drainSpaces. s peekFor: $.].
    self drainSpaces.
    (s peekFor: $}) ifFalse: [self error: 'expected closing }'].
    ^ STDynamicArray for: elems
  ]

  parseUnarySelector [
    "A unary selector is an identifier *not* followed by a :"
    | pos id |
    pos := s position.
    id := self parseIdentifier.
    s peek = $: ifTrue: [s position: pos. ^nil].
    self drainSpaces.
    ^ id
  ]

  parseMany: aBlock [
    | matches p |
    matches := OrderedCollection new.
    [ p := aBlock value. p isNil ] whileFalse: [
      matches add: p.
      self drainSpaces].
    ^ matches
  ]

  parseMany: aBlock separatedOrEndedBy: sepBlock [
    "Parses 0 or more of aBlock's return value, separated by sepBlock.
    sepBlock should return true if the separator matches.
    A trailing separator is allowed, but not required."
    | matches p |
    matches := OrderedCollection new.
    p := aBlock value.
    p ifNil: [^ matches]. "Empty"
    matches add: p. "First one in."
    self drainSpaces.

    "Now we parse 0 or more sep + body pairs."
    [ sepBlock value and: [
      self drainSpaces.
      p := aBlock value.
      p isNil not]
    ] whileTrue: [
      matches add: p.
      self drainSpaces].
    ^ matches
  ]

  parseUnaryExpr [
    "Railroad diagrams are left-recursive. This is a primary followed by 0 or
    more unary selectors."
    | prim selectors |
    prim := self parsePrimary ifNil: [^nil].
    self drainSpaces.
    selectors := self parseMany: [self parseUnarySelector].
    "This gets wrapped up in 0 or more unary sends."
    prim := STLiteral for: prim.
    selectors do: [:sel | prim := STUnarySend new inner: prim; selector: sel].
    ^ prim
  ]

  parseBinaryTail [
    "Parses a single binary 'tail': a binary selector and argument.
    Returns them as a two-element array."
    | op arg |
    op := self parseBinarySelector ifNil: [^nil].
    self drainSpaces.
    arg := self parseUnaryExpr ifNil: [self error: 'failed to parse binary expr'].
    ^ {op. arg}
  ]

  parseBinaryExpr [
    "Railroad diagrams are left-recursive. This parses a unary expression
    followed by 0 or more binary selectors + arguments.
    Returns a left-associating chain of STBinarySends."
    | inner pairs |
    inner := self parseUnaryExpr ifNil: [^nil].
    pairs := self parseMany: [self parseBinaryTail].
    pairs do: [:pair |
      inner := STBinarySend new left: inner;
          op: (pair at: 1);
          right: (pair at: 2)].
    ^ inner
  ]

  parseKeywordPair [
    "Parses a keyword: arg pair as a two-element array."
    | kw arg |
    kw := self parseKeyword ifNil: [^nil].
    self drainSpaces.
    arg := self parseBinaryExpr ifNil: [self error: 'Malformed keyword send arg'].
    ^ {kw. arg}
  ]

  parseKeywordExpr [
    "This is a binary expression followed by a keyword tail.
    Returns a STKeywordSend, or just a STBinarySend if there's no keywords."
    | rcvr tail |
    rcvr := self parseBinaryExpr ifNil: [^nil].
    self drainSpaces.
    tail := self parseKeywordTail ifNil: [^rcvr].
    tail receiver: rcvr.
    ^ tail
  ]

  parseKeywordTail [
    | pairs send |
    pairs := self parseMany: [self parseKeywordPair].
    pairs isEmpty ifTrue: [^nil].

    send := STKeywordSend new.
    pairs do: [:pair | send addKeywordArg: pair].
    ^ send
  ]

  parseMessageExpr [
    "Parses a single complete message send, from keywords down."
    ^ self parseKeywordExpr
  ]

  parseCascadeTail [
    "Parses a semicolon and cascade tail, which is either a keyword send, or a
    binary tail, or a unary *selector*."
    | p |
    (s peekFor: $;) ifFalse: [^nil].
    self drainSpaces.
    p := self parseKeywordTail.
    p isNil ifFalse: [^ p].
    p := self parseBinaryTail.
    p isNil ifFalse: [^ STBinarySend new op: p first; right: p last].
    p := self parseUnarySelector ifNil: [self error: 'bad cascade tail'].
    ^ STUnarySend new selector: p
  ]

  parseCascade [
    "A cascade is a complete send, followed by 0 or more '; cascadeTail' pairs."
    | first tails cascade |
    first := self parseMessageExpr ifNil: [^nil].
    tails := self parseMany: [self drainSpaces. self parseCascadeTail].
    tails isEmpty ifTrue: [^first].
    "We want to extract the inner sender from the first expression, then treat
    the rest of that first expression as a tail."
    cascade := STCascade new receiver: first receiver.
    cascade addTail: first asTail.
    tails do: [:tail | cascade addTail: tail].
    ^ cascade
  ]

  parseAssignment [
    "Parses 'ident :=' or consumes nothing."
    | pos id |
    pos := s position.
    id := self parseIdentifier ifNil: [^nil].
    self drainSpaces.
    ((s peekFor: $:) and: [s peekFor: $=]) ifTrue: [^ id].
    "Otherwise, reset and return nil."
    s position: pos.
    ^ nil
  ]

  parseExpr [
    "Parses 0 or more assignments followed by a cascade.
    Returns either a STAssignment or just the inner cascade."
    | ids cascade |
    ids := self parseMany: [self parseAssignment].
    cascade := self parseCascade.
    ^ ids isEmpty ifTrue: [cascade]
        ifFalse: [STAssignments new targets: ids; value: cascade]
  ]

  parseStatement [
    "Parses a single statement, generally just a cascade but maybe a return.
    Includes parsing trailing whitespace, plus the optional period and space
    after it."
    | inner |
    (s peekFor: $^) ifTrue: [
      "Handle a return: no period allowed after this."
      self drainSpaces.
      inner := self parseExpr.
      self drainSpaces.
      ^ STReturn for: inner
    ] ifFalse: [
      inner := self parseExpr.
      self drainSpaces.
      ^ inner
    ]
  ]

  parseStatements [
    "Parses 0 or more statements, separated by periods, with an optional final
    ^ return. Note that a trailing period is allowed, except after a ^return."
    | stmts hasReturn |
    self drainSpaces.
    stmts := self parseMany: [self parseStatement]
      separatedOrEndedBy: [s peekFor: $.].
    "Check that if there is a return, it's at the end."
    1 to: stmts size - 1 do: [:ix |
      ((stmts at: ix) isKindOf: STReturn)
        ifTrue: [self error: 'illegal return not at end of statements list']].
    "TODO: I don't think I'm properly banning a trailing dot after a return.
    It doesn't really matter."
    ^ stmts
  ]

  parseVar [
    "Parses a :identifier, returning the identifier."
    | id |
    (s peekFor: $:) ifFalse: [^nil].
    id := self parseIdentifier ifNil: [self error: 'malformed block argument'].
    self drainSpaces.
    ^ id
  ]

  parseBlock [
    "'[' ((':' identifier)+ '|')? ('|' temps '|')? statements ']'"
    | pos vars temps stmts |
    pos := s position.
    (s peekFor: $[) ifFalse: [^nil].
    self drainSpaces.

    vars := Array new.
    (s peek = $:) ifTrue: [
      vars := self parseMany: [self parseVar].
      self drainSpaces.
      (s peekFor: $|) ifFalse: [self error: 'failed to parse block args - no |']].

    self drainSpaces.
    temps := self parseTemporaries.
    self drainSpaces.

    stmts := self parseStatements ifNil: [self error: 'failed to parse block body'].
    (s peekFor: $]) ifFalse: [self error: 'failed to parse closing ]'].
    ^ STBlock new vars: vars; temps: temps; body: stmts.
  ]

  parseTemporaries [
    | temps |
    (s peekFor: $|) ifFalse: [^Array new].
    temps := self parseMany: [self drainSpaces. self parseIdentifier].
    self drainSpaces.
    (s peekFor: $|) ifFalse: [
      self error: 'failed to parse temporaries -  no closing |'].
    ^ temps
  ]

  parseBinarySignature [
    | p id |
    p := self parseBinarySelector ifNil: [^nil].
    self drainSpaces.
    id := self parseIdentifier
      ifNil: [self error: 'failed to parse binary signature'].
    ^ STBinarySend new op: p; right: id
  ]

  parseKeywordSignaturePair [
    | k id |
    k := self parseKeyword ifNil: [^nil].
    self drainSpaces.
    id := self parseIdentifier ifNil: [
      self error: 'failed to parse keyword signature'].
    ^ {k. id}
  ]

  parseKeywordSignature [
    | ks send |
    ks := self parseMany: [self parseKeywordSignaturePair].
    ks isEmpty ifTrue: [^nil].
    ^ STKeywordSend new kwargs: ks
  ]

  parseSignature [
    "Could be binary, keyword, or unary."
    | p |
    self drainSpaces.
    p := self parseBinarySignature.
    p isNil ifFalse: [^p].
    p := self parseKeywordSignature.
    p isNil ifFalse: [^p].
    p := self parseIdentifier.
    p isNil ifFalse: [^STUnarySend new selector: p].
    self error: 'failed to parse method signature 1: ', (s next: 30) printString
  ]

  parsePrimitive [
    "Primitives are '<primitive:' number '>'."
    | n |
    (s peekFor: $<) ifFalse: [self error: 'expected primitive'].
    self drainSpaces.
    self parseKeyword = (STIdentifier for: 'primitive') ifFalse: [
      self error: 'primitive should be <primitive: nnn>, bad keyword'].
    self drainSpaces.
    n := self parseNumber ifNil: [self error: 'expected primitive number'].
    self drainSpaces.
    (s peekFor: $>) ifFalse: [self error: 'expected closing > of primitive'].
    ^ n
  ]

  parseMethod [
    | method |
    method := STMethod new.
    method signature: (self parseSignature ifNil: [
      self error: 'failed to parse method signature 2']).
    self drainSpaces.

    s peek = $< ifTrue: [
      method primitive: self parsePrimitive.
      self drainSpaces].

    method temps: (self parseTemporaries ifNil: [
      self error: 'failed to parse temporaries']).
    self drainSpaces.
    method body: (self parseStatements ifNil: [
      self error: 'failed to parse method body']).
    ^ method
  ]

  parseMethods [
    | ast m |
    (s peekFor: $!) ifFalse: [^nil].
    self drainSpaces.
    ast := STMethodsBlock new.
    ast cls: (self parseIdentifier ifNil: [self error: 'expected class name']).
    self drainSpaces.

    (s peekFor: $!) ifFalse: [
      self parseIdentifier = (STIdentifier for: 'class') ifFalse: [
        self error: 'expected ''class'' or !'].
      ast classy: true.
      self drainSpaces.
      (s peekFor: $!) ifFalse: [self error: 'expected ! after class']].

    self drainSpaces.

    "Now we expect methods separated by !s. Two bangs in a row is the end."
    [ s peekFor: $! ] whileFalse: [
      m := self parseMethod ifNil: [self error: 'failed to parse method'].
      ast addMethod: m.
      self drainSpaces.
      (s peekFor: $!) ifFalse: [self error: 'expected ! after method'].
      self drainSpaces].

    self drainSpaces.
    ^ ast
  ]

  parseTopLevelCode [
    "Parses a single top-level entity."
    | stmt |
    s atEnd ifTrue: [^nil].
    s peek = $! ifTrue: [^self parseMethods].
    stmt := self parseStatement.
    self drainSpaces.
    (s peekFor: $.) ifFalse: [self error: 'expected . after top-level code'].
    self drainSpaces.
    ^ stmt
  ]

  parseFile [
    "Parses 0 or more top-level blocks: code lines and methods blocks."
    | chunks |
    self drainSpaces.
    chunks := self parseMany: [self parseTopLevelCode].
    self drainSpaces.
    s atEnd ifFalse: [self error: 'could not parse whole file'].
    ^ chunks
  ]

  parseFile: filename [
    s := filename asFile readStream.
    ^ self parseFile
  ]
]

Parser class extend [
  on: aString [
    | p |
    p := self new.
    p onStream: (ReadStream on: aString).
    ^ p
  ]
]

"(Parser new parseFile: '../../dcpu/mist/st/Kernel.st') do: [:each |
  Transcript print: each; cr; cr; cr]."

