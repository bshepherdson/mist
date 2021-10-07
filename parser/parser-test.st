
TestCase subclass: ParseTest [
  tryParse: aString as: aNumber [
    | p |
    p := Parser on: aString.
    self assert: p parseNumber equals: aNumber
  ]

  testDecimal [
    self assert: (Parser on: '1'    ) parseNumber equals: 1.
    self assert: (Parser on: '123'  ) parseNumber equals: 123.
    self assert: (Parser on: '0'    ) parseNumber equals: 0.
    self assert: (Parser on: '99'   ) parseNumber equals: 99.
    self assert: (Parser on: '10r99') parseNumber equals: 99
  ]

  testNegated [
    self assert: (Parser on: '-1'  ) parseNumber equals: -1.
    self assert: (Parser on: '-123') parseNumber equals: -123.
    self assert: (Parser on: '-0'  ) parseNumber equals: 0.
    self assert: (Parser on: '-99' ) parseNumber equals: -99
  ]

  testHex [
    self assert: (Parser on: '16r12'  ) parseNumber equals: 18.
    self assert: (Parser on: '16rf'   ) parseNumber equals: 15.
    self assert: (Parser on: '16rCAFE') parseNumber equals: 51966.
    self assert: (Parser on: '16r-12' ) parseNumber equals: -18
  ]

  testCharConstant [
    self assert: (Parser on: '$a')  parseCharConstant equals: $a.
    self assert: (Parser on: '$X')  parseCharConstant equals: $X.
    self assert: (Parser on: '$7')  parseCharConstant equals: $7.
    self assert: (Parser on: '$$')  parseCharConstant equals: $$.
    self assert: (Parser on: '$-')  parseCharConstant equals: $-.
    self assert: (Parser on: '$''') parseCharConstant equals: $'.
    self assert: (Parser on: '$"')  parseCharConstant equals: $"
  ]

  testIdentifier [
    self assert: (Parser on: 'asdf') parseIdentifier
      equals: (STIdentifier for: 'asdf').
    self assert: (Parser on: 'x')    parseIdentifier
      equals: (STIdentifier for: 'x').
    self assert: (Parser on: 'x19y') parseIdentifier
      equals: (STIdentifier for: 'x19y')
  ]

  testBinarySelector [
    self assert: (Parser on: '>')   parseBinarySelector
      equals: (STBinarySelector for: '>').
    self assert: (Parser on: '-')   parseBinarySelector
      equals: (STBinarySelector for: '-').
    self assert: (Parser on: '>>')   parseBinarySelector
      equals: (STBinarySelector for: '>>').
    self assert: (Parser on: '==')   parseBinarySelector
      equals: (STBinarySelector for: '==')
  ]

  testKeyword [
    self
      assert: ((Parser on: 'def:')  parseKeywords: (STIdentifier for: 'abc'))
      equals: (STKeyword for: {'abc'. 'def'}).
    self
      assert: ((Parser on: '')  parseKeywords: (STIdentifier for: 'abc'))
      equals: (STKeyword for: {'abc'}).
    self
      assert: ((Parser on: 'def:cyx:c9:')
        parseKeywords: (STIdentifier for: 'abc'))
      equals: (STKeyword for: {'abc'. 'def'. 'cyx'. 'c9'})
  ]

  testSymbol [
    self assert: (Parser on: '77')  parseSymbol equals: nil.
    self assert: (Parser on: 'abc:def:')  parseSymbol
      equals: (STSymbol for: (STKeyword for: {'abc'. 'def'})).
    self assert: (Parser on: 'abc')  parseSymbol
      equals: (STSymbol for: (STIdentifier for: 'abc')).
    self assert: (Parser on: '>>')  parseSymbol
      equals: (STSymbol for: (STBinarySelector for: '>>'))
  ]

  testSymbolConstant [
    self assert: (Parser on: '#abc:def:')  parseSymbolConstant
      equals: (STSymbol for: (STKeyword for: {'abc'. 'def'})).
    self assert: (Parser on: '#abc')  parseSymbolConstant
      equals: (STSymbol for: (STIdentifier for: 'abc')).
    self assert: (Parser on: '#>>')  parseSymbolConstant
      equals: (STSymbol for: (STBinarySelector for: '>>'))
  ]

  testString [
    self assert: (Parser on: '''asdf''') parseString
      equals: (STString for: 'asdf').
    self assert: (Parser on: '''as"d x"f''') parseString
      equals: (STString for: 'as"d x"f').
    self assert: (Parser on: '''as''''d x''''f''') parseString
      equals: (STString for: 'as''d x''f') "lol escaping"
  ]

  testComment [
    | p |
    p := Parser on: '"asdf"'.
    self assert: p parseComment equals: p.
    self assert: p stream atEnd.
    p := Parser on: '"asdf   ""   another comment
       spanning lines"'.
    self assert: p parseComment equals: p.
    self assert: p stream atEnd.
  ]

  testArrayElement [
    self assert: (Parser on: '77') parseArrayElement
         equals: 77.
    self assert: (Parser on: 'asdf') parseArrayElement
         equals: (STSymbol for: (STIdentifier for: 'asdf')).
    self assert: (Parser on: 'asdf:cyx:') parseArrayElement
         equals: (STSymbol for: (STKeyword for: {'asdf'. 'cyx'})).
    self assert: (Parser on: '@') parseArrayElement
         equals: (STSymbol for: (STBinarySelector for: '@')).
    self assert: (Parser on: '-') parseArrayElement
         equals: (STSymbol for: (STBinarySelector for: '-')).
    self assert: (Parser on: '$a') parseArrayElement
         equals: $a.
  ]

  testArray [
    self assert: (Parser on: '($a $b $c)') parseArray
         equals: (STArray for: {$a. $b. $c}).
    self assert: (Parser on: '($a 77 foo)') parseArray
         equals: (STArray for: {$a. 77. (STSymbol for: (STIdentifier for: 'foo'))}).
    self assert: (Parser on: '(  $a   77   foo   )') parseArray
         equals: (STArray for: {$a. 77. (STSymbol for: (STIdentifier for: 'foo'))}).
    self assert: (Parser on: '(  )') parseArray
         equals: (STArray for: Array new).
    self assert: (Parser on: '(- + << abc:def: ''foo spaces'')') parseArray
         equals: (STArray for: {
           STSymbol for: (STBinarySelector for: '-').
           STSymbol for: (STBinarySelector for: '+').
           STSymbol for: (STBinarySelector for: '<<').
           STSymbol for: (STKeyword for: {'abc'. 'def'}).
           STString for: 'foo spaces'}).
  ]

  testParseLiteral [
    self assert: (Parser on: '16r2f') parseLiteral
         equals: 47.
    self assert: (Parser on: '#foo') parseLiteral
         equals: (STSymbol for: (STIdentifier for: 'foo')).
    self assert: (Parser on: '#foo:bar:') parseLiteral
         equals: (STSymbol for: (STKeyword for: {'foo'. 'bar'})).
    self assert: (Parser on: '#<<') parseLiteral
         equals: (STSymbol for: (STBinarySelector for: '<<')).
    self assert: (Parser on: '#(7 foo $3)') parseLiteral
         equals: (STArray for: {7. STSymbol for: (STIdentifier for: 'foo'). $3}).
    self assert: (Parser on: '#()') parseLiteral
         equals: (STArray for: Array new).
    self assert: (Parser on: '''some string things''') parseLiteral
         equals: (STString for: 'some string things').
    self assert: (Parser on: '$$') parseLiteral
         equals: $$.
  ]
]


