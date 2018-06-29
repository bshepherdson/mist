/*
  Converted to ANTLR 4 by James Ladd (Redline Smalltalk Project http://redline.st).
  Adapted from the Amber Smalltalk grammar parser.pegjs
  
  2015/01/18 James Ladd (object@redline.st)
*/

grammar Smalltalk;

script : method EOF;
method : methodHeader ws? sequence;
sequence : temps ws? statementBlock? | ws? statementBlock;
ws : (NEWLINE | WHITESPACE | COMMENT)+;
ws_oneline : (WHITESPACE | COMMENT)+;
temps : PIPE (ws? IDENTIFIER)+ ws? PIPE;
statementBlock : answer ws? # StatementAnswer
           | statements ws? PERIOD ws? answer # StatementExpressionsAnswer
           | statements PERIOD? ws? # StatementExpressions
           ;
answer : CARROT ws? expression ws? PERIOD?;
statements : statement (PERIOD statements)?;
statement : expression;
cascade : (keywordSend | binarySend) (ws? SEMI_COLON ws? message)+;
expression : assignment | cascade | keywordSend | binarySend | primitive;
message : binaryMessage | unaryMessage | keywordMessage;
assignment : variable ws? ASSIGNMENT ws? expression;
variable : IDENTIFIER;
binarySend : unarySend binaryTail?;
unarySend : operand ws? unaryTail?;
keywordSend : binarySend keywordMessage;
keywordMessage : ws? (keywordPair ws?)+;
keywordPair : KEYWORD ws? binarySend ws?;
operand : literal | reference | subexpression;
subexpression : OPEN_PAREN ws? expression ws? CLOSE_PAREN;
literal : runtimeLiteral | parsetimeLiteral;
runtimeLiteral : dynamicDictionary | dynamicArray | block;
block : BLOCK_START (blockParamList PIPE)? ws? sequence? BLOCK_END;
blockParamList : (ws? BLOCK_PARAM)+;
expressions : expression (PERIOD expressions)?;
dynamicDictionary : DYNDICT_START ws? expressions? ws? DYNARR_END;
dynamicArray : DYNARR_START ws? expressions? ws? DYNARR_END;
parsetimeLiteral : pseudoVariable | number | charConstant | literalArray | stringLit | symbol;
number : MINUS? (HEX | EXP | FLOAT | INTEGER);
charConstant : CHARACTER_CONSTANT;
pseudoVariable : RESERVED_WORD;
stringLit : STRING;
symbol : HASH bareSymbol;
primitive : LT ws? KEYWORD ws? stringLit ws? GT;
bareSymbol : (IDENTIFIER | BINARY_SELECTOR) | KEYWORD+ | stringLit;
literalArray : LITARR_START literalArrayRest;
literalArrayRest : ws? ((parsetimeLiteral | bareLiteralArray | bareSymbol) ws?)* CLOSE_PAREN;
bareLiteralArray : OPEN_PAREN literalArrayRest;
unaryTail : unaryMessage ws? unaryTail? ws?;
unaryMessage : ws? unarySelector;
unarySelector : IDENTIFIER;
keywords : KEYWORD+;
reference : variable;
binaryTail : binaryMessage binaryTail?;
binaryMessage : ws? BINARY_SELECTOR ws? (unarySend | operand);

unaryHeader : unarySelector;
binaryHeader : BINARY_SELECTOR ws? IDENTIFIER;
keywordHeader : (keywordHeaderPair ws_oneline?)+;
keywordHeaderPair : KEYWORD ws_oneline? IDENTIFIER;
methodHeader : (keywordHeader | binaryHeader | unaryHeader) ws_oneline? NEWLINE;

NEWLINE : '\n';
WHITESPACE : [ \t\r];
STRING : '\'' (.)*? '\'';
COMMENT : '"' (.)*? '"';
BLOCK_START : '[';
BLOCK_END : ']';
CLOSE_PAREN : ')';
OPEN_PAREN : '(';
PIPE : '|';
PERIOD : '.';
SEMI_COLON : ';';
MINUS : '-';
BINARY_SELECTOR : ('\\' | '+' | '*' | '/' | '=' | '>' | '<' | ',' | '@' | '%' | '~' | PIPE | '&' | MINUS | '?')+;
LT : '<';
GT : '>';
RESERVED_WORD : 'nil' | 'true' | 'false' | 'self' | 'super';
HEX : '16r' ([0-9a-fA-F]+);
INTEGER : [0-9]+;
FLOAT : INTEGER PERIOD INTEGER;
EXP : (FLOAT | INTEGER) 'e' INTEGER;
IDENTIFIER : [a-zA-Z]+[a-zA-Z0-9_]*;
CARROT : '^';
COLON : ':';
ASSIGNMENT : ':=';
HASH : '#';
LITARR_START : '#(';
DYNDICT_START : '#{';
DYNARR_END : '}';
DYNARR_START : '{';
KEYWORD : IDENTIFIER COLON;
BLOCK_PARAM : COLON IDENTIFIER;
CHARACTER_CONSTANT : '$' ('[' | ']' | '-' | [0-9a-zA-Z!@#$%^&*(){}+=|'":;,.<>?/~`]);
