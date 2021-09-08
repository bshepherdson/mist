/*
  Converted to ANTLR 4 by James Ladd (Redline Smalltalk Project http://redline.st).
  Adapted from the Amber Smalltalk grammar parser.pegjs
  
  2015/01/18 James Ladd (object@redline.st)
*/

grammar Smalltalk;

/* MethodsFor protocol thing is not included here. */
script:    (methods | expr '.')+;
methods:   '!' ID (WS 'class')? '!' (method '!')+ '!';
method:    message pragma? temps? exprs;
message:   ID | binsel ID | (keysel ID)+;
pragma:    '<' keymsg '>';
temps:     '|' ID* '|';

unit:      ID | literal | block | arrayconstructor | '(' expr ')';
unaryexpr: unit ID+;
primary:   unit | unaryexpr;
exprs:     (expr '.')* ('^'? expr)?;
expr:      (ID ':=')* expr2;
expr2:     primary | msgexpr (';' cascade)*;

msgexpr:   unaryexpr | binexpr | keyexpr;
cascade:   ID | binmsg | keymsg;
binexpr:   primary binmsg+;
binmsg:    binsel primary;
binsel:    BINCHAR BINCHAR?;
keyexpr:   keyexpr2 keymsg;
keyexpr2:  binexpr | primary;
keymsg:    (keysel keyexpr2)+;
keysel:    ID ':';

block:     '[' ((':' ID)* '|')? temps? exprs ']';
arrayconstructor: '{' exprs '}';
literal:   number | STRINGLIT | charconst | symconst | arrayconst | binding | eval;
arrayconst: '#' array | '#' bytearray;
bytearray:  '[' number* ']';
array:      '(' (literal | array | bytearray | arraysym )* ')';
number:     (DIG 'r')? '-'? ALPHANUM1 ('.' ALPHANUM1)? (exp '-'? DIG)?;
charconst:  '$' CHAR;
symconst:   '#' symbol | '#' STRINGLIT;
arraysym:   (ID | ':')+;
exp:        'd' | 'e' | 'q' | 's';
binding:    '#{' (ID '.')* ID '}';
symbol:     ID | binsel | keysel+;
eval:       '##(' temps? exprs ')';


BINCHAR:    [+*/~|,<>=&@?\\%-];
ID:         [A-Za-z_][A-Za-z0-9_]*;
ALPHANUM1:  [A-Za-z0-9]+;
DIG:        ('0'..'9')+;
STRINGLIT:  ['] (~['])* ['];
CHAR:       [0-9A-Za-z!@#$%^&*()[\]+=_~-];
WS:         [ \t\r\n]+ -> channel(HIDDEN);
COMMENT:    '"' .*? '"' -> skip;

