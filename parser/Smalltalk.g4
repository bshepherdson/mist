/*
  Converted to ANTLR 4 by James Ladd (Redline Smalltalk Project http://redline.st).
  Adapted from the Amber Smalltalk grammar parser.pegjs
  
  2015/01/18 James Ladd (object@redline.st)
*/

grammar Smalltalk;

/* MethodsFor protocol thing is not included here. */
script:    (methods | expr '.')+;
methods:   '!' id (WS 'class')? '!' (method '!')+ '!';
method:    message pragma? temps? exprs;
message:   id | binsel id | (keysel id)+;
pragma:    '<' keymsg '>';
temps:     '|' id* '|';

unit:      id | literal | block | arrayconstructor | '(' expr ')';
unaryexpr: unit id+;
primary:   unit | unaryexpr;
exprs:     (expr '.')* ('^'? expr)?;
expr:      (id ':=')* expr2;
expr2:     primary | msgexpr (';' cascade)*;

msgexpr:   unaryexpr | binexpr | keyexpr;
cascade:   id | binmsg | keymsg;
binexpr:   primary binmsg+;
binmsg:    binsel primary;
binsel:    binchar binchar?;
keyexpr:   keyexpr2 keymsg;
keyexpr2:  binexpr | primary;
keymsg:    (keysel keyexpr2)+;
keysel:    id ':';

block:     '[' ((':' id)* '|')? temps? exprs ']';
arrayconstructor: '{' exprs '}';
literal:   number | stringlit | charconst | symconst | arrayconst | binding | eval;
arrayconst: '#' array | '#' bytearray;
bytearray:  '[' number* ']';
array:      '(' (literal | array | bytearray | arraysym )* ')';
number:     (DIG+ 'r')? '-'? alphanum+ ('.' alphanum+)? (exp '-'? DIG+)?;
stringlit:     '\'' CHAR* '\'';
charconst:  '$' CHAR;
symconst:   '#' symbol | '#' stringlit;
arraysym:   (id | ':')+;
exp:        'd' | 'e' | 'q' | 's';
binding:    '#{' (id '.')* id '}';
symbol:     id | binsel | keysel+;
eval:       '##(' temps? exprs ')';
binchar:    '+' | '-' | '*' | '/' | '~' | '|' | ',' | '<' | '>' | '=' | '&' | '@' | '?' | '\\' | '%';
id:         (LETTER | '_')(LETTER | DIG | '_')*;
alphanum:   LETTER | DIG;


LETTER:    'A'..'Z' | 'a'..'z';
DIG:       '0'..'9';
CHAR:      [0-9A-Za-z!@#$%^&*()[\]+=_~-];
WS:        [ \t\r\n]+ -> channel(HIDDEN);
COMMENT:   '"' .*? '"' -> skip;

