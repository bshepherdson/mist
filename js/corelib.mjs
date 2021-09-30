// A later stage of bootstrapping, which runs after the initial bootstrap, then
// the AA tree functionality.

import {defClass, impoverishedClasses, lateBinding} from './bootstrap.mjs';
import {insert, mkDict} from './dict.mjs';
import {
  CLS_BOOLEAN, CLS_TRUE, CLS_FALSE,
  CLS_ASSOCIATION, CLS_CONTEXT, CLS_BLOCK_CLOSURE,
  CLS_ARRAY, CLS_CHARACTER, CLS_PROCESS, CLS_PROCESS_TABLE, CLS_OBJECT,
  CTX_PC, CTX_STACK_INDEX, CTX_METHOD, CTX_LOCALS, CTX_SENDER,
  MA_BOOLEAN, MA_TRUE, MA_FALSE, MA_CLASS_DICT, MA_NIL,
  BEHAVIOR_FORMAT, BEHAVIOR_METHODS, CLASS_NAME,
  FORMAT_VARIABLE_IV, CLASS_VAR1, PROCESS_TABLE_NEXT_PRIORITY, IV_BLOCK,
  basicNew, classOf, classTable, mkInstance,
  fromSmallInteger, toSmallInteger, wrapSymbol,
  read, readIV, write, writeIV, writeIVNew, writeArrayNew,
} from './memory.mjs';
import {vm} from './vm.mjs';

lateBinding.dictFactory = () => mkDict();
lateBinding.symbolize = wrapSymbol;

// And the class dictionary appender.

lateBinding.addToClassDict = (sym, cls) => {
  const dict = read(MA_CLASS_DICT);
  insert(dict, sym, cls);
};

write(MA_CLASS_DICT, mkDict(256));

// And update the nil method dictioaries we just created for those classes (and
// their metaclasses).

for (const name of Object.keys(impoverishedClasses)) {
  const cls = impoverishedClasses[name];
  writeIV(cls, BEHAVIOR_METHODS, mkDict());
  const metaclass = classOf(cls);
  writeIV(metaclass, BEHAVIOR_METHODS, lateBinding.dictFactory());
  const symbol = wrapSymbol(name);
  writeIV(cls, CLASS_NAME, symbol);
  lateBinding.addToClassDict(symbol, cls);
}

const object = read(classTable(CLS_OBJECT));
defClass(CLS_BLOCK_CLOSURE, 'BlockClosure', object, IV_BLOCK);

const ctx = defClass(CLS_CONTEXT, 'MethodContext', object, 5);
const fmt = fromSmallInteger(readIV(ctx, BEHAVIOR_FORMAT));
writeIVNew(ctx, BEHAVIOR_FORMAT,
    toSmallInteger((fmt & 0xffffff) | (FORMAT_VARIABLE_IV << 24)));


const bool = defClass(CLS_BOOLEAN, 'Boolean', object, 0);
const true_ = defClass(CLS_TRUE, 'True', bool, 0);
const false_ = defClass(CLS_FALSE, 'False', bool, 0);

// Create the built-in instances.
mkInstance(true_, undefined, (_) => MA_TRUE);
mkInstance(false_, undefined, (_) => MA_FALSE);

const chr = defClass(CLS_CHARACTER, 'Character', object, 1, 1);
const charTable = mkInstance(read(classTable(CLS_ARRAY)), 256);
writeIV(chr, CLASS_VAR1, charTable);

defClass(CLS_PROCESS, 'Process', object, 4);
defClass(CLS_PROCESS_TABLE, 'ProcessTable', object, 3);
defClass(CLS_ASSOCIATION, 'Association', object, 2);

// Populate the process table - four priorities.
let pt = MA_NIL;
for(let i = 0; i < 4; i++) {
  const newPT = mkInstance(read(classTable(CLS_PROCESS_TABLE)));
  writeIV(newPT, PROCESS_TABLE_NEXT_PRIORITY, pt);
  pt = newPT;
}
vm.processTable = pt;

// Populate the character table.
for (let i = 0; i < 256; i++) {
  const c = basicNew(chr);
  writeIV(c, 0, toSmallInteger(i));
  writeArrayNew(charTable, i, c);
}

// Tricky to initialize these, so capturing it in a function.
export function newContext(method, sender, locals) {
  const ctx = mkInstance(read(classTable(CLS_CONTEXT)), 19);
  writeIV(ctx, CTX_PC, toSmallInteger(0));
  writeIV(ctx, CTX_STACK_INDEX, toSmallInteger(0));
  writeIV(ctx, CTX_METHOD, method);
  writeIV(ctx, CTX_LOCALS, locals);
  writeIV(ctx, CTX_SENDER, sender);
  return ctx;
}


