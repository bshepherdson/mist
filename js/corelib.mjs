// A later stage of bootstrapping, which runs after the initial bootstrap, then
// the AA tree functionality.

import {defClass, impoverishedClasses, lateBinding} from './bootstrap.mjs';
import {insert, mkDict} from './dict.mjs';
import {
  CLS_BOOLEAN, CLS_TRUE, CLS_FALSE,
  MA_BOOLEAN, MA_TRUE, MA_FALSE,
  mkInstance,
} from './memory.mjs';
import {insert} from './tree.mjs';

lateBinding.dictFactory = () => mkDict();

// And the class dictionary appender.

lateBinding.addToClassDict = (sym, cls) => {
  const dict = read(MA_CLASS_DICT);
  insert(dict, sym, cls);
};

// And update the nil method dictioaries we just created for those classes (and
// their metaclasses).

for (const name of Object.keys(impoverishedClasses)) {
  const cls = impoverishedClasses[name];
  writeIV(cls, BEHAVIOR_METHODS, mkDict());
  const metaclass = classOf(cls);
  writeIV(metaclass, BEHAVIOR_METHODS, lateBinding.dictFactory());
  const symbol = wrapSymbol(name);
  writeIV(cls, CLASS_NAME);
  lateBinding.addToClassDict(symbol, cls);
}

defClass(CLS_BLOCK_CLOSURE, 'BlockClosure', IV_BLOCK);

const ctx = defClass(CLS_CONTEXT, 'MethodContext', 5);
const fmt = fromSmallInteger(readIV(ctx, BEHAVIOR_FORMAT));
writeIVNew(ctx, BEHAVIOR_FORMAT,
    toSmallInteger((fmt & 0xffffff) | (FORMAT_VARIABLE_IV << 24)));


const bool = defClass(CLS_BOOLEAN, 'Boolean', object, 0);
const true_ = defClass(CLS_TRUE, 'True', bool, 0);
const false_ = defClass(CLS_FALSE, 'False', bool, 0);

// Create the built-in instances.
mkInstance(true_, undefined, (_) => MA_TRUE);
mkInstance(false_, undefined, (_) => MA_FALSE);

const chr = defClass(CLS_CHARACTER, 'Character', 1, 1);
const charTable = mkInstance(read(classTable(CLS_ARRAY)), 256);
writeIV(chr, IV_BEHAVIOR + IV_CLASS_DESCRIPTION + IV_CLASS, charTable);

// Populate the character table.
for (let i = 0; i < 256; i++) {
  const c = basicNew(chr);
  writeIV(c, 0, toSmallInteger(i));
  writeArrayNew(charTable, i, c);
}

