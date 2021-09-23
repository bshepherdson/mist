// A later stage of bootstrapping, which runs after the initial bootstrap, then
// the AA tree functionality.

import {defClass, lateBinding} from './bootstrap.mjs';
import {
  CLS_BOOLEAN, CLS_TRUE, CLS_FALSE,
  MA_BOOLEAN, MA_TRUE, MA_FALSE,
  mkInstance,
} from './memory.mjs';
import {insert} from './tree.mjs';

lateBinding.dictFactory = () => mkInstance(CLS_IDENTITY_DICTIONARY);

// And the class dictionary appender.
lateBinding.addToClassDict = (sym, cls) => {
  const dict = read(MA_CLASS_DICT);
  write(MA_CLASS_DICT, insert(dict, sym, cls));
};

// And update the nil method dictioaries we just created for those classes (and
// their metaclasses).
const impoverishedClasses = [
  CLS_PROTO_OBJECT, CLS_OBJECT,
  CLS_BEHAVIOR, CLS_CLASS_DESCRIPTION, CLS_CLASS, CLS_METACLASS,
  CLS_COLLECTION, CLS_DICTIONARY, CLS_IDENTITY_DICTIONARY,
  CLS_AA_NODE,
];

for (const cls of impoverishedClasses) {
  const {behavior, metaBehavior} = classFriends(cls);
  writeAt(behavior, BEHAVIOR_METHODS, dictFactory());
  writeAt(metaBehavior, BEHAVIOR_METHODS, dictFactory());
  // TODO These need to be added as GC roots!

  lateBinding.addToClassDict(readAt(cls, CLS_NAME), cls);
}


// Now we can call defClass() and get properly constructed classes.
defClass(CLS_BOOLEAN, 'Boolean', CLS_OBJECT);
defClass(CLS_TRUE, 'True', CLS_BOOLEAN);
defClass(CLS_FALSE, 'False', CLS_BOOLEAN);

defClass(CLS_UNDEFINED_OBJECT, 'UndefinedObject', CLS_OBJECT);

initObject(MA_NIL, CLS_UNDEFINED_OBJECT, MA_OBJECT_INSTANCE, 0);
initObject(MA_BOOLEAN, CLS_BOOLEAN, MA_OBJECT_INSTANCE, 0);
initObject(MA_TRUE, CLS_TRUE, MA_BOOLEAN, 0);
initObject(MA_FALSE, CLS_FALSE, MA_BOOLEAN, 0);


defClass(CLS_MAGNITUDE, 'Magnitude', CLS_OBJECT, 0, 0);
defClass(CLS_CHARACTER, 'Character', CLS_MAGNITUDE, 1, 0);
defClass(CLS_NUMBER, 'Number', CLS_MAGNITUDE, 0, 0);
defClass(CLS_INTEGER, 'Integer', CLS_NUMBER, 0, 0);
defClass(CLS_SMALL_INTEGER, 'SmallInteger', CLS_NUMBER, 1, 0);

defClass(CLS_COMPILED_METHOD, 'CompiledMethod', CLS_OBJECT, 6, 0);
defClass(CLS_BLOCK_CLOSURE, 'BlockClosure', CLS_OBJECT, 4, 0);
defClass(CLS_CONTEXT, 'MethodContext', CLS_OBJECT, 24, 0);


// Populate the character table.
for (let i = 0; i < 256; i++) {
  ASCII_TABLE[i] = mkInstance(CLS_CHARACTER);
  writeAt(ASCII_TABLE[i], CHAR_VALUE, wrapSmallInteger(i));
}

