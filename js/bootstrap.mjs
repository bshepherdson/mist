// Contains hand-rolled classes and methods necessary to bootstrap the system.
import {
  ASCII_TABLE,
  BEHAVIOR_SUPERCLASS, BEHAVIOR_METHODS, BEHAVIOR_FORMAT,
  MA_METAMETA,
  METACLASS_THIS_CLASS,
  CLASS_NAME, CLASS_SUBCLASSES,
  CLS_PROTO_OBJECT, CLS_OBJECT, CLS_UNDEFINED_OBJECT,
  CLS_CLASS, CLS_CLASS_DESCRIPTION, CLS_BEHAVIOR, CLS_METACLASS,
  CLS_COLLECTION, CLS_DICTIONARY, CLS_IDENTITY_DICTIONARY, CLS_AA_NODE,
  IV_BEHAVIOR, IV_CLASS_DESCRIPTION, IV_CLASS, IV_METACLASS,
  MA_NIL,
  addClassToTable, classOf, initObject, mkInstance,
  readIV, writeIV, writeIVNew,
  fromSmallInteger, toSmallInteger, wrapSymbol,
} from './memory.mjs';

// Bootstrapping proceeds in several phases:
// 1. vm.mjs         VM's initial setup of the registers.
// 2. memory.mjs     Memory created, a few pointers in memory updated.
// 3. bootstrap.mjs
//    a.                 Knot-tying setup of the innermost kernel of classes
//    b.                 Define enough classes for IdentityDictionary to exist.
// 4. dict.mjs       The hobo dictionary used to bootstrap into Smalltalk.
// 5. corelib.mjs
//    a.                 Upgrades the slightly hobo classes from bootstrap.mjs.
//    b.                 Adds a bunch more built-in classes, like the booleans.
// 6. driver.mjs     Starts reading the input from the Smalltalk files.


// Slight hack: we can't define the method dictionaries until the superclasses
// have been created, so this starts out as a dummy that returns nil. It gets
// replaced later and the existing dictionaries updated.
export const impoverishedClasses = {};
export const lateBinding = {
  dictFactory: () => MA_NIL,
  addToClassDict: (sym, cls) => {},
  symbolize: (name) => MA_NIL,
  register: (cls, name) => {
    impoverishedClasses[name] = cls;
  },
};

// Class construction
// ==================
// The 10 rules of the object system:
//  1. Everything is an object.
//  2. Every object is an instance of a class.
//  3. Every class has a superclass.
//  4. Everything happens by sending messages.
//  5. Method lookup follows the inheritance (superclass) chain.
//  ---------
//  6. Every class is an instance of a metaclass.
//  7. The metaclass hierarchy parallels the class hierarchy.
//  8. Every metaclass inherits from Class and Behavior.
//  9. Every metaclass is an instance of Metaclass.
// 10. The metaclass of Metaclass is an instance of Metaclass.
//
// The hierarchy looks like this:
//
//  /------------------------ Behavior
//  |                             ^   (subclass-of)
//  |                             |
//  |                       ClassDescription   /----\
//  |                             ^        ^   |    v
//  |                             |         \  |    v
//  |                           Class       Metaclass
//  |                             ^             ^
//  |         (instance-of)       |             ^  (instance-of)
//  |  ProtoObject ----->> ProtoObject class ---|
//  |    ^                        ^             |
//  v    |                        |             |
//  Object   ------------>>  Object class ------|
//    ^                           ^             |
//    |                           |             |
//  Color     ------------>>  Color class ------|
//    ^                           ^             |
//    |                           |             |
// Transparent  ---->>  Transparent class ------|
//  ^
//  ^
//  |
// transparentBlue
//
// So when we're defining a new class, say Transparent:
// - The superclass field on Behavior is set to Color.
// - The superclass field on meta-Behavior is Color class.
// - Then the new class is an instance of the metaclass.
//   - ie. The class field of Transparent is Transparent class!
export function defClass(clsIndex, name, superclass, opt_instVars, opt_classVars) {
  // First we set up the metaclass, Transparent class in the example.
  // This is an instance of Metaclass.
  const metaclass = mkInstance(read(classTable(CLS_METACLASS)));

  // See the diagram above: the new metaclass (Transparent class)'s superclass
  // is the superclass's (Color's) metaclass (Color class).
  // That's the class field of our superclass!
  //
  // Special case for ProtoObject (where superclass is nil). This is the root
  // of the regular object hierachy, ie. ProtoObject has no superclass.
  const supermeta = superclass === MA_NIL ?
      // However, ProtoObject class's superclass is Class!
      read(classTable(CLS_CLASS)) :
      // Otherwise, look up the superclass and get *its* class.
      classOf(superclass);

  // All the writes here are to freshly allocated classes, so we can safely use
  // the *New forms.
  writeIVNew(metaclass, BEHAVIOR_SUPERCLASS, supermeta);
  writeIVNew(metaclass, BEHAVIOR_METHODS, lateBinding.dictFactory());

  const upstreamClassVars = behaviorToInstVars(supermeta);
  const metaFormat = fixedInstVarsFormat(classVars + (opt_classVars || 0));
  writeIVNew(metaclass, BEHAVIOR_FORMAT, metaFormat);

  // Add the metaclass to the class table.
  addClassToTable(metaclass);

  // With the metaclass now properly defined, we can make an instance of it:
  // our new class!
  const cls = mkInstance(metaclass);

  // Set its instance variables: name and subclasses.
  // TODO: Set subclasses, now or later.
  const symbol = lateBinding.symbolize(name);
  writeIVNew(cls, CLASS_NAME, name);
  writeIVNew(cls, BEHAVIOR_SUPERCLASS, superclass);
  writeIVNew(cls, BEHAVIOR_METHODS, lateBinding.dictFactory());

  const upstreamInstVars = behaviorToInstVars(superclass);
  const format = fixedInstVarsFormat(instVars + (opt_instVars || 0));
  writeIVNew(cls, BEHAVIOR_FORMAT, format);

  write(classTable(clsIndex), cls);
  checkOldToNew(classTable(clsIndex), cls);

  // Finally Metaclass itself has an instance variable to set: thisClass.
  writeIVNew(metaclass, METACLASS_THIS_CLASS, cls);

  lateBinding.addToClassDict(symbol, cls);
  lateBinding.register(cls);
  return cls;
}

// We can't actually call defClass yet!
// It needs to call mkInstance(CLS_METACLASS), which needs the format field of
// Behavior.
// It also needs to wrapSymbol for the class name, which needs CLS_SYMBOL to be
// populated similarly.
// So we populate these few specific classes by hand.
// allocObject (and initObject underneath) only need class hashes, ie. raw
// CLS_FOO values.

// Remember that `Metaclass class class == Metaclass`, but `Metaclass class` is
// a regular class with a name and such.
const classTotalIVs = IV_BEHAVIOR + IV_CLASS_DESCRIPTION + IV_CLASS;
const metaclass = allocObject(0, classTotalIVs, 0, alloc);
writeIV(metaclass, BEHAVIOR_FORMAT, fixedInstVarsFormat(classTotalIVs));
write(classTable(CLS_METACLASS), metaclass);
checkOldToNew(classTable(CLS_METACLASS), metaclass);


const symbol = allocObject(0, classTotalIVs, 0, alloc);
writeIV(symbol, BEHAVIOR_FORMAT, fixedInstVarsFormat(classTotalIVs));
write(classTable(CLS_SYMBOL), symbol);
checkOldToNew(classTable(CLS_SYMBOL), symbol);


// Now I think we can start declaring regular classes!
const protoObject = defClass(CLS_PROTO_OBJECT, 'ProtoObject', null, 0);
const object = defClass(CLS_OBJECT, 'Object', protoObject, 0);
const behavior = defClass(CLS_BEHAVIOR, 'Behavior', object, IV_BEHAVIOR);
const classDesc = defClass(CLS_CLASS_DESCRIPTION, 'ClassDescription',
    behavior, IV_CLASS_DESCRIPTION);
const class_ = defClass(CLS_CLASS, 'Class', classDesc, IV_CLASS);
const metaclass = defClass(CLS_METACLASS, 'Metaclass', classDesc, IV_METACLASS);

const nilObj = defClass(CLS_UNDEFINED_OBJECT, 'UndefinedObject', object, 0);
mkInstance(nilObj, undefined, (_) => MA_NIL);


// TODO This could probably be sped up by inlining, but it adds a lot of
// complexity. Nested (word)arrays will probably do.
defClass(CLS_COMPILED_METHOD, 'CompiledMethod', IV_METHOD);
defClass(CLS_BLOCK_CLOSURE, 'BlockClosure', 4);

// Collections, far enough for Symbol and String.
const collection = defClass(CLS_COLLECTION, 'Collection', object, 0);
const seqColl = defClass(CLS_SEQUENCEABLE_COLLECTION, 'SequenceableCollection',
    collection, 0);
const arrColl = defClass(CLS_ARRAYED_COLLECTION, 'ArrayedCollection', seqColl, 0);

const arr = defClass(CLS_ARRAY, 'Array', arrColl, 0);
// Fix the format. It should be FORMAT_VARIABLE.
writeIVNew(arr, BEHAVIOR_FORMAT, toSmallInteger(FORMAT_VARIABLE << 24));

const str = defClass(CLS_STRING, 'String', arrColl, 0);
const sym = defClass(CLS_SYMBOL, 'Symbol', str, 0, 1); // 1 class var: symbol dictionary.
// Adjust the format! It's FORMAT_WORDS_EVEN and the allocation code handles the
// length.
writeIVNew(str, BEHAVIOR_FORMAT, toSmallInteger(FORMAT_WORDS_EVEN << 24));
writeIVNew(sym, BEHAVIOR_FORMAT, toSmallInteger(FORMAT_WORDS_EVEN << 24));

const mag = defClass(CLS_MAGNITUDE, 'Magnitude', object, 0);
const num = defClass(CLS_NUMBER, 'Number', mag, 0);
const integer = defClass(CLS_INTEGER, 'Integer', num, 0);
const smallInteger = defClass(CLS_SMALL_INTEGER, 'SmallInteger', integer, 0);


const hashed = defClass(CLS_HASHED_COLLECTION, 'HashedCollection', collection, 2);
const dict = defClass(CLS_DICTIONARY, 'Dictionary', hashed, 0);
const idDict = defClass(CLS_IDENTITY_DICTIONARY, 'IdentitiyDictionary', dict, 0);


// Rule 10: The metaclass of Metaclass is an instance of Metaclass.
// TODO I'm not sure this is done?

// We need the hobo implementation of "dictionaries" in dict.mjs. They're
// not really dictionaries, just arrays with [k1, v1, k2, v2, ...] in arbitrary
// order. They should get replaced with real dictionaries after we've started
// loading Smalltalk and defined the real thing.
