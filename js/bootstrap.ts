// Contains hand-rolled classes and methods necessary to bootstrap the system.
import {vm} from './vm';
import {
  IdentityHash, ptr,
  BEHAVIOR_SUPERCLASS, BEHAVIOR_FORMAT, BEHAVIOR_METHODS,
  Format,
  MASK_CLASS_INDEX,
  METACLASS_THIS_CLASS,
  CLASS_NAME,
  CLS_PROTO_OBJECT, CLS_OBJECT, CLS_UNDEFINED_OBJECT,
  CLS_CLASS, CLS_CLASS_DESCRIPTION, CLS_BEHAVIOR, CLS_METACLASS,
  CLS_COMPILED_METHOD, CLS_BLOCK_CLOSURE, CLS_LINKED_LIST,
  CLS_COLLECTION, CLS_SEQUENCEABLE_COLLECTION, CLS_ARRAYED_COLLECTION,
  CLS_HASHED_COLLECTION, CLS_DICTIONARY, CLS_IDENTITY_DICTIONARY,
  CLS_ASSOCIATION, CLS_ARRAY, CLS_WORD_ARRAY, CLS_SYMBOL, CLS_STRING,
  CLS_MAGNITUDE, CLS_NUMBER, CLS_INTEGER, CLS_SMALL_INTEGER,
  IV_BEHAVIOR, IV_CLASS_DESCRIPTION, IV_CLASS, IV_METACLASS, IV_METHOD,
  CLASS_DESCRIPTION_INSTANCE_VARIABLES,
  MA_NIL,
  addClassToTable, alloc, allocObject,
  classOf, classTable, mkInstance,
  behaviorToInstVars, fixedInstVarsFormat,
  read, readIV, write, writeIV, writeIVNew,
  fromSmallInteger, toSmallInteger, gcTemps, gcRelease, seq,
} from './memory';

// Bootstrapping proceeds in several phases:
// 1. vm.mjs         VM's initial setup of the registers.
// 2. memory.mjs     Memory created, a few pointers in memory updated.
// 3. bootstrap.mjs
//    a.               Knot-tying setup of the innermost kernel of classes
//    b.               Define enough classes for IdentityDictionary to exist.
// 4. dict.mjs       The hobo dictionary used to bootstrap into Smalltalk.
// 5. corelib.mjs
//    a.               Upgrades the slightly hobo classes from bootstrap.mjs.
//    b.               Adds a bunch more built-in classes, like the booleans.
// 6. driver.mjs     Starts reading the input from the Smalltalk files.


// Slight hack: we can't define the method dictionaries until the superclasses
// have been created, so this starts out as a dummy that returns nil. It gets
// replaced later and the existing dictionaries updated.
interface ImpoverishedClass {
  cls: ptr;
  vars?: string[];
}

export const impoverishedClasses: {[name: string]: ImpoverishedClass} = {};

export const lateBinding = {
  dictFactory: () => MA_NIL,
  addToClassDict: (sym: ptr, cls: ptr) => {},
  symbolize: (name: string) => MA_NIL,
  varArray: (name: ptr|string, names: string[]): ptr => {
    if (typeof name === 'string') {
      impoverishedClasses[name].vars = names;
    }
    return MA_NIL;
  },
  register: (cls: ptr, name: ptr|string) => {
    if (typeof name === 'string') {
      impoverishedClasses[name] = {cls};
    }
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
export function defClass(clsIndex: IdentityHash, name: string|ptr,
    superclass: ptr, instVars: string[]): ptr {
  const [v_metaclass, v_supermeta, v_superclass, v_name, v_symbol, v_class] =
      seq(6);
  const ptrs = gcTemps(6);
  ptrs[v_superclass] = superclass;
  ptrs[v_name] = typeof name === 'string' ? MA_NIL : name;
  // First we set up the metaclass, Transparent class in the example.
  // This is an instance of Metaclass.
  ptrs[v_metaclass] = mkInstance(read(classTable(CLS_METACLASS)));

  // See the diagram above: the new metaclass (Transparent class)'s superclass
  // is the superclass's (Color's) metaclass (Color class).
  // That's the class field of our superclass!
  //
  // Special case for ProtoObject (where superclass is nil). This is the root
  // of the regular object hierachy, ie. ProtoObject has no superclass.
  ptrs[v_supermeta] = ptrs[v_superclass] === MA_NIL ?
      // However, ProtoObject class's superclass is Class!
      read(classTable(CLS_CLASS)) :
      // Otherwise, look up the superclass and get *its* class.
      classOf(ptrs[v_superclass]);

  writeIV(ptrs[v_metaclass], BEHAVIOR_SUPERCLASS, ptrs[v_supermeta]);
  writeIV(ptrs[v_metaclass], BEHAVIOR_METHODS, lateBinding.dictFactory());

  const classInstVars = mkInstance

  const upstreamClassVars = behaviorToInstVars(ptrs[v_supermeta]);
  const metaFormat =
      fixedInstVarsFormat(upstreamClassVars + 0 /*classInstVars*/);
  writeIV(ptrs[v_metaclass], BEHAVIOR_FORMAT, toSmallInteger(metaFormat));

  // Add the metaclass to the class table.
  addClassToTable(ptrs[v_metaclass]);

  // With the metaclass now properly defined, we can make an instance of it:
  // our new class!
  ptrs[v_class] = mkInstance(ptrs[v_metaclass]);

  // Set its instance variables: name and subclasses.
  // TODO: Set subclasses, now or later.
  // Name might be a string (bootstrapping) or a pointer to a Symbol already.
  ptrs[v_symbol] = typeof name === 'string' ?
      lateBinding.symbolize(name) : ptrs[v_name];
  writeIV(ptrs[v_class], CLASS_NAME, ptrs[v_symbol]);
  writeIV(ptrs[v_class], BEHAVIOR_SUPERCLASS, ptrs[v_superclass]);
  writeIV(ptrs[v_class], BEHAVIOR_METHODS, lateBinding.dictFactory());

  const upstreamInstVars = behaviorToInstVars(ptrs[v_superclass]);
  const format = fixedInstVarsFormat(upstreamInstVars + instVars.length);
  writeIVNew(ptrs[v_class], BEHAVIOR_FORMAT, toSmallInteger(format));

  addClassToTable(ptrs[v_class], clsIndex);

  // Finally Metaclass itself has an instance variable to set: thisClass.
  writeIV(ptrs[v_metaclass], METACLASS_THIS_CLASS, ptrs[v_class]);

  lateBinding.addToClassDict(ptrs[v_symbol], ptrs[v_class]);
  lateBinding.register(ptrs[v_class], name);
  const p: ptr = lateBinding.varArray(name, instVars);
  writeIVNew(ptrs[v_class], CLASS_DESCRIPTION_INSTANCE_VARIABLES, p);

  const cls = ptrs[v_class];
  gcRelease(ptrs);
  return cls;
}

// We can't actually call defClass yet!
// It needs to call mkInstance(CLS_METACLASS), which needs the format field of
// Behavior.
// allocObject (and initObject underneath) only need class hashes, ie. raw
// CLS_FOO values.
// A few things (such as adding to the class dictionary and defining symbols)
// are late-bound so they can be filled in later.

// (Proto)Object instances have no instance variables. ProtoObject (the Class)
// has 5 (inherited from Behavior and Class, its superclasses) and
// ProtoObject class has 4 (it's an instance of Metaclass).
//
// The minimum we need to start constructing regular classes is:
// ProtoObject, Object,
// Behavior, ClassDescription, Class,
// Metaclass, and then the metaclasses for all the above.
//
// We'll have to create these in an impoverished way, with fields blanked out,
// and then populate them.
const classTotalIVs = IV_BEHAVIOR + IV_CLASS_DESCRIPTION + IV_CLASS;
const metaclassTotalIVs = IV_BEHAVIOR + IV_CLASS_DESCRIPTION + IV_METACLASS;

// That gives how many instance variables *this class's instances have!
// Classes always have classTotalIVs.
// This doesn't populate the name (no symbols) or the subclasses or methods
// lists.
function bootstrapClass(clsHash: IdentityHash, itsClassHash: IdentityHash,
    superclass: ptr, instVars: number): ptr {
  const cls = allocObject(itsClassHash, classTotalIVs, 0, alloc);
  // The format describes the instances of this new class: fixed with instVars.
  writeIVNew(cls, BEHAVIOR_FORMAT,
      toSmallInteger(fixedInstVarsFormat(instVars)));
  writeIV(cls, BEHAVIOR_SUPERCLASS, superclass);
  write(classTable(clsHash), cls);

  // The identityHash of a class is its own class index.
  const hdr1 = read(cls);
  write(cls, (hdr1 & ~MASK_CLASS_INDEX) | clsHash);

  return cls;
}

const protoObject = bootstrapClass(CLS_PROTO_OBJECT, 0, MA_NIL, 0);
const object = bootstrapClass(CLS_OBJECT, 0, protoObject, 0);
const behavior = bootstrapClass(CLS_BEHAVIOR, 0, object, IV_BEHAVIOR);
const classDesc = bootstrapClass(CLS_CLASS_DESCRIPTION, 0, behavior,
    IV_BEHAVIOR + IV_CLASS_DESCRIPTION);
const class_ = bootstrapClass(CLS_CLASS, 0, classDesc, classTotalIVs);
// Metaclass is itself a class, and its instances are metaclasses, so its
// format field holds metaclassTotalIVs.
const metaclass = bootstrapClass(CLS_METACLASS, 0, classDesc,
    metaclassTotalIVs);

// Register all of these classes as "impoverished" and in need of polishing up.
lateBinding.register(protoObject, 'ProtoObject');
lateBinding.register(object, 'Object');
lateBinding.register(behavior, 'Behavior');
lateBinding.register(classDesc, 'ClassDescription');
lateBinding.register(class_, 'Class');
lateBinding.register(metaclass, 'Metaclass');

// Now those bootstrapping classes all have 0 in the class field, which needs
// fixing. We create the metaclasses at nameless slots in the class table,
// and get the number back.
function bootstrapMetaclass(metaclassFor: IdentityHash, superclass: ptr): ptr {
  const meta = allocObject(CLS_METACLASS, metaclassTotalIVs, 0, alloc);
  writeIVNew(meta, BEHAVIOR_FORMAT,
      toSmallInteger(fixedInstVarsFormat(classTotalIVs)));
  writeIV(meta, BEHAVIOR_SUPERCLASS, superclass);
  const hash = addClassToTable(meta);

  // Write that into the target class's header.
  const cls = read(classTable(metaclassFor));
  const hdr2 = read(cls + 2);
  write(cls + 2, (hdr2 & ~MASK_CLASS_INDEX) | hash);

  // Metaclasses are themselves classes, so they need their identity hashes to
  // be their own class indexes.
  const hdr1 = read(meta);
  write(meta, (hdr1 & ~MASK_CLASS_INDEX) | hash);

  writeIV(meta, METACLASS_THIS_CLASS, cls);
  return meta;
}

const protoObjectClass = bootstrapMetaclass(CLS_PROTO_OBJECT,
    read(classTable(CLS_CLASS)));
const objectClass = bootstrapMetaclass(CLS_OBJECT, protoObjectClass);
const behaviorClass = bootstrapMetaclass(CLS_BEHAVIOR, objectClass);
const classDescClass = bootstrapMetaclass(CLS_CLASS_DESCRIPTION, behaviorClass);
const classClass = bootstrapMetaclass(CLS_CLASS, classDescClass);
const metaclassClass = bootstrapMetaclass(CLS_METACLASS, classDescClass);



// Now I think we can start declaring regular classes!
const nilObj = defClass(CLS_UNDEFINED_OBJECT, 'UndefinedObject', object, []);
mkInstance(nilObj, undefined /* arrayLength */, (_) => MA_NIL);


// TODO This could probably be sped up by inlining, but it adds a lot of
// complexity. Nested (word)arrays will probably do.
defClass(CLS_COMPILED_METHOD, 'CompiledMethod', object,
    ['bytecode', 'literals', 'selector', 'class', 'argc', 'locals']);

// Collections, far enough for Symbol and String.
const collection = defClass(CLS_COLLECTION, 'Collection', object, []);
const seqColl = defClass(CLS_SEQUENCEABLE_COLLECTION, 'SequenceableCollection',
    collection, []);
const arrColl =
    defClass(CLS_ARRAYED_COLLECTION, 'ArrayedCollection', seqColl, []);
const linkedList = defClass(CLS_LINKED_LIST, 'LinkedList', seqColl,
    ['head', 'tail']);

const arr = defClass(CLS_ARRAY, 'Array', arrColl, []);
// Fix the format. It should be Format.VARIABLE.
writeIVNew(arr, BEHAVIOR_FORMAT, toSmallInteger(Format.VARIABLE << 24));

const wordArr = defClass(CLS_WORD_ARRAY, 'WordArray', arrColl, []);
const str = defClass(CLS_STRING, 'String', wordArr, []);
const sym = defClass(CLS_SYMBOL, 'Symbol', str, []);
// Adjust the format! It's Format.WORDS_EVEN and the allocation code handles the
// length.
writeIVNew(wordArr, BEHAVIOR_FORMAT, toSmallInteger(Format.WORDS_EVEN << 24));
writeIVNew(str, BEHAVIOR_FORMAT, toSmallInteger(Format.WORDS_EVEN << 24));
writeIVNew(sym, BEHAVIOR_FORMAT, toSmallInteger(Format.WORDS_EVEN << 24));

const mag = defClass(CLS_MAGNITUDE, 'Magnitude', object, []);
const num = defClass(CLS_NUMBER, 'Number', mag, []);
const integer = defClass(CLS_INTEGER, 'Integer', num, []);
const smallInteger = defClass(CLS_SMALL_INTEGER, 'SmallInteger', integer, []);


const hashed = defClass(CLS_HASHED_COLLECTION, 'HashedCollection', collection,
      ['array', 'tally']);
const dict = defClass(CLS_DICTIONARY, 'Dictionary', hashed, []);
const idDict = defClass(CLS_IDENTITY_DICTIONARY, 'IdentityDictionary', dict, []);
defClass(CLS_ASSOCIATION, 'Association', object, ['key', 'value']);

// We need the hobo implementation of "dictionaries" in dict.mjs. They're
// not really dictionaries, just arrays with [k1, v1, k2, v2, ...] in arbitrary
// order. They should get replaced with real dictionaries after we've started
// loading Smalltalk and defined the real thing.
