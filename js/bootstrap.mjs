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
//    a. Knot-tying setup of the innermost kernel of classes (eg. Class)
//    b. Define enough classes for IdentityDictionary to exist.
// 4. tree.mjs       The AA tree used for class & method dictionaries.
// 5. corelib.mjs
//    a. Upgrades the slightly hobo classes from bootstrap.mjs.
//    b. Adds a bunch more built-in classes, like the booleans.


// Slight hack: we can't define the method dictionaries until the superclasses
// have been created, so this starts out as a dummy that returns nil. It gets
// replaced later and the existing dictionaries updated.
export const lateBinding = {
  dictFactory: () => MA_NIL,
  addToClassDict: (sym, cls) => {},
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
export function defClass(cls, name, superclass, opt_instVars, opt_classVars) {
  // First we set up the metaclass, Transparent class in the example.
  // This is an instance of Metaclass.
  const metaclass = mkInstance(CLS_METACLASS);

  // See the diagram above: the new metaclass (Transparent class)'s superclass
  // is the superclass's (Color's) metaclass (Color class).
  // That's the class field of our superclass!
  //
  // Special case for ProtoObject (where superclass is nil). This is the root
  // of the regular object hierachy, ie. ProtoObject has no superclass.
  // However, ProtoObject class's superclass is Class!
  const supermeta = superclass === MA_NIL ? CLS_CLASS : classOf(superclass);
  writeIV(metaclass, BEHAVIOR_SUPERCLASS, supermeta);
  writeIV(metaclass, BEHAVIOR_METHODS, lateBinding.dictFactory());

  const upstreamClassVars = behaviorToInstVars(supermeta);
  instVarsFormatBehavior(metaclass, classVars + (opt_classVars || 0));

  // Add the metaclass to the class table.
  addClassToTable(metaclass);

  // With the metaclass now properly defined, we can make an instance of it:
  // our new class!
  const cls = mkInstance(metaclass);

  // Set its instance variables: name and subclasses.
  // TODO: Set subclasses, now or later.
  const symbol = wrapSymbol(name);
  writeIVNew(cls, CLASS_NAME, name);
  writeIVNew(cls, BEHAVIOR_SUPERCLASS, superclass);
  writeIVNew(cls, BEHAVIOR_METHODS, lateBinding.dictFactory());

  const upstreamInstVars = behaviorToInstVars(superclass);
  instVarsFormatBehavior(metaclass, classVars + (opt_classVars || 0));

  addClassToTable(cls);

  // Finally Metaclass itself has an instance variable to set: thisClass.
  writeIV(metaclass, METACLASS_THIS_CLASS, cls);

  lateBinding.addToClassDict(symbol, cls);
  return cls;
}

// We can't actually call defClass yet!
// It needs to call mkInstance(CLS_METACLASS), which will recursively
// mkInstance(CLS_CLASS_DESCRIPTION), mkInstance(CLS_BEHAVIOR) and
// mkInstace(CLS_OBJECT) which is the base case.
//
// mkInstance needs the superclass and instVars values off Behavior, so we need
// to populate that for those three classes.

// First, the canned superobjects for ProtoObject and Object, since they
// (a) are the root of the hierarchy and (b) have no instance variables, they
// can be shared universally (saving a lot of memory!)
initObject(MA_PROTO_OBJECT_INSTANCE, CLS_PROTO_OBJECT, MA_NIL, 0);
initObject(MA_OBJECT_INSTANCE, CLS_OBJECT, MA_PROTO_OBJECT_INSTANCE, 0);

// Metaclass is itself an instance of Metaclass, so we need to set that up.
// Object -> Behavior -> ClassDescription -> Metaclass
// MA_METAMETA_* are the superobjects of this circular instance.

// Behavior:
initObject(MA_METAMETA_BEHAVIOR, CLS_BEHAVIOR, MA_OBJECT_INSTANCE, IV_BEHAVIOR);
// Method dictionary will be added later, but we need to set the instVars count
// and superclass (Metaclass's superclass is ClassDescription).
writeAt(MA_METAMETA_BEHAVIOR, BEHAVIOR_INST_VARS, wrapSmallInteger(IV_BEHAVIOR));
writeAt(MA_METAMETA_BEHAVIOR, BEHAVIOR_SUPERCLASS, CLS_CLASS_DESCRIPTION);

// ClassDescription
initObject(MA_METAMETA_DESCRIPTION, CLS_CLASS_DESCRIPTION, MA_METAMETA_BEHAVIOR,
    IV_CLASS_DESCRIPTION);

// meta-Metaclass itself.
initObject(MA_METAMETA, CLS_METACLASS, MA_METAMETA_DESCRIPTION, IV_METACLASS);
// This circularly Metaclass's thisClass field is set to Metaclass as well.
// This is circular the other way!
writeAt(MA_METAMETA, METACLASS_THIS_CLASS, CLS_METACLASS);


// Now to actually set up CLS_METACLASS and its parents CLS_BEHAVIOR and
// CLS_CLASS_DESCRIPTION. We half-populate those, with no OBJ_CLASS fields and
// such, since there's nothing to point to. Later defClass calls will fill those
// in, and we're not trying to look up messages!
//
// Instead, we just need to be able to build the superObject chain for
// CLS_METACLASS properly.
const behaviorBehavior = allocObject(IV_BEHAVIOR);
initObject(behaviorBehavior, MA_NIL, MA_OBJECT_INSTANCE, IV_BEHAVIOR);
writeAt(behaviorBehavior, BEHAVIOR_SUPERCLASS, CLS_OBJECT);
writeAt(behaviorBehavior, BEHAVIOR_INST_VARS, wrapSmallInteger(IV_BEHAVIOR));

const behaviorDescription = allocObject(IV_CLASS_DESCRIPTION);
initObject(behaviorDescription, MA_NIL, behaviorBehavior, IV_CLASS_DESCRIPTION);

initObject(CLS_BEHAVIOR, MA_NIL, behaviorDescription, IV_CLASS);
writeAt(CLS_BEHAVIOR, CLASS_NAME, wrapSymbol('Behavior'));


// Now set up the sketch of CLS_CLASS_DESCRIPTION.
const cdBehavior = allocObject(IV_BEHAVIOR);
initObject(cdBehavior, MA_NIL, MA_OBJECT_INSTANCE, IV_BEHAVIOR);
writeAt(cdBehavior, BEHAVIOR_SUPERCLASS, CLS_BEHAVIOR);
writeAt(cdBehavior, BEHAVIOR_INST_VARS, wrapSmallInteger(IV_CLASS_DESCRIPTION));

const cdDescription = allocObject(IV_CLASS_DESCRIPTION);
initObject(cdDescription, MA_NIL, cdBehavior, IV_CLASS_DESCRIPTION);

initObject(CLS_CLASS_DESCRIPTION, MA_NIL, cdDescription, IV_CLASS);
writeAt(CLS_CLASS_DESCRIPTION, CLASS_NAME, wrapSymbol('ClassDescription'));


// And now CLS_METACLASS
const metaBehavior = allocObject(IV_BEHAVIOR);
initObject(metaBehavior, MA_NIL, MA_OBJECT_INSTANCE, IV_BEHAVIOR);
writeAt(metaBehavior, BEHAVIOR_SUPERCLASS, CLS_CLASS_DESCRIPTION);
writeAt(metaBehavior, BEHAVIOR_INST_VARS, wrapSmallInteger(IV_METACLASS));

const metaDescription = allocObject(IV_CLASS_DESCRIPTION);
initObject(metaDescription, MA_NIL, metaBehavior, IV_CLASS_DESCRIPTION);

initObject(CLS_METACLASS, MA_NIL, metaDescription, IV_CLASS);
writeAt(CLS_METACLASS, CLASS_NAME, wrapSymbol('Metaclass'));


// Finally, we need to set up CLS_CLASS.
const classBehavior = allocObject(IV_BEHAVIOR);
initObject(classBehavior, MA_NIL, MA_OBJECT_INSTANCE, IV_BEHAVIOR);
writeAt(classBehavior, BEHAVIOR_SUPERCLASS, CLS_CLASS_DESCRIPTION);
writeAt(classBehavior, BEHAVIOR_INST_VARS, wrapSmallInteger(IV_CLASS));

const classDescription = allocObject(IV_CLASS_DESCRIPTION);
initObject(classDescription, MA_NIL, classBehavior, IV_CLASS_DESCRIPTION);

initObject(CLS_CLASS, MA_NIL, classDescription, IV_CLASS);
writeAt(CLS_CLASS, CLASS_NAME, wrapSymbol('Class'));


// Now we should be able to call mkInstance(Metaclass) successfully.
// And we can call defClass to define the hierarchy necessary to create
// CLS_IDENTITY_DICTIONARY; then we can fix the existing classes' dictionaries.
// ProtoObject -> Object -> Collection -> Dictionary -> IdentityDictionary.

defClass(CLS_PROTO_OBJECT, 'ProtoObject', MA_NIL, 0, 0);

// Sanity checks, since something is wrong.
// ProtoObject (the class) is an instance of ProtoObject class, which has no
// personal instance variables.
console.log('CLS_PROTO_OBJECT size',
    readAt(CLS_PROTO_OBJECT, 0), OBJ_MEMBERS_BASE);

// This is ProtoObject class.
const protoObjectClass = readAt(CLS_PROTO_OBJECT, OBJ_CLASS);
// Its class is Metaclass.
console.log('ProtoObject class class == Metaclass',
    readAt(protoObjectClass, OBJ_CLASS) === CLS_METACLASS);

const protoObjectSuper1 = readAt(CLS_PROTO_OBJECT, OBJ_SUPER);
console.log('ProtoObject is a subclass of Class',
    readAt(protoObjectSuper1, OBJ_CLASS) === CLS_CLASS);
const protoObjectSuper2 = readAt(protoObjectSuper1, OBJ_SUPER);
console.log('Class is a subclass of ClassDescription',
    readAt(protoObjectSuper2, OBJ_CLASS) === CLS_CLASS_DESCRIPTION);
const protoObjectSuper3 = readAt(protoObjectSuper2, OBJ_SUPER);
console.log('ClassDescription is a subclass of Behavior',
    readAt(protoObjectSuper3, OBJ_CLASS) === CLS_BEHAVIOR);

console.log('Which stores the instance variable count (0)',
    readSmallInteger(protoObjectSuper3 + BEHAVIOR_INST_VARS) === 0);

const protoObjectSuper4 = readAt(protoObjectSuper3, OBJ_SUPER);
console.log('Behavior is a subclass of Object, with its default superobj',
    readAt(protoObjectSuper4, OBJ_CLASS) === CLS_OBJECT,
    protoObjectSuper4 === MA_OBJECT_INSTANCE);

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
defClass(CLS_OBJECT, 'Object', CLS_PROTO_OBJECT, 0, 0);

defClass(CLS_BEHAVIOR, 'Behavior', CLS_OBJECT, IV_BEHAVIOR, 0);
defClass(CLS_CLASS_DESCRIPTION, 'ClassDescription', CLS_BEHAVIOR,
    IV_CLASS_DESCRIPTION, 0);
defClass(CLS_CLASS, 'Class', CLS_CLASS_DESCRIPTION, IV_CLASS, 0);
defClass(CLS_METACLASS, 'Metaclass', CLS_CLASS_DESCRIPTION, IV_METACLASS, 0);

defClass(CLS_COLLECTION, 'Collection', CLS_OBJECT, 0, 0);
defClass(CLS_DICTIONARY, 'Dictionary', CLS_COLLECTION, 1, 0);
defClass(CLS_IDENTITY_DICTIONARY, 'IdentityDictionary', CLS_DICTIONARY, 1, 0);

defClass(CLS_AA_NODE, 'AANode', CLS_OBJECT, 5, 0);

// TODO Now that there's proper (meta)classes defined for Metaclass and friends,
// are there impoverished early instances out there? My brain hurts, but I need
// to think this through. Probably the system will bootstrap correctly and I can
// check it out with tests like sending messages to Metaclass and Behavior and
// things later on.

