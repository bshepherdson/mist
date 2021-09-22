// Contains hand-rolled classes and methods necessary to bootstrap the system.
import {
  BEHAVIOR_SUPERCLASS, BEHAVIOR_METHODS, BEHAVIOR_INST_VARS,
  METACLASS_THIS_CLASS,
  CLASS_NAME, CLASS_SUBCLASSES,
  CLS_PROTO_OBJECT, CLS_OBJECT, CLS_UNDEFINED_OBJECT,
  CLS_CLASS, CLS_CLASS_DESCRIPTION, CLS_BEHAVIOR, CLS_METACLASS,
  CLS_BOOLEAN, CLS_TRUE, CLS_FALSE,
  CLS_COLLECTION, CLS_DICTIONARY, CLS_IDENTITY_DICTIONARY, CLS_AA_NODE,
  IV_BEHAVIOR, IV_CLASS_DESCRIPTION, IV_CLASS, IV_METACLASS,
  MA_PROTO_OBJECT_INSTANCE, MA_OBJECT_INSTANCE, MA_NIL,
  MA_BOOLEAN, MA_TRUE, MA_FALSE,
  classFriends, initObject, mkInstance, writeAt, wrapSmallInteger, wrapSymbol,
} from './memory.mjs';

initObject(MA_PROTO_OBJECT_INSTANCE, CLS_PROTO_OBJECT, MA_NIL, 0);
initObject(MA_OBJECT_INSTANCE, CLS_OBJECT, MA_PROTO_OBJECT_INSTANCE, 0);


// Slight hack: we can't define the method dictionaries until the superclasses
// have been created, so this starts out as a dummy that returns nil. It gets
// replaced later and the existing dictionaries updated.
let dictFactory = () => MA_NIL;

// Remember the 10 rules of the object system:
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


// Given a class pointer, (eg. CLS_MY_CLASS), its name as a platform string, the
// pointer to its superclass (CLS_MY_SUPER_CLASS) and the inst vars and class
// vars counts, defines the class and its metaclass.
function defClass(cls, name, superclass, opt_instVars, opt_classVars) {
  const {
    classDescription,
    behavior,
    metaclass,
    metaClassDescription,
    metaBehavior,
  } = classFriends(cls);

  // First lets set up the metaclass's superobject chain.
  initObject(metaBehavior, CLS_BEHAVIOR, MA_OBJECT_INSTANCE, IV_BEHAVIOR);

  // And its instance variables: superclass and methodDict.
  // The superclass of the Metaclass is the metaclass of my own superclass.
  const superFriends = classFriends(superclass);
  writeAt(metaBehavior, BEHAVIOR_SUPERCLASS, superFriends.metaclass);
  writeAt(metaBehavior, BEHAVIOR_METHODS, dictFactory());
  writeAt(metaBehavior, BEHAVIOR_INST_VARS, wrapSmallInteger(opt_classVars || 0));

  // Now the meta ClassDescription.
  initObject(metaClassDescription, CLS_CLASS_DESCRIPTION, metaBehavior, 0);

  // And then the metaclass and its instance variables.
  initObject(metaclass, CLS_METACLASS, metaClassDescription, IV_METACLASS);
  writeAt(metaclass, METACLASS_THIS_CLASS, cls);


  // Now the Behavior for the class.
  initObject(behavior, CLS_BEHAVIOR, MA_OBJECT_INSTANCE, IV_BEHAVIOR);
  writeAt(behavior, BEHAVIOR_SUPERCLASS, superclass);
  writeAt(behavior, BEHAVIOR_METHODS, dictFactory());
  writeAt(behavior, BEHAVIOR_INST_VARS, wrapSmallInteger(opt_instVars || 0));

  // ClassDescription next.
  initObject(classDescription, CLS_CLASS_DESCRIPTION, behavior, 0);

  // And finally the real class.
  initObject(cls, CLS_CLASS, classDescription, IV_CLASS);
  writeAt(cls, CLASS_NAME, wrapSymbol(name));
  writeAt(cls, CLASS_SUBCLASSES, MA_NIL); // TODO Maintain this set.
  // Maybe that can be handled outside the bootstrap, and fixed up after
  // bootstrapping is done?

  return cls;
}

// Now we can call defClass to define the hierarchy necessary to create
// CLS_IDENTITY_DICTIONARY; then we can fix the existing classes' dictionaries.
// ProtoObject -> Object -> Collection -> Dictionary -> IdentityDictionary.

defClass(CLS_PROTO_OBJECT, 'ProtoObject', MA_NIL, 0, 0);
defClass(CLS_OBJECT, 'Object', CLS_PROTO_OBJECT, 0, 0);

defClass(CLS_BEHAVIOR, 'Behavior', CLS_OBJECT, IV_BEHAVIOR, 0);
defClass(CLS_CLASS_DESCRIPTION, 'ClassDescription', CLS_BEHAVIOR,
    IV_CLASS_DESCRIPTION, 0);
defClass(CLS_CLASS, 'Class', CLS_CLASS_DESCRIPTION, IV_CLASS, 0);
defClass(CLS_METACLASS, 'Metaclass', CLS_CLASS_DESCRIPTION, IV_METACLASS, 0);

defClass(CLS_COLLECTION, 'Collection', CLS_OBJECT, 0, 0);
defClass(CLS_DICTIONARY, 'Dictionary', CLS_COLLECTION, 1, 0);
defClass(CLS_IDENTITY_DICTIONARY, 'IdentityDictionary', CLS_DICTIONARY, 1, 0);

// Now fix the dictionary factory.
dictFactory = () => mkInstance(CLS_IDENTITY_DICTIONARY);

// And update the nil method dictioaries we just created for those classes (and
// their metaclasses).
const impoverishedClasses = [
  CLS_PROTO_OBJECT, CLS_OBJECT,
  CLS_BEHAVIOR, CLS_CLASS_DESCRIPTION, CLS_CLASS, CLS_METACLASS,
  CLS_COLLECTION, CLS_DICTIONARY, CLS_IDENTITY_DICTIONARY,
];

for (const cls of impoverishedClasses) {
  const {behavior, metaBehavior} = classFriends(cls);
  writeAt(behavior, BEHAVIOR_METHODS, dictFactory());
  writeAt(metaBehavior, BEHAVIOR_METHODS, dictFactory());
  // TODO These need to be added as GC roots!
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

defClass(CLS_AA_NODE, 'AANode', CLS_OBJECT, 5, 0);

