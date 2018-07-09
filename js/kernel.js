// Core slices of Javascript that are needed to bootstrap the VM.
//
// Fundamental design: Smalltalk objects are JS objects with a couple of extra
// fields. They don't have JS prototypes. Internal fields are prefixed with $
// since that isn't a legal in ST identifiers.
// These are the fields on an object:
// - $class - Points to the actual class (not its name).
// - $vars - An array of instance variables.
//
// One instance variable on Class is 'methodDict', which is a HashedCollection
// of methods by name; that's the one used for lookup.
//
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

// Metaclasses are called 'Foo class'.
//
// The core classes are given here:
// - Core hierarchy: ProtoObject -> Object
// - Class hierarchy: (Object) -> Behavior -> ClassDescription -> Class
// - Collections: (Object) -> Collection -> ArrayedCollection -> Array
//                                       -> IndexedCollection -> Dictionary
//                                                            -> HashedCollection

// Make "global" be "global", on Node and browsers.
(function() {
  this.global = this;
})();

const classes = {};

classes['Metaclass'] = {};

function mkClass(name, superName, instVars, classVars) {
  const meta = {
    $class: classes['Metaclass'],
    $vars: [
      name + ' class',
      classes[superName + ' class'],
      classVars || 0,
    ],
  };

  const cls = {
    $class: meta,
    $vars: [
      name,
      classes[superName],
      instVars || 0,
    ],
  };

  classes[name + ' class'] = meta;
  classes[name] = cls;
}

mkClass('Object', 'nil', 0);
mkClass('Behavior', 'Object', 0);
mkClass('ClassDescription', 'Behavior', 4); // name, superclass, instance variables, methodDict.
mkClass('Class', 'ClassDescription', 0);
mkClass('Metaclass', 'ClassDescription', 0);

// HACK: These are hobo implementations of the String and Symbol classes, which
// will get upgraded later.
mkClass('String', 'Object', 1);
mkClass('Symbol', 'String', 0, 1);
mkClass('Number', 'Object', 1);


mkClass('NullObject', 'Object');
const stNil = mkInstance(classes['NullObject']);
classes['nil'] = stNil;

mkClass('Boolean', 'Object');
mkClass('True', 'Boolean');
mkClass('False', 'Boolean');
const stTrue = mkInstance(classes['True']);
const stFalse = mkInstance(classes['False']);
classes['true'] = stTrue;
classes['false'] = stFalse;


const STRING_RAW = 0;
const NUMBER_RAW = 0;
const SYMBOL_CLASS_DICT = 5;

// Class's instance variables: [name superclass instanceVariables methodDict]
const CLASS_VAR_NAME = 0;
const CLASS_VAR_SUPERCLASS = 1;
const CLASS_VAR_INSTVAR_COUNT = 2;
const CLASS_VAR_METHODS = 3;


// Rule 10: The metaclass of Metaclass is an instance of Metaclass.
classes['Metaclass class'].$class = classes['Metaclass'];

classes['Object class'].$vars[CLASS_VAR_SUPERCLASS] = classes['Class'];

mkClass('BlockClosure', 'Object', 4); // Bytecode, argv, argc, methodRecord
mkClass('CompiledMethod', 'Object', 4); // Bytecode, locals, argc, selector

const CLOSURE_BYTECODE = 0;
const CLOSURE_ARGV = 1;
const CLOSURE_ARGC = 2;
const CLOSURE_METHOD_RECORD = 3; // The activationRecord for my method. Used to implement block returns.

const METHOD_BYTECODE = 0;
const METHOD_LOCALS = 1;
const METHOD_ARGC = 2;
const METHOD_SELECTOR = 3;

function mkInstance(cls) {
  return {
    $class: cls,
    $vars: [],
  };
}

function methodLookup(selector, cls) {
  let c = cls;
  while (c) {
    const dict = c.$vars[CLASS_VAR_METHODS];
    if (dict && dict[selector]) {
      return dict[selector];
    }
    c = c.$vars[CLASS_VAR_SUPERCLASS];
  }
  return null;
}

const ERR_DOES_NOT_UNDERSTAND = 'doesNotUnderstand';
const ERR_ARGC_MISMATCH = 'argcMismatch';

// And the single method that starts the ball rolling: Class>>#>>
const theMethod = mkInstance(classes['CompiledMethod']);
theMethod.$vars[METHOD_BYTECODE] = [
  {bytecode: 'primitive', keyword: 'builtin:', name: 'addMethod'},
  {bytecode: 'answerSelf'},
];
theMethod.$vars[METHOD_ARGC] = 1;
theMethod.$vars[METHOD_LOCALS] = 2; // receiver, 1 arg, no temps.
theMethod.$vars[METHOD_SELECTOR] = '>>';

classes['ClassDescription'].$vars[CLASS_VAR_METHODS] = {
  '>>': theMethod,
};

