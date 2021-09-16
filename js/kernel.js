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
mkClass('CompiledMethod', 'Object', 3); // argc, locals, first BC

// HACK: These are hobo implementations of the String and Symbol classes, which
// will get upgraded later.
mkClass('String', 'Object', 1);
mkClass('Symbol', 'String', 0, 1);
mkClass('Number', 'Object', 1);
mkClass('SystemDictionary', 'Object', 0);
mkClass('MethodContext', 'Object', 5);

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

// Indexes into MethodContext/ActivationRecord.
const CTX_METHOD = 0;
const CTX_LOCALS = 1;
const CTX_PC = 2;
const CTX_SENDER = 3;


// Rule 10: The metaclass of Metaclass is an instance of Metaclass.
classes['Metaclass class'].$class = classes['Metaclass'];

classes['Object class'].$vars[CLASS_VAR_SUPERCLASS] = classes['Class'];

mkClass('BlockClosure', 'Object', 6);
// Argc, locals, bytecode, methodRecord, argv index, handleActive
mkClass('CompiledMethod', 'Object', 4); // Bytecode, locals, argc, selector

// NB: Keep the shared prefixes of CLOSURE_ and METHOD_ in sync! A few parts of
// the VM treat them interchangeably.
const CLOSURE_ARGC = 0;
const CLOSURE_LOCALS = 1;
const CLOSURE_BYTECODE = 2;
const CLOSURE_METHOD_RECORD = 3; // The MethodContext for my method. Used to implement block returns.
const CLOSURE_ARGV_START = 4; // Index into the parent method's locals list where the first arg is located.
const CLOSURE_HANDLER_ACTIVE = 5;

const METHOD_ARGC = 0;
const METHOD_LOCALS = 1;
const METHOD_BYTECODE = 2;

const DICTIONARY_RAW = 0;

function mkInstance(cls) {
  return {
    $class: cls,
    $vars: [],
  };
}

function wrapNumber(n) {
  const inst = mkInstance(classes['Number']);
  inst.$vars[NUMBER_RAW] = n;
  return inst;
}

function wrapString(s) {
  const inst = mkInstance(classes['String']);
  inst.$vars[STRING_RAW] = s;
  return inst;
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

// Creates hand-rolled Smalltalk methods for bootstrapping.
function mkMethod(argc, locals, bytecode) {
  const method = mkInstance(classes['CompiledMethod']);
  method.$vars[METHOD_BYTECODE] = bytecode;
  method.$vars[METHOD_ARGC] = wrapNumber(argc);
  method.$vars[METHOD_LOCALS] = wrapNumber(locals);
  return method;
}

function attachMethod(cls, selector, method) {
  let dict = classes[cls].$vars[CLASS_VAR_METHODS];
  if (!dict) {
    dict = classes[cls].$vars[CLASS_VAR_METHODS] = {};
  }

  dict[selector] = method;
}

// Two key built-in methods that are used to compile more:
// - CompiledMethod class >> argc:locals:length: to create new methods.
// - Class >> method:selector: for attaching them to a class's dictionary.
attachMethod('CompiledMethod class', 'argc:locals:length:',
    mkMethod(3, 1 + 3 + 0, [
      {bytecode: 'primitive', keyword: 'builtin:', name: 'defineMethod'},
    ]));

attachMethod('ClassDescription', 'method:selector:',
    mkMethod(2, 1 + 2 + 0, [
      {bytecode: 'primitive', keyword: 'builtin:', name: 'addMethod'},
    ]));

attachMethod('String', 'asSymbol',
    mkMethod(0, 1, [
      {bytecode: 'primitive', keyword: 'builtin:', name: 'toSymbol'},
    ]));

attachMethod('SystemDictionary class', 'at:',
    mkMethod(1, 1 + 1 + 0, [
      {bytecode: 'primitive', keyword: 'builtin:', name: 'systemDictAt'},
    ]));


// MethodContext is the activation record!
// This class just wraps the ST object with a JS interface.
// NB: It's circular to call Smalltalk methods to implement MethodContext; that
// would require creating more MethodContexts!
class ActivationRecord {
  // There are two ways to create an ActivationRecord:
  // - directly from a MethodContext with new ActivationRecord(someContext), or
  // - constructing a new one for a message send, with
  //   new ActivationRecord().init(parent, locals, method)
  constructor(opt_ctx) {
    if (opt_ctx) {
      this.ctx = opt_ctx;
    }
    // JS VM stack.
    this.stack = [];
  }

  init(parent, locals, method) {
    this.ctx = mkInstance(classes['MethodContext']);
    this.ctx.$vars[CTX_METHOD] = method;
    this.ctx.$vars[CTX_LOCALS] = locals || [];
    this.ctx.$vars[CTX_SENDER] = parent || stNil;
    this.ctx.$vars[CTX_PC] = wrapNumber(0);

    // Set all the actual locals (not the arguments) to stNil.
    for (let i = 0; i < method.$vars[METHOD_LOCALS].$vars[NUMBER_RAW]; i++) {
      this.ctx.$vars[CTX_LOCALS].push(stNil);
    }

    this._thread = parent ? parent.thread() : null;
    return this;
  }

  context() {
    return this.ctx;
  }

  peek() {
    return this.bytecode()[this.pc()];
  }

  next() {
    const bc = this.peek();
    this.pcBump(1);
    return bc;
  }

  // Returns the raw JS array.
  bytecode() {
    return this.method().$vars[METHOD_BYTECODE];
  }

  method() {
    return this.ctx.$vars[CTX_METHOD];
  }

  pc() {
    return this.ctx.$vars[CTX_PC].$vars[NUMBER_RAW];
  }

  pcBump(amount) {
    // TODO Immutable numbers?
    this.ctx.$vars[CTX_PC].$vars[NUMBER_RAW] += amount;
  }

  // JS indexing.
  // For regular methods:
  // - 0 = receiver
  // - 1 to argc = args
  // - argc+1 to argc+locals = locals
  //
  // Blocks know how to adjust for this, since their args and locals are inlined
  // into the parent's fields.
  getLocal(ix) {
    return this.ctx.$vars[CTX_LOCALS][ix];
  }

  setLocal(ix, value) {
    if (ix <= this.argc()) {
      throw new Exception('Cannot set args/receiver');
    }
    this.ctx.$vars[CTX_LOCALS][ix] = value;
  }

  locals() {
    return this.ctx.$vars[CTX_LOCALS];
  }

  self() {
    return this.getLocal(0);
  }

  parent() {
    return this.ctx.$vars[CTX_SENDER];
  }

  thread() {
    return this._thread;
  }

  // When representing a block execution, the call stack captures how we really
  // got here: some method called `aBlock value` or similar.
  // - this is the block's own context
  // - this.parent() is eg. BlockClosure>>value
  // - this.parent().parent() is the calling method.
  //
  // Meanwhile this.blockContext() is the MethodContext that defined the block.
  // That might not even be on the stack anymore! But we preserve it with its
  // locals so we can close over them.
  inBlockContext(ar) {
    this._blockContext = ar;
  }
  blockContext() {
    return this._blockContext;
  }
}


// Upgrade standard Javascript arrays to become Smalltalk objects.
// TODO These should probably be divided into a separate NativeArray or JsArray
// or something.
Object.defineProperty(Array.prototype, '$class', {
  configurable: false,
  enumerable: false,
  //writable: false,
  get: () => classes['Array'],
});

