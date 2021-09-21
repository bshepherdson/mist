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
const CTX_STACK = 4;


classes['Metaclass'] = {};

const methodDictPrototype = {};
const stringPrototype = {};
const numberPrototype = {};

function mkClass(name, superName, instVars, classVars) {
  const metaDict = Object.create(methodDictPrototype);
  metaDict.$vars = [{}];

  const superMeta = classes[superName + ' class'];
  const superMetaVars = superMeta ?
      superMeta.$vars[CLASS_VAR_INSTVAR_COUNT].$vars[NUMBER_RAW] : 0;

  const meta = {
    $class: classes['Metaclass'],
    $vars: [
      wrapString(name + ' class'),
      classes[superName + ' class'],
      wrapNumber(superMetaVars + (classVars || 0)),
      metaDict,
    ],
  };

  const superClass = classes[superName];
  const superVars = superClass ?
      superClass.$vars[CLASS_VAR_INSTVAR_COUNT].$vars[NUMBER_RAW] : 0;

  const dict = Object.create(methodDictPrototype);
  dict.$vars = [{}];
  const cls = {
    $class: meta,
    $vars: [
      wrapString(name),
      classes[superName],
      wrapNumber(superVars + (instVars || 0)),
      dict,
    ],
  };

  classes[name + ' class'] = meta;
  classes[name] = cls;
}

mkClass('Object', null, 0);
mkClass('Behavior', 'Object', 0);

mkClass('NullObject', 'Object');
const stNil = mkInstance(classes['NullObject']);
classes['nil'] = stNil;
classes['Object'].$vars[CLASS_VAR_SUPERCLASS] = stNil;

mkClass('ClassDescription', 'Behavior', 4); // name, superclass, instance variables, methodDict.
mkClass('Class', 'ClassDescription', 0);
mkClass('Metaclass', 'ClassDescription', 0);
mkClass('CompiledMethod', 'Object', 5); // argc, locals, BC, class, selector

// HACK: These are hobo implementations of the String and Symbol classes, which
// will get upgraded later.
mkClass('String', 'Object', 1);
mkClass('Symbol', 'String', 0, 1);
mkClass('Number', 'Object', 1);
mkClass('SystemDictionary', 'Object', 0);
mkClass('MethodContext', 'Object', 5);

mkClass('Collection', 'Object', 0);
mkClass('HashedCollection', 'Collection', 1);

mkClass('Boolean', 'Object');
mkClass('True', 'Boolean');
mkClass('False', 'Boolean');
const stTrue = mkInstance(classes['True']);
const stFalse = mkInstance(classes['False']);
classes['true'] = stTrue;
classes['false'] = stFalse;

// This is a bit of tricky bootstrapping, since it's hard to define instances.
methodDictPrototype.$class = classes['HashedCollection'];
stringPrototype.$class = classes['String'];
numberPrototype.$class = classes['Number'];


// Rule 10: The metaclass of Metaclass is an instance of Metaclass.
classes['Metaclass class'].$class = classes['Metaclass'];

classes['Object class'].$vars[CLASS_VAR_SUPERCLASS] = classes['Class'];

mkClass('BlockClosure', 'Object', 6);
// Argc, locals, bytecode, methodRecord, argv index, handleActive

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
const METHOD_CLASS = 3;
const METHOD_NAME = 4;

const DICTIONARY_RAW = 0;

function mkInstance(cls) {
  const vars = [];
  if (cls) {
    const instVars = cls.$vars[CLASS_VAR_INSTVAR_COUNT].$vars[NUMBER_RAW];
    for (let i = 0; i < instVars; i++) {
      vars.push(stNil);
    }
  }

  return {
    $class: cls,
    $vars: vars,
  };
}

function wrapNumber(n) {
  const cls = classes['Number'];
  const inst = cls ? mkInstance(cls) : Object.create(numberPrototype);
  if (!cls) {
    inst.$vars = [];
  }
  inst.$vars[NUMBER_RAW] = n;
  return inst;
}

function wrapString(s) {
  const cls = classes['String'];
  const inst = cls ? mkInstance(cls) : Object.create(stringPrototype);
  if (!cls) {
    inst.$vars = [];
  }
  inst.$vars[STRING_RAW] = s;
  return inst;
}

function methodLookup(selector, cls) {
  let c = cls;
  while (c && c != stNil) {
    const dict = c.$vars[CLASS_VAR_METHODS].$vars[DICTIONARY_RAW];
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
  let dict = classes[cls].$vars[CLASS_VAR_METHODS].$vars[DICTIONARY_RAW];
  if (!dict) {
    dict = classes[cls].$vars[CLASS_VAR_METHODS].$vars[DICTIONARY_RAW] = {};
  }

  method.$vars[METHOD_CLASS] = classes[cls];
  method.$vars[METHOD_NAME] = wrapString(selector);
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
  constructor(thread, opt_ctx) {
    // JS VM stack.
    this._thread = thread;

    if (opt_ctx) {
      this.ctx = opt_ctx;
    }
  }

  init(parent, locals, method) {
    this.ctx = mkInstance(classes['MethodContext']);
    this.ctx.$vars[CTX_METHOD] = method;
    this.ctx.$vars[CTX_LOCALS] = locals || [];
    this.ctx.$vars[CTX_SENDER] = parent && parent.context() || stNil;
    this.ctx.$vars[CTX_PC] = wrapNumber(0);
    this.ctx.$vars[CTX_STACK] = [];

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

  stack() {
    return this.ctx.$vars[CTX_STACK];
  }

  pcBump(amount) {
    // TODO Immutable numbers?
    this.ctx.$vars[CTX_PC].$vars[NUMBER_RAW] += amount;
  }

  argc() {
    return this.ctx.$vars[CTX_METHOD].$vars[METHOD_ARGC].$vars[NUMBER_RAW];
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
    // Originally I tried to throw when ix <= this.argc() but that doesn't work
    // for blocks, since there are actually blocks and locals interleaved. eg.
    // a method with 0 args, 1 local, block with 1 arg and no local. The block
    // can write to the local (1) but that fails the above test.
    // Ultimately checking the target of writes is the compiler's business.
    this.ctx.$vars[CTX_LOCALS][ix] = value;
  }

  locals() {
    return this.ctx.$vars[CTX_LOCALS];
  }

  self() {
    return this.getLocal(0);
  }

  parent() {
    const sender = this.ctx && this.ctx.$vars[CTX_SENDER];
    return sender ?  new ActivationRecord(this.thread(), sender) : null;
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

  // Comparison of ActivationRecords actually does === on the inner
  // MethodContexts because we recreate the ActivationRecord in runBlock.
  equals(other) {
    return this.ctx === other.ctx;
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

