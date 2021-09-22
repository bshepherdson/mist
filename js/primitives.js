// Called from Class>>basicNew, expects the receiver to be the class.
// Answers the new instance.
const PRIM_BASIC_NEW = 1;
primitives[PRIM_BASIC_NEW] = function(process, ctx) {
  const instance = mkInstance(self(ctx));
  answer(process, ctx, instance);
};


// Called from Object>>class, answers the class of the receiver.
const PRIM_CLASS = 2;
primitives[PRIM_CLASS] = function(process, ctx) {
  answer(process, ctx, readAt(self(ctx), OBJ_CLASS));
};

function mkSubclass(superclass, name, instanceVariables, classVariables) {
  const parent = ar.self();
  const name = ar.getLocal(1).$vars[STRING_RAW];
  let instVars = '';
  let classVars = '';

  let ix = 2;
  if (instanceVariables) {
    instVars = ar.getLocal(ix++).$vars[STRING_RAW];
  }
  if (classVariables) {
    classVars = ar.getLocal(ix++).$vars[STRING_RAW];
  }

  instVars = instVars ? instVars.split(/ +/).length : 0;
  classVars = classVars ? classVars.split(/ +/).length : 0;

  mkClass(name, parent.$vars[CLASS_VAR_NAME].$vars[STRING_RAW], instVars, classVars);
  return classes[name];
}

// Called from Class>>subclass:, expects receiver to be the parent class and
// the first arg to be the new name. Answers the resulting class.
builtins['subclass:'] = function(ar) {
  answer(ar, mkSubclass(ar, false, false));
};
builtins['subclass:instanceVariableNames:'] = function(ar) {
  answer(ar, mkSubclass(ar, true, false));
};
builtins['subclass:instanceVariableNames:classVariableNames:'] = function(ar) {
  answer(ar, mkSubclass(ar, true, true));
};

// Called from CompiledMethod class >> argc:locals:length: to consume the
// following bytecodes from the caller's scope.
// Answers the new CompiledMethod instance.
builtins['defineMethod'] = function(ar) {
  // Parent's PC is the start of the method, and needs to be advanced.
  const start = ar.parent().pc();
  const argc = ar.getLocal(1).$vars[NUMBER_RAW];
  const temps = ar.getLocal(2).$vars[NUMBER_RAW];
  const len = ar.getLocal(3).$vars[NUMBER_RAW];

  const method = mkInstance(classes['CompiledMethod']);
  method.$vars[METHOD_BYTECODE] = ar.parent().bytecode().slice(start, start + len);
  method.$vars[METHOD_LOCALS] = wrapNumber(1 + argc + temps);
  method.$vars[METHOD_ARGC] = ar.getLocal(1);
  ar.parent().pcBump(len); // Advance the PC past the method we just compiled.
  answer(ar, method);
}

// Called from Class>>method:selector: Receiver is the class itself,
// argv[0] the method.
// The class has a method dictionary.
// Answers 'self', the class.
builtins['addMethod'] = function(ar) {
  const self = ar.self();
  const method = ar.getLocal(1);
  const selector = ar.getLocal(2).$vars[STRING_RAW];
  let dict = self.$vars[CLASS_VAR_METHODS].$vars[DICTIONARY_RAW];
  if (!dict) {
    self.$vars[CLASS_VAR_METHODS].$vars[DICTIONARY_RAW] = dict = {};
  }

  method.$vars[METHOD_CLASS] = self;
  method.$vars[METHOD_NAME] = ar.getLocal(2);
  dict[selector] = method;
  answer(ar, self);
};

function toSymbol(s) {
  let dict = classes['Symbol class'].$vars[SYMBOL_CLASS_DICT];
  if (!dict) {
    dict = classes['Symbol class'].$vars[SYMBOL_CLASS_DICT] = {};
  }

  if (!dict[s]) {
    dict[s] = mkInstance(classes['Symbol']);
    dict[s].$vars[STRING_RAW] = s;
  }
  return dict[s];
}

// Called by String>>#asSymbol. The (wrapped) String is the receiver.
// Answers the Symbol version of this string.
builtins['toSymbol'] = function(ar) {
  answer(ar, toSymbol(ar.self().$vars[STRING_RAW]));
};

builtins['runBlock'] = function(ar) {
  // self is the block, which remembers its containing context.
  // We need to populate the block's arguments into the right spots, if any.
  // This primitive is called from eg. BlockClosure >> value:value: so the args
  // are exactly those that need to be passed to the block.
  const block = ar.self();
  const argcWanted = block.$vars[CLOSURE_ARGC].$vars[NUMBER_RAW];
  const argcGiven = ar.method().$vars[METHOD_ARGC].$vars[NUMBER_RAW];
  if (argcWanted !== argcGiven) {
    throw new BlockArgumentCountMismatchError(argcWanted, argcGiven);
  }

  const outerAR = new ActivationRecord(ar.thread(), block.$vars[CLOSURE_METHOD_RECORD]);
  const argv = block.$vars[CLOSURE_ARGV_START].$vars[NUMBER_RAW];
  for (let i = 0; i < argcGiven; i++) {
    outerAR.setLocal(argv + i, ar.getLocal(i + 1));
  }

  // Now it's populated, so we set up the block's AR and call it.
  const newAR = new ActivationRecord(ar.thread()).init(ar.parent(), outerAR.locals(), block);
  newAR.inBlockContext(outerAR);

  // This primitive is called from inside eg. BlockClosure value: x but there's
  // no need to add an extra layer to the return.
  // This returns directly from the block to the caller.
  ar.thread().pop();
  ar.thread().push(newAR);
};

// Exactly like runBlock except it prevents a context switch in between.
// I'm not actually sure how vital this is - for now it's a copy of runBlock.
// TODO Figure out this and other context switching things!
builtins['runBlockNCS'] = builtins['runBlock'];

function binOp(fn, ar) {
  const a = ar.self().$vars[NUMBER_RAW];
  const b = ar.getLocal(1).$vars[NUMBER_RAW];
  return fn(a, b);
}

function numericBinOp(fn) {
  return function(ar) {
    answer(ar, wrapNumber(binOp(fn, ar)));
  };
}

function comparisonBinOp(fn) {
  return function(ar) {
    answer(ar, binOp(fn, ar) ? stTrue : stFalse);
  };
}

builtins['+'] = numericBinOp((a, b) => a + b);
builtins['-'] = numericBinOp((a, b) => a - b);
builtins['*'] = numericBinOp((a, b) => a * b);
builtins['/'] = numericBinOp((a, b) => a / b);
builtins['%'] = numericBinOp((a, b) => a % b);
builtins['|'] = numericBinOp((a, b) => a | b);
builtins['&'] = numericBinOp((a, b) => a & b);
builtins['^'] = numericBinOp((a, b) => a ^ b);

builtins['<']    = comparisonBinOp((a, b) => a < b);
builtins['num='] = comparisonBinOp((a, b) => a === b);

// Identity
builtins['=='] = function(ar) {
  const res = ar.self() === ar.getLocal(1);
  answer(ar, res ? stTrue : stFalse);
};

builtins['^-1'] = function(ar) {
  const value = ar.self().$vars[NUMBER_RAW];
  answer(ar, wrapNumber(value ^ -1));
};

builtins['numStr'] = function(ar) {
  const value = ar.self().$vars[NUMBER_RAW];
  answer(ar, wrapString('' + value));
};

// Logs the *first argument* (not self).
// Answers that argument.
builtins['console.log'] = function(ar) {
  const value = ar.getLocal(1);
  console.log(value);
  answer(ar, value);
};

// Logs the *first argument* (not self), which is a string.
// Answers that argument.
builtins['console.log.string'] = function(ar) {
  const value = ar.getLocal(1);
  console.log(value.$vars[STRING_RAW]);
  answer(ar, value);
};


builtins['instVarAt:'] = function(ar) {
  const index = ar.getLocal(1).$vars[NUMBER_RAW]; // 1-based index.
  answer(ar, ar.self().$vars[index-1]);
};

builtins['instVarAt:put:'] = function(ar) {
  const index = ar.getLocal(1).$vars[NUMBER_RAW]; // 1-based index.
  ar.self().$vars[index-1] = ar.getLocal(2);
  answer(ar, ar.self());
};


// Working with a raw Javascript map. Puts that map in the first instance
// variable. Answers self.
builtins['new object'] = function(ar) {
  ar.self().$vars[DICTIONARY_RAW] = {};
  answer(ar, ar.self());
};

// Answers the value looked up.
builtins['dict_at:'] = function(ar) {
  const key = ar.getLocal(1).$vars[STRING_RAW];
  const map = ar.self().$vars[DICTIONARY_RAW];
  answer(ar, map[key] || stNil);
};
// Answers self.
builtins['dict_at:put:'] = function(ar) {
  const key = ar.getLocal(1).$vars[STRING_RAW];
  const map = ar.self().$vars[DICTIONARY_RAW];
  const value = ar.getLocal(2);
  map[key] = value;
  answer(ar, ar.self());
};

function stArray(array) {
  return array; // JS Arrays are already turned into ST objects.
  /*
  return {
    $class: classes['Array'],
    $vars: [array || []],
  };
  */
}

builtins['dict_values'] = function(ar) {
  const map = ar.self().$vars[DICTIONARY_RAW];
  answer(ar, stArray(Object.values(map)));
};
builtins['dict_keys'] = function(ar) {
  const map = ar.self().$vars[DICTIONARY_RAW];
  answer(ar, stArray(Object.keys(map).map(wrapString)));
};


builtins['systemDictAt'] = function(ar) {
  const target = ar.getLocal(1).$vars[STRING_RAW];
  answer(ar, classes[target] || stNil);
}

// Working with a raw Javascript array.
// Expects the array to be instance variable 0.
builtins['new array'] = function(ar) {
  // Sets the size if there's an argument, it's new: aSize
  const argc = ar.method().$vars[METHOD_ARGC].$vars[NUMBER_RAW] || 0;
  const len = argc > 0 ? ar.getLocal(1).$vars[NUMBER_RAW] : 0;
  const ret = [];
  for (let i = 0; i < len; i++) {
    ret.push(stNil);
  }
  answer(ar, ret);
};

builtins['array_at:'] = function(ar) {
  const key = ar.getLocal(1).$vars[NUMBER_RAW];
  answer(ar, ar.self()[key]);
};
builtins['array_at:put:'] = function(ar) {
  const key = ar.getLocal(1).$vars[NUMBER_RAW];
  ar.self()[key] = ar.getLocal(2);
  answer(ar, ar.self());
};

builtins['array_length'] = function(ar) {
  answer(ar, wrapNumber(ar.self().length));
};

builtins['starts_with_str'] = function(ar) {
  const self = ar.self().$vars[STRING_RAW];
  const arg = ar.getLocal(1).$vars[STRING_RAW];
  answer(ar, self.startsWith(arg) ? stTrue : stFalse);
};

builtins['str_concat'] = function(ar) {
  const self = ar.self().$vars[STRING_RAW];
  const arg = ar.getLocal(1).$vars[STRING_RAW];
  answer(ar, wrapString(self + arg));
};

builtins['halt'] = function(ar) {
  const raw = ar.self().$vars[NUMBER_RAW];
  console.log('halted', typeof raw === 'number' ? raw : ar.self());
  debugger;
  answer(ar, stNil);
};

builtins['throw'] = function(ar) {
  throw ar.self();
  answer(ar, stNil);
};


// The performers send a message to self via reflection.
// self is the receiver; the first argument is the selector, other args follow.
// They should return whatever the call returns, so the perform: context frame
// is elided. That is, the return should be directly to the sender of perform:.
// To achieve that, they pass ar.parent() as the optional return target for
// send().
// That also means we don't need to worry about popping the stack cleanly.
builtins['perform:'] = function(ar) {
  send(ar, ar.self(), ar.getLocal(1).$vars[STRING_RAW], [], false, ar.parent());
};
builtins['perform:with:'] = function(ar) {
  send(ar, ar.self(), ar.getLocal(1).$vars[STRING_RAW], [ar.getLocal(2)], false, ar.parent());
};
builtins['perform:with:with:'] = function(ar) {
  send(ar, ar.self(), ar.getLocal(1).$vars[STRING_RAW], ar.locals().slice(2, 4), false, ar.parent());
};
builtins['perform:with:with:with:'] = function(ar) {
  send(ar, ar.self(), ar.getLocal(1).$vars[STRING_RAW], ar.locals().slice(2, 5), false, ar.parent());
};

builtins['perform:withArguments:'] = function(ar) {
  ar.thread().pop();
  const args = ar.getLocal(2);
  if (!Array.isArray(args)) {
    throw new Error('perform:withArguments: only works on Arrays');
  }
  send(ar, ar.self(), ar.getLocal(1).$vars[STRING_RAW], args, false, ar.parent());
};

