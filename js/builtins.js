const builtins = {};

function answer(ar, value) {
  ar.thread.pop();
  ar.parent.stack.push(value);
}

// Called from Class>>basicNew, expects the receiver to be the class.
// Answers the new instance.
builtins.basicNew = function(ar) {
  answer(ar, mkInstance(ar.locals[0]));
};

// Called from Object>>class, answers the class of the receiver.
builtins.class = function(ar) {
  answer(ar, ar.locals[0].$class);
};

function mkSubclass(ar, instanceVariables, classVariables) {
  const parent = ar.locals[0];
  const name = ar.locals[1].$vars[STRING_RAW];
  let instVars = '';
  let classVars = '';

  let ix = 2;
  if (instanceVariables) {
    instVars = ar.locals[ix++].$vars[STRING_RAW];
  }
  if (classVariables) {
    classVars = ar.locals[ix++].$vars[STRING_RAW];
  }

  instVars = instVars ? instVars.split(/ +/).length : 0;
  classVars = classVars ? classVars.split(/ +/).length : 0;

  mkClass(name, parent.$vars[CLASS_VAR_NAME], instVars, classVars);
  ar.stack.push(classes[name]);
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
  const start = ar.parent.pc;
  const argc = ar.locals[1].$vars[NUMBER_RAW];
  const temps = ar.locals[2].$vars[NUMBER_RAW];
  const len = ar.locals[3].$vars[NUMBER_RAW];

  const method = mkInstance(classes['CompiledMethod']);
  method.$vars[METHOD_BYTECODE] = ar.parent.bytecode.slice(start, start + len);
  method.$vars[METHOD_LOCALS] = wrapNumber(1 + argc + temps);
  method.$vars[METHOD_ARGC] = ar.locals[1];
  ar.parent.pc += len; // Advance the PC past the method we just compiled.
  answer(ar, method);
}

// Called from Class>>method:selector: Receiver is the class itself,
// argv[0] the method.
// The class has a method dictionary.
// Answers 'self', the class.
builtins['addMethod'] = function(ar) {
  const self = ar.locals[0];
  const method = ar.locals[1];
  const selector = ar.locals[2].$vars[STRING_RAW];
  let dict = self.$vars[CLASS_VAR_METHODS];
  if (!dict) {
    self.$vars[CLASS_VAR_METHODS] = dict = {};
  }

  dict[selector] = method;
  answer(ar, self);
};

// Called by String>>#asSymbol. The (wrapped) String is the receiver.
// Answers the Symbol version of this string.
builtins['toSymbol'] = function(ar) {
  const raw = ar.locals[0].$vars[STRING_RAW];
  let dict = classes['Symbol class'].$vars[SYMBOL_CLASS_DICT];
  if (!dict) {
    dict = classes['Symbol class'].$vars[SYMBOL_CLASS_DICT] = {};
  }

  if (!dict[raw]) {
    dict[raw] = mkInstance(classes['Symbol']);
    dict[raw].$vars[STRING_RAW] = raw;
  }
  answer(ar, dict[raw]);
};

builtins['runBlock'] = function(ar) {
  // self is the block, which remembers its containing context.
  // We need to populate the block's arguments into the right spots, if any.
  const block = ar.locals[0];
  const argc = block.$vars[CLOSURE_ARGC];
  if (argc + 1 !== ar.locals.length) {
    throw new BlockArgumentCountMismatchError(argc, ar.locals.length - 1);
  }

  const argv = block.$vars[CLOSURE_ARGV];
  const outerAR = block.$vars[CLOSURE_METHOD_RECORD];
  for (let i = 0; i < argc; i++) {
    outerAR.locals[argv + i] = ar.locals[i + 1];
  }

  // Now it's populated, so we set up the block's AR and call it.
  const newAR = activationRecord(ar, outerAR.locals,
      block.$vars[CLOSURE_BYTECODE], 0);
  newAR.methodRecord = outerAR;

  // This primitive is called from inside eg. BlockClosure value: x but there's
  // no need to add an extra layer to the return.
  // This returns directly from the block to the caller.
  ar.thread.pop();
  ar.thread.push(newAR);
};

function numericBinOp(fn) {
  return function(ar) {
    const a = ar.locals[0].$vars[NUMBER_RAW];
    const b = ar.locals[1].$vars[NUMBER_RAW];
    const c = fn(a, b);
    answer(ar, wrapNumber(c));
  };
}

builtins['+']  = numericBinOp((a, b) => a + b);
builtins['-']  = numericBinOp((a, b) => a - b);
builtins['*']  = numericBinOp((a, b) => a * b);
builtins['/']  = numericBinOp((a, b) => a / b);
builtins['%']  = numericBinOp((a, b) => a % b);
builtins['<']  = numericBinOp((a, b) => a < b);
builtins['|']  = numericBinOp((a, b) => a | b);
builtins['&']  = numericBinOp((a, b) => a & b);
builtins['^']  = numericBinOp((a, b) => a ^ b);
builtins['num='] = numericBinOp((a, b) => a === b);

// Identity
builtins['=='] = function(ar) {
  const res = ar.locals[0] === ar.locals[1];
  answer(ar, res ? stTrue : stFalse);
};

builtins['^-1'] = function(ar) {
  const value = ar.locals[0].$vars[NUMBER_RAW];
  answer(ar, wrapNumber(value & -1));
};

// Logs the *first argument* (not self).
// Answers that argument.
builtins['console.log'] = function(ar) {
  console.log(ar.locals[1]);
  answer(ar, ar.locals[1]);
};


// Working with a raw Javascript map. Puts that map in the first instance
// variable. Answers self.
builtins['new object'] = function(ar) {
  ar.locals[0].$vars[DICTIONARY_RAW] = {};
  answer(ar, ar.locals[0]);
};

// Answers the value looked up.
builtins['dict_at:'] = function(ar) {
  const key = ar.locals[1].$vars[STRING_RAW];
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  answer(ar, map[key] || stNil);
};
// Answers self.
builtins['dict_at:put:'] = function(ar) {
  const key = ar.locals[1].$vars[STRING_RAW];
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  const value = ar.locals[2];
  map[key] = value;
  answer(ar, ar.locals[0]);
};

function stArray(array) {
  return {
    $class: classes['Array'],
    $vars: [array || []],
  };
}

builtins['dict_values'] = function(ar) {
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  answer(ar, stArray(Object.values(map)));
};
builtins['dict_keys'] = function(ar) {
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  answer(ar, stArray(Object.keys(map)));
};



// Working with a raw Javascript array.
// Expects the array to be instance variable 0.
builtins['new array'] = function(ar) {
  answer(ar, []);
};

builtins['array_at:'] = function(ar) {
  const key = ar.locals[1].$vars[NUMBER_RAW];
  answer(ar, ar.locals[0][key]);
};
builtins['array_at:put:'] = function(ar) {
  const key = ar.locals[1].$vars[NUMBER_RAW];
  ar.locals[0][key] = ar.locals[2];
  answer(ar, ar.locals[0]);
};

builtins['array_length'] = function(ar) {
  const array = ar.locals[0];
  answer(ar, wrapNumber(array.length));
};

