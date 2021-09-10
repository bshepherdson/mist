const builtins = {};

// Called from Class>>basicNew, expects the receiver to be the class.
// Pushes a new instance of that class.
builtins.basicNew = function(ar) {
  ar.stack.push(mkInstance(ar.locals[0]));
};

// Called from Object>>class, pushes the class of the receiver.
builtins.class = function(ar) {
  ar.stack.push(ar.locals[0].$class);
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
// the first arg to be the new name. Pushes the resulting class.
builtins['subclass:'] = function(ar) {
  mkSubclass(ar, false, false);
};
builtins['subclass:instanceVariableNames:'] = function(ar) {
  mkSubclass(ar, true, false);
};
builtins['subclass:instanceVariableNames:classVariableNames:'] = function(ar) {
  mkSubclass(ar, true, true);
};

// Called from Class>>#>>. Receiver is the class itself, argv[0] the method.
// The method knows its own selector, and the class has a method dictionary.
builtins['addMethod'] = function(ar) {
  const self = ar.locals[0];
  const method = ar.locals[1];
  let dict = self.$vars[CLASS_VAR_METHODS];
  if (!dict) {
    self.$vars[CLASS_VAR_METHODS] = dict = {};
  }

  dict[method.$vars[METHOD_SELECTOR]] = method;
};

// Called by String>>#asSymbol. The (wrapped) String is the receiver.
// Pushes the Symbol version of this string.
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
  ar.stack.push(dict[raw]);
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
  ar.thread.push(newAR);
};

function numericBinOp(fn) {
  return function(ar) {
    const a = ar.locals[0].$vars[NUMBER_RAW];
    const b = ar.locals[1].$vars[NUMBER_RAW];
    const c = fn(a, b);
    ar.stack.push(wrapLiteral(c));
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
  ar.stack.push(res ? stTrue : stFalse);
};

builtins['^-1'] = function(ar) {
  const value = ar.locals[0].$vars[NUMBER_RAW];
  ar.stack.push(wrapLiteral(value & -1));
};

// Logs the *first argument* (not self).
builtins['console.log'] = function(ar) {
  console.log(ar.locals[1]);
};


// Working with a raw Javascript map. Puts that map in the first instance
// variable.
builtins['new object'] = function(ar) {
  ar.locals[0].$vars[DICTIONARY_RAW] = {};
};

builtins['dict_at:'] = function(ar) {
  const key = ar.locals[1].$vars[STRING_RAW];
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  ar.stack.push(map[key] || stNil);
};
builtins['dict_at:put:'] = function(ar) {
  const key = ar.locals[1].$vars[STRING_RAW];
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  const value = ar.locals[2];
  map[key] = value;
};

function stArray(array) {
  return {
    $class: classes['Array'],
    $vars: [array || []],
  };
}

builtins['dict_values'] = function(ar) {
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  ar.stack.push(stArray(Object.values(map)));
};
builtins['dict_keys'] = function(ar) {
  const map = ar.locals[0].$vars[DICTIONARY_RAW];
  ar.stack.push(stArray(Object.keys(map)));
};



// Working with a raw Javascript array.
// Expects the array to be instance variable 0.
builtins['new array'] = function(ar) {
  ar.stack.push([]);
};

builtins['array_at:'] = function(ar) {
  const key = ar.locals[1].$vars[NUMBER_RAW];
  ar.stack.push(ar.locals[0][key]);
};
builtins['array_at:put:'] = function(ar) {
  const key = ar.locals[1].$vars[NUMBER_RAW];
  ar.locals[0][key] = ar.locals[2];
};

builtins['array_length'] = function(ar) {
  const array = ar.locals[0];
  ar.stack.push(wrapLiteral(array.length));
};

