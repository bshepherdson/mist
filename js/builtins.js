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

