// Primitive list.
// $FF isn't allowed; it's reserved for expansion.
import {answer, primitives, self} from './bytecodes.mjs';
import {push, pop, readIV, readArray, writeIV, writeArray} from './memory.mjs';

//   0: basicNew
primitives[0] = function(process, ctx) {
  // Self is a class, we want to create and answer a new instance of it.
  const cls = self(ctx);
  const p = mkInstance(cls);
  answer(process, ctx, p);
};

//   1: class
primitives[1] = function(process, ctx) {
  answer(process, ctx, classOf(self(ctx)));
};

//   2: halt
primitives[2] = function(process, ctx) {
  debugger;
  answer(process, ctx, self(ctx));
};

//   3: instVarAt:
primitives[3] = function(process, ctx) {
  const index = fromSmallInteger(readLocal(ctx, 1)); // 1-based
  const value = readIV(self(ctx), index - 1); // 0-based
  answer(process, ctx, value);
};

//   4: instVarAt:put:
primitives[4] = function(process, ctx) {
  const index = fromSmallInteger(readLocal(ctx, 1)); // 1-based
  const value = readLocal(ctx, 2);
  writeIV(self(ctx), index - 1, value); // 0-based index here
};

function call(process, ctx, argc, selector, argFn) {
  // Since the primitive always returns directly, we can (ab)use the sender's
  // stack.
  const sender = readIV(ctx, CTX_SENDER);
  push(sender, self(ctx));
  for (let i = 0; i < argc; i++) {
    push(sender, argFn(ctx, i));
  }
  popContext(process);
  sendOp(process, sender, argc, selector, false);
}

function performArg(ctx, i) {
  return readLocal(ctx, i + 2);
}

//   5: perform:
primitives[5] = function(process, ctx) {
  call(process, ctx, 0, readLocal(ctx, 1), performArg);
};

//   6: perform:with:
primitives[6] = function(process, ctx) {
  call(process, ctx, 1, readLocal(ctx, 1), performArg);
};

//   7: perform:with:with:
primitives[7] = function(process, ctx) {
  call(process, ctx, 2, readLocal(ctx, 1), performArg);
};

//   8: perform:with:with:with:
primitives[8] = function(process, ctx) {
  call(process, ctx, 3, readLocal(ctx, 1), performArg);
};

//   9: perform:withArguments:
primitives[9] = function(process, ctx) {
  const argv = readLocal(ctx, 2);
  call(process, ctx, 3, readLocal(ctx, 1), (c, i) => readArray(argv, i));
};


function mkSubclass(ctx, hasInstVars, hasClassVars) {
  const superclass = self(ctx);
  const className = readLocal(ctx, 1); // Pointer to a symbol.
  let instVars = '';
  let classVars = '';

  let ix = 2;
  if (hasInstVars) {
    instVars = asJSString(readLocal(ctx, ix++));
  }
  if (hasClassVars) {
    classVars = asJSString(readLocal(ctx, ix++));
  }

  instVars = instVars ? instVars.split(/ +/).length : 0;
  classVars = classVars ? classVars.split(/ +/).length : 0;

  const classIndex = read(MA_NEXT_CLASS_INDEX);
  write(classIndex + 1);
  return defClass(classIndex, className, superclass, instVars, classVars);
}

//  10: subclass:
primitives[10] = function(process, ctx) {
  answer(process, ctx, mkSubclass(ctx, false, false));
};

//  11: subclass:instanceVariableNames:
primitives[11] = function(process, ctx) {
  answer(process, ctx, mkSubclass(ctx, true, false));
};

//  12: subclass:instanceVariableNames:classVarNames:
primitives[12] = function(process, ctx) {
  answer(process, ctx, mkSubclass(ctx, true, true));
};


function runBlock(process, ctx, argc) {
  const block = self(ctx);
  const argcWanted = fromSmallInteger(readIV(ctx, CLOSURE_ARGC));
  if (argc !== argcWanted) {
    throw new BlockArgumentCountMismatchError(argcWanted, argc);
  }

  const outerCtx = readIV(block, BLOCK_CONTEXT);
  const locals = readIV(outerCtx, CTX_LOCALS);

  const newCtx = mkInstance(read(classTable(CLS_CONTEXT)), 19);
  writeIV(newCtx, CTX_PC, readIV(block, BLOCK_PC_START));
  writeIV(newCtx, CTX_SENDER, readIV(ctx, CTX_SENDER));
  writeIV(newCtx, CTX_METHOD, block);
  writeIV(newCtx, CTX_STACK_INDEX, toSmallInteger(0));
  writeIV(newCtx, CTX_LOCALS, locals);

  const argv = fromSmallInteger(readIV(block, BLOCK_ARGV));
  for (let i = 0; i < argc; i++) {
    writeArray(locals, argv + i, readLocal(ctx, i + 1));
  }

  popContext(process); // Pop the #value frame off the stack.
  pushContext(process, newCtx); // And execute into the new one.
}

//  13: value
primitives[13] = function(process, ctx) {
  runBlock(process, ctx, 0);
};

//  14: valueNoContextSwitch
primitives[14] = function(process, ctx) {
  // TODO: Atomic operations that prevent the VM switching threads.
  runBlock(process, ctx, 0);
};

//  15: value:
primitives[15] = function(process, ctx) {
  runBlock(process, ctx, 1);
};

//  16: value:value:
primitives[16] = function(process, ctx) {
  runBlock(process, ctx, 2);
};

//  17: value:value:value:
primitives[17] = function(process, ctx) {
  runBlock(process, ctx, 3);
};

//  18: value:value:value:value:
primitives[18] = function(process, ctx) {
  runBlock(process, ctx, 4);
};

//  20: console.log.string - logs the first argument, not self.
primitives[20] = function(process, ctx) {
  const p = readLocal(ctx, 1);
  const isStr = hasClass(p, CLS_STRING);
  const isSym = hasClass(p, CLS_SYMBOL);
  if (isStr || isSym) {
    console.log((isSym ? '#' : '') + asJSString(p));
  } else {
    // TODO Better object output?
    console.log('Object: ' + p);
  }
};

//  21: throw
primitives[21] = function(process, ctx) {
  throw new Error(readLocal(ctx, 1));
};

//  25: basicNew:
primitives[25] = function(process, ctx) {
  // Self is a class, and we pass the number in arg 1 as the size.
  const cls = self(ctx);
  const size = fromSmallInteger(readLocal(ctx, 1));
  answer(process, ctx, mkInstance(cls, size));
};

//  26: at:
primitives[26] = function(process, ctx) {
  const array = self(ctx);
  const index = fromSmallInteger(readLocal(ctx, 1));
  answer(process, ctx, readArray(array, index));
};

//  27: at:put:
primitives[27] = function(process, ctx) {
  const array = self(ctx);
  const index = fromSmallInteger(readLocal(ctx, 1));
  const value = readLocal(ctx, 2);
  writeArray(array, index, value);
  answer(process, ctx, array);
};

//  28: array size
primitives[28] = function(process, ctx) {
  const array = self(ctx);
  answer(process, ctx, toSmallInteger(arraySize(array)));
};


//  30: ==
primitives[30] = function(process, ctx) {
  const a = self(ctx);
  const b = readLocal(ctx, 1);
  answer(process, ctx, a === b ? MA_TRUE : MA_FALSE);
};

//  These are on any SmallInteger:
function binOp(fn, ctx) {
  const a = fromSmallInteger(self(ctx));
  const b = fromSmallInteger(readLocal(ctx, 1));
  return fn(a, b);
}

function numericBinOp(fn) {
  return function(process, ctx) {
    answer(process, ctx, toSmallInteger(binOp(fn, ctx)));
  };
}

function comparisonBinOp(fn) {
  return function(process, ctx) {
    answer(process, ctx, binOp(fn, ctx) ? MA_TRUE : MA_FALSE);
  };
}


//  31: +
primitives[31] = numericBinOp((a, b) => a + b);
//  32: -
primitives[32] = numericBinOp((a, b) => a - b);
//  33: *
primitives[33] = numericBinOp((a, b) => a * b);
//  34: // (integer division)
primitives[34] = numericBinOp((a, b) => {
  const base = Math.floor(Math.abs(a) / Math.abs(b));
  return Math.sign(a) === Math.sign(b) ? base : -base;
});

//  35: \\
primitives[35] = numericBinOp((a, b) => a % b);

//  38: bitwise or
primitives[38] = numericBinOp((a, b) => a | b);
//  39: bitwise and
primitives[39] = numericBinOp((a, b) => a & b);
//  40: bitwise xor
primitives[40] = numericBinOp((a, b) => a ^ b);

//  36: <
primitives[36] = comparisonBinOp((a, b) => a < b);
//  37: = (numbers)
primitives[37] = comparisonBinOp((a, b) => a === b);

//  41: bitwise invert
primitives[41] = function(process, ctx) {
  const n = fromSmallInteger(self(ctx));
  answer(process, ctx, toSmallInteger(~n));
};

//  42: number toString
primitives[42] = function(process, ctx) {
  const n = fromSmallInteger(self(ctx));
  answer(process, ctx, wrapString('' + n));
};

