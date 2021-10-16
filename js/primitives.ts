// Primitive list.
// $FF isn't allowed; it's reserved for expansion.
import {Primitive, answer, primitives, readLocal, self, sendOp} from './bytecodes';
import {defClass} from './bootstrap';
import {BlockArgumentCountMismatchError} from './errors';
import {
  sti, ptr,
  arraySize, behaviorToInstVars, classOf, identityHash,
  MA_NEXT_CLASS_INDEX, MA_NIL, MA_TRUE, MA_FALSE,
  CLS_CHARACTER, CLS_CONTEXT, CLS_STRING, CLS_SYMBOL,
  BLOCK_CONTEXT, BLOCK_ARGV, BLOCK_ARGC, BLOCK_PC_START,
  CTX_LOCALS, CTX_SENDER, CTX_PC, CTX_METHOD, CTX_STACK_INDEX,
  CLASS_VAR1,
  classTable, hasClass, fromSmallInteger, toSmallInteger, isSmallInteger,
  asJSString, mkInstance, push, pop, peek,
  read, readIV, readArray,
  write, writeIV, writeArray,
  readWordArray, writeWordArray, wordArraySize,
  wrapString,
} from './memory';
import {pushContext} from './process';
import {vm} from './vm';

// NB: Primitives execute directly on the parent's context, with the receiver
// and arguments on the stack. They consume those values, and leave a result.
// They MUST leave a (possibly dummy) result, since that value is expected and
// might be DROP'd.

function unary(f: (p: ptr) => ptr): Primitive {
  return (): boolean => {
    push(vm.ctx, f(pop(vm.ctx)));
    return true;
  };
}

//   0: basicNew  -  TOS is a class, we want to create a new instance of it.
primitives[0] = unary(mkInstance);

//   1: class
primitives[1] = unary(classOf);

//   2: basicHash
primitives[2] = unary((p) => toSmallInteger(identityHash(p)));

//   3: instVarAt:
primitives[3] = function() {
  const index = fromSmallInteger(pop(vm.ctx)); // 1-based
  const value = readIV(pop(vm.ctx), index - 1); // 0-based
  push(vm.ctx, value);
  return true;
};

//   4: instVarAt:put:   - Leaves the value on the stack.
primitives[4] = function() {
  const value = pop(vm.ctx);
  const index = fromSmallInteger(pop(vm.ctx)); // 1-based
  writeIV(peek(vm.ctx), index - 1, value); // 0-based index here
  // Read back the value, in case there was a GC.
  push(vm.ctx, readIV(pop(vm.ctx), index - 1));
  return true;
};


// For perform: and friends, the stack is (receiver selector arg...).
// Pop the right number of times to get at the selector, then pass it to
// sendOp.
function call(argc: number) {
  const args = [];
  for (let i = 0; i < argc; i++) {
    args.push(pop(vm.ctx));
  }
  const selector = pop(vm.ctx);
  for (let i = 0; i < argc; i++) {
    push(vm.ctx, args.pop()!);
  }
  sendOp(argc, selector, false);
}

//   5: perform:
primitives[5] = function() {
  call(0);
  return true;
};

//   6: perform:with:
primitives[6] = function() {
  call(1);
  return true;
};

//   7: perform:with:with:
primitives[7] = function() {
  call(2);
  return true;
};

//   8: perform:with:with:with:
primitives[8] = function() {
  call(3);
  return true;
};

//   9: perform:withArguments:
primitives[9] = function() {
  // This one is special. The stack is (receiver selector argv).
  // Pop off the args array and selector, push all the args in order, and call.
  const argv = pop(vm.ctx);
  const selector = pop(vm.ctx);
  const argc = arraySize(argv);
  for (let i = 0; i < argc; i++) {
    push(vm.ctx, readArray(argv, i));
  }
  sendOp(argc, selector, false);
  return true;
};


function mkSubclass(hasInstVars: boolean, hasClassVars: boolean): ptr {
  const classVarNames = hasClassVars ? asJSString(pop(vm.ctx)) : '';
  const instVarNames = hasInstVars ? asJSString(pop(vm.ctx)) : '';

  const className = pop(vm.ctx); // Pointer to a symbol.
  const superclass = pop(vm.ctx);

  let instVars = instVarNames.split(/ +/).length;
  let classVars = classVarNames.split(/ +/).length;

  const classIndex = read(MA_NEXT_CLASS_INDEX);
  write(MA_NEXT_CLASS_INDEX, classIndex + 1);
  return defClass(classIndex, className, superclass, instVars, classVars);
}

//  10: subclass:
primitives[10] = function() {
  push(vm.ctx, mkSubclass(false, false));
  return true;
};

//  11: subclass:instanceVariableNames:
primitives[11] = function() {
  push(vm.ctx, mkSubclass(true, false));
  return true;
};

//  12: subclass:instanceVariableNames:classVarNames:
primitives[12] = function() {
  push(vm.ctx, mkSubclass(true, true));
  return true;
};


function runBlock(argc: number) {
  // Stack is (block args...)
  // Allocating the context first, so we don't have values out of memory in argv
  // in case of a GC.
  const newCtx = mkInstance(read(classTable(CLS_CONTEXT)), 19);
  const argv = new Array(argc);
  for (let i = argc - 1; i >= 0; i--) {
    argv[i] = pop(vm.ctx);
  }
  const block = pop(vm.ctx);
  const argcWanted = fromSmallInteger(readIV(block, BLOCK_ARGC));
  if (argc !== argcWanted) {
    throw new BlockArgumentCountMismatchError(argcWanted, argc);
  }

  const outerCtx = readIV(block, BLOCK_CONTEXT);
  const locals = readIV(outerCtx, CTX_LOCALS);

  // These writes are safe because the mkInstance was the last alloc and so it's
  // gotta be in Eden.
  writeIV(newCtx, CTX_PC, readIV(block, BLOCK_PC_START));
  writeIV(newCtx, CTX_SENDER, vm.ctx);
  writeIV(newCtx, CTX_METHOD, block);
  writeIV(newCtx, CTX_STACK_INDEX, toSmallInteger(0));
  writeIV(newCtx, CTX_LOCALS, locals);

  const argvIndex = fromSmallInteger(readIV(block, BLOCK_ARGV));
  for (let i = 0; i < argc; i++) {
    writeArray(locals, argvIndex + i, argv[i]);
  }

  pushContext(newCtx); // Execute into the new stack frame.
  // NB: Running a block consumes all the values off the stack; it will
  // ultimately push the result of the block.
}

//  13: value
primitives[13] = function() {
  runBlock(0);
  return true;
};

//  14: valueNoContextSwitch
primitives[14] = function() {
  // TODO: Atomic operations that prevent the VM switching threads.
  runBlock(0);
  return true;
};

//  15: value:
primitives[15] = function() {
  runBlock(1);
  return true;
};

//  16: value:value:
primitives[16] = function() {
  runBlock(2);
  return true;
};

//  17: value:value:value:
primitives[17] = function() {
  runBlock(3);
  return true;
};

//  18: value:value:value:value:
primitives[18] = function() {
  runBlock(4);
  return true;
};

//  20: console.log.string - logs the first argument, not self.
//  Leaves nil on the stack.
primitives[20] = function() {
  const p = pop(vm.ctx);
  const isStr = hasClass(p, CLS_STRING);
  const isSym = hasClass(p, CLS_SYMBOL);
  if (isStr || isSym) {
    console.log((isSym ? '#' : '') + asJSString(p));
  } else {
    // TODO Better object output?
    console.log('Object: ' + p);
  }
  pop(vm.ctx); // Pop the ignored receiver.
  push(vm.ctx, MA_NIL);
  return true;
};

//  21: throw
primitives[21] = function() {
  throw new Error('' + self(vm.ctx));
};

//  22: halt
primitives[22] = function() {
  debugger;
  return true;
};


//  25: basicNew:
primitives[25] = function() {
  // (class size) on the stack.
  const size = fromSmallInteger(pop(vm.ctx));
  const cls = pop(vm.ctx);
  push(vm.ctx, mkInstance(cls, size));
  return true;
};

//  26: at:
primitives[26] = function() {
  const index = fromSmallInteger(pop(vm.ctx));
  const array = pop(vm.ctx);
  // Index is a 1-based Smalltalk index.
  push(vm.ctx, readArray(array, index - 1));
  return true;
};

//  27: at:put:   (leaves *value* on the stack).
primitives[27] = function() {
  const value = pop(vm.ctx);
  const index = fromSmallInteger(pop(vm.ctx));
  const array = peek(vm.ctx);
  // Index is a 1-based Smalltalk index.
  writeArray(array, index - 1, value);
  // Read back the value in case of a GC with the above write.
  push(vm.ctx, readArray(pop(vm.ctx), index - 1));
  return true;
};

//  28: array size
primitives[28] = unary((array: ptr) => toSmallInteger(arraySize(array)));

//  29: instance size - currently just the instance variables count for
//  regular classes.
primitives[29] = unary((cls: ptr) =>
    toSmallInteger(behaviorToInstVars(cls)));

//  30: ==
primitives[30] = function() {
  const b = pop(vm.ctx);
  const a = pop(vm.ctx);
  push(vm.ctx, a === b ? MA_TRUE : MA_FALSE);
  return true;
};

//  These are on any SmallInteger:
type binFn<T> = (a: sti, b: sti) => T|null;

function binOp<T>(fn: binFn<T>): T|null {
  const bRaw = pop(vm.ctx);
  if (!isSmallInteger(bRaw)) {
    push(vm.ctx, bRaw);
    return null;
  }
  const a = fromSmallInteger(pop(vm.ctx));
  const b = fromSmallInteger(bRaw);
  return fn(a, b);
}

function numericBinOp(fn: binFn<sti>): Primitive {
  return function() {
    const result = binOp(fn);
    if (result === null) return false;
    push(vm.ctx, toSmallInteger(result));
    return true;
  };
}

function comparisonBinOp(fn: binFn<boolean>): Primitive {
  return function() {
    const result = binOp(fn);
    if (result === null) return false;
    push(vm.ctx, result ? MA_TRUE : MA_FALSE);
    return true;
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
primitives[41] = unary((n: ptr) => toSmallInteger(~fromSmallInteger(n)));

//  42: number toString
primitives[42] = unary((n: ptr) => wrapString('' + fromSmallInteger(n)));

//  43: bitwise shifts
primitives[43] = numericBinOp((a, b) => b >= 0 ? a << b : a >> -b);

// 50: word array size
primitives[50] = unary((array: ptr) => toSmallInteger(wordArraySize(array)));

// 51: wordAt: index
primitives[51] = function() {
  const index = fromSmallInteger(pop(vm.ctx));
  const arr = pop(vm.ctx);
  // 1-based Smalltalk indices
  push(vm.ctx, toSmallInteger(readWordArray(arr, index - 1)));
  return true;
};

// 52: wordAt: index put: aWord  - leaves the *value* on the stack.
primitives[52] = function() {
  const rawValue = pop(vm.ctx);
  const value = fromSmallInteger(rawValue);
  const index = fromSmallInteger(pop(vm.ctx));
  const arr = pop(vm.ctx);
  // 1-based Smalltalk indices
  writeWordArray(arr, index - 1, value);
  push(vm.ctx, rawValue);
  return true;
};

// 53: Character class>>value: - retrieves the Character instance for a
// particular ASCII value.
primitives[53] = function() {
  const num = fromSmallInteger(pop(vm.ctx));
  const asciiTable = readIV(read(classTable(CLS_CHARACTER)), CLASS_VAR1);
  push(vm.ctx, readArray(asciiTable, num));
  return true;
};

