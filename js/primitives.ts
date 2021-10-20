// Primitive list.
// $FF isn't allowed; it's reserved for expansion.
import {Primitive, answer, primitives, readLocal, self, sendOp} from './bytecodes';
import {defClass} from './bootstrap';
import {BlockArgumentCountMismatchError} from './errors';
import {
  sti, ptr,
  arraySize, behaviorToInstVars, classOf, identityHash,
  MA_NEXT_CLASS_INDEX, MA_NIL, MA_TRUE, MA_FALSE, MA_GLOBALS,
  CLS_CHARACTER, CLS_CONTEXT, CLS_STRING, CLS_SYMBOL, CLS_ARRAY,
  BLOCK_CONTEXT, BLOCK_ARGV, BLOCK_ARGC, BLOCK_PC_START,
  CTX_LOCALS, CTX_SENDER, CTX_PC, CTX_METHOD, CTX_STACK_INDEX,
  COLOR_STRING, POINT_X, POINT_Y, RECT_ORIGIN,
  CLASS_VAR1,
  LINKED_LIST_HEAD, LINKED_LIST_TAIL,
  PROCESS_LINK, PROCESS_MY_LIST, PROCESS_SUSPENDED_CONTEXT, PROCESS_PRIORITY,
  PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES,
  classTable, hasClass, fromSmallInteger, toSmallInteger, isSmallInteger,
  asJSString, mkInstance, push, pop, peek,
  read, readIV, readArray,
  write, writeIV, writeArray, writeArrayNew, writeIVNew,
  readWordArray, writeWordArray, wordArraySize,
  gcTemps, gcRelease, seq,
  wrapString, wrapSymbol,
} from './memory';
import {SYM_PROCESSOR} from './corelib';
import {lookup} from './dict';
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
  const args = gcTemps(argc);
  for (let i = 0; i < argc; i++) {
    args[i] = pop(vm.ctx);
  }
  const selector = pop(vm.ctx);
  for (let i = 0; i < argc; i++) {
    push(vm.ctx, args[i]);
  }
  sendOp(argc, selector, false);
  gcRelease(args);
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
  const ptrs = gcTemps(1); // argv
  ptrs[0] = pop(vm.ctx);
  const selector = pop(vm.ctx);
  const argc = arraySize(ptrs[0]);
  for (let i = 0; i < argc; i++) {
    push(vm.ctx, readArray(ptrs[0], i));
  }
  sendOp(argc, selector, false);
  gcRelease(ptrs);
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
  const ARGV = 4;
  const [v_newCtx, v_outerCtx, v_locals, v_block] = seq(ARGV);
  const ptrs = gcTemps(ARGV + argc); // newCtx, outerCtx, locals, block, args...
  ptrs[v_newCtx] = mkInstance(read(classTable(CLS_CONTEXT)), 19);
  for (let i = argc - 1; i >= 0; i--) {
    ptrs[i + ARGV] = pop(vm.ctx);
  }
  ptrs[v_block] = pop(vm.ctx);
  const argcWanted = fromSmallInteger(readIV(ptrs[v_block], BLOCK_ARGC));
  if (argc !== argcWanted) {
    throw new BlockArgumentCountMismatchError(argcWanted, argc);
  }

  ptrs[v_outerCtx] = readIV(ptrs[v_block], BLOCK_CONTEXT);
  ptrs[v_locals] = readIV(ptrs[v_outerCtx], CTX_LOCALS);

  // These writes are safe because the mkInstance was the last alloc and so it's
  // gotta be in Eden.
  writeIV(ptrs[v_newCtx], CTX_PC, readIV(ptrs[v_block], BLOCK_PC_START));
  writeIV(ptrs[v_newCtx], CTX_SENDER, vm.ctx);
  writeIV(ptrs[v_newCtx], CTX_METHOD, ptrs[v_block]);
  writeIV(ptrs[v_newCtx], CTX_STACK_INDEX, toSmallInteger(0));
  writeIV(ptrs[v_newCtx], CTX_LOCALS, ptrs[v_locals]);

  const argvIndex = fromSmallInteger(readIV(ptrs[v_block], BLOCK_ARGV));
  for (let i = 0; i < argc; i++) {
    writeArray(ptrs[v_locals], argvIndex + i, ptrs[ARGV + i]);
  }

  pushContext(ptrs[v_newCtx]); // Execute into the new stack frame.
  gcRelease(ptrs);
  // NB: Running a v_newCtxlock consumes all the values off the stack; it will
  // ultimately pusv_newCtx the result of the block.
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
// TODO: This could be an array lookup on a class variable, once those are
// sorted out.
primitives[53] = function() {
  const num = fromSmallInteger(pop(vm.ctx));
  const asciiTable = readIV(read(classTable(CLS_CHARACTER)), CLASS_VAR1);
  push(vm.ctx, readArray(asciiTable, num));
  return true;
};



// 54: Process>>resume
// Allows the receiver (a Process) to continue. Put the receiver in line to
// become the activeProcess. Fail if the receiver is already waiting in a queue
// (like a Semaphore or ProcessorScheduler). Fail if the receiver's
// suspendedContext is nil, since there's nowhere to return to.
primitives[54] = function() {
  const [v_proc, v_scheduler, v_list, v_tail] = seq(4);
  const ptrs = gcTemps(4);
  ptrs[v_proc] = peek(vm.ctx);
  if (readIV(ptrs[v_proc], PROCESS_MY_LIST) !== MA_NIL) return false;
  if (readIV(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT) === MA_NIL) return false;
  ptrs[v_scheduler] = lookup(read(MA_GLOBALS), SYM_PROCESSOR);
  ptrs[v_list] = readArray(
      readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES),
      fromSmallInteger(readIV(ptrs[v_proc], PROCESS_PRIORITY)) - 1);

  writeIVNew(ptrs[v_proc], PROCESS_LINK, MA_NIL);
  ptrs[v_tail] = readIV(ptrs[v_list], LINKED_LIST_TAIL);
  if (ptrs[v_tail] === MA_NIL) {
    writeIV(ptrs[v_list], LINKED_LIST_HEAD, ptrs[v_proc]);
    writeIV(ptrs[v_list], LINKED_LIST_TAIL, ptrs[v_proc]);
  } else {
    writeIV(ptrs[v_tail], PROCESS_LINK, ptrs[v_proc]);
    writeIV(ptrs[v_list], LINKED_LIST_TAIL, ptrs[v_proc]);
  }
  gcRelease(ptrs);
  return true;
};

// 55: Process>>suspend
// Stop the process that the receiver represents, in such a way that it can be
// resumed later with #resume. If the receiver represents the activeProcess,
// suspend it. Otherwise remove the receiver from the list of waiting processes.
// The return value of this method is the list the receiver was previously on
// (if any).
// Suspended processes are in no list.
primitives[55] = function() {
  const [v_proc, v_list] = seq(2);
  const ptrs = gcTemps(2);
  ptrs[v_proc] = pop(vm.ctx);
  ptrs[v_list] = readIV(ptrs[v_proc], PROCESS_MY_LIST);

  // If we're in a list, rig up a call to removeLink:
  push(vm.ctx, ptrs[v_list]);
  push(vm.ctx, ptrs[v_proc]);
  sendOp(1, wrapSymbol('removeLink:'), false);
  // proc on the stack.

  writeIVNew(ptrs[v_proc], PROCESS_MY_LIST, MA_NIL);
  writeIVNew(ptrs[v_proc], PROCESS_LINK, MA_NIL);

  // Need to put the list on the stack, or nil.
  gcRelease(ptrs);
  return true;
};



function point(p: ptr): [number, number] {
  return [
    fromSmallInteger(readIV(p, POINT_X)),
    fromSmallInteger(readIV(p, POINT_Y)),
  ];
}

declare global {
  var __ui: CanvasRenderingContext2D;
}

// 60: Canvas>>privDrawPolygon:color:borderWidth:borderColor:
primitives[60] = function() {
  globalThis.__ui.strokeStyle = asJSString(readIV(pop(vm.ctx), COLOR_STRING));
  globalThis.__ui.lineWidth = fromSmallInteger(pop(vm.ctx));
  globalThis.__ui.fillStyle = asJSString(readIV(pop(vm.ctx), COLOR_STRING));

  const verts = pop(vm.ctx);
  const len = arraySize(verts);
  if (len < 2) return false;
  globalThis.__ui.beginPath();
  globalThis.__ui.moveTo.apply(__ui, point(readArray(verts, 0)));
  for (let i = 1; i < len; i++) {
    globalThis.__ui.lineTo.apply(__ui, point(readArray(verts, i)));
  }
  globalThis.__ui.closePath();
  globalThis.__ui.stroke();
  globalThis.__ui.fill();

  push(vm.ctx, self(vm.ctx));
  return true;
};

// 61: Canvas>>privFillRectX:y:width:height:color:
primitives[61] = function() {
  globalThis.__ui.fillStyle = asJSString(readIV(pop(vm.ctx), COLOR_STRING));
  const h = fromSmallInteger(pop(vm.ctx));
  const w = fromSmallInteger(pop(vm.ctx));
  const y = fromSmallInteger(pop(vm.ctx));
  const x = fromSmallInteger(pop(vm.ctx));
  globalThis.__ui.fillRect(x, y, w, h);
  return true;
};

// 62: Canvas>>privMeasureText:font:
primitives[62] = function() {
  globalThis.__ui.font = asJSString(pop(vm.ctx));
  const fm = globalThis.__ui.measureText(asJSString(pop(vm.ctx)));
  const ascent = Math.ceil(fm.actualBoundingBoxAscent);
  const descent = Math.ceil(fm.actualBoundingBoxDescent);
  const width = Math.ceil(fm.width);
  const extent = mkInstance(read(classTable(CLS_ARRAY)), 2);
  writeArrayNew(extent, 0, toSmallInteger(width));
  writeArrayNew(extent, 1, toSmallInteger(ascent + descent));
  push(vm.ctx, extent);
  return true;
};

// 63: Canvas>>drawString:from:to:in:font:color:
primitives[63] = function() {
  globalThis.__ui.fillStyle = asJSString(readIV(pop(vm.ctx), COLOR_STRING));
  const font = pop(vm.ctx);
  globalThis.__ui.font = font === MA_NIL ? '10px sans-serif' : asJSString(font);
  const origin = readIV(pop(vm.ctx), RECT_ORIGIN);

  // These are Smalltalk indices.
  const end = fromSmallInteger(pop(vm.ctx));
  const start = fromSmallInteger(pop(vm.ctx));
  const s = asJSString(pop(vm.ctx));
  const x = fromSmallInteger(readIV(origin, POINT_X));
  const y = fromSmallInteger(readIV(origin, POINT_Y));
  push(vm.ctx, self(vm.ctx));

  // The Smalltalk y value is the top corner, but fillText draws from
  // the baseline. So we measure and adjust y by the ascender.
  const text = s.substring(start - 1, end);
  const fm = globalThis.__ui.measureText(text);
  globalThis.__ui.fillText(text, x, y + fm.actualBoundingBoxAscent);
  return true;
};

