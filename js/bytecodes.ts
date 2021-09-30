import {
  stw, stl, ptr,
  classOf, classTable, hasClass,
  BEHAVIOR_METHODS, BEHAVIOR_SUPERCLASS,
  BLOCK_ARGC, BLOCK_ARGV, BLOCK_PC_START, BLOCK_CONTEXT,
  CTX_METHOD, CTX_LOCALS, CTX_SENDER, CTX_PC, CTX_STACK_INDEX,
  CLASS_NAME, METACLASS_THIS_CLASS,
  CLS_ARRAY, CLS_BLOCK_CLOSURE, CLS_METACLASS,
  METHOD_LITERALS, METHOD_ARGC, METHOD_CLASS, METHOD_LOCALS,
  PROCESS_CONTEXT,
  MA_CLASS_DICT,
  MA_NIL, MA_TRUE, MA_FALSE,
  mkInstance, peek, pop, push,
  asJSString, fromSmallInteger, toSmallInteger,
  read, readArray, readIV,
  writeArray, writeIV, writeIVNew,
} from './memory';
import {ArgumentCountMismatchError} from './errors';
import {lookup, printDict} from './dict';
import {newContext} from './corelib';
import {popContext, readPC} from './process';

// Locals on contexts are an Array, in this order:
// self, args..., locals + block args...
// self is the original receiver the message was actually sent to.
// methodOwner is self, or the superobject corresponding to where the method was
// found.

export function readLocal(ctx: ptr, index: number): ptr {
  const locals = readIV(ctx, CTX_LOCALS); // Pointer to a pointer array.
  return readArray(locals, index);
}

export function readLiteral(ctx: ptr, index: number): ptr {
  const method = readIV(ctx, CTX_METHOD);
  const lits = readIV(method, METHOD_LITERALS); // Pointer to a pointer array.
  return readArray(lits, index);
}

export function writeLocal(ctx: ptr, index: number, value: ptr) {
  const locals = readIV(ctx, CTX_LOCALS); // Pointer to a pointer array.
  writeArray(locals, index, value);
}

// Returns the pointer to self, the 0th local.
// This is the actual receiver, even if the method is defined on a superclass.
export function self(ctx: ptr): ptr {
  return readLocal(ctx, 0);
}


//- `$0: push` A whole family of opcodes that push values onto the interpreter
//  stack.
//    - `$00: push special` Push special values
//        - `$0000: push self`
//        - `$0001: push thisContext`
//        - `$0002: push nil`
//        - `$0003: push true`
//        - `$0004: push false`
//        - Lots of room for expansion here.
//    - `$01: push local` Pushes local whose number is in the operand.
//    - `$02: push instance variable` Pushes instance variable whose number is in
//      the operand.
//        - Note that only this specific class's instance variables are
//          accessible! Superclasses cannot be accessed directly.
//    - `$03: push global` Push a global value looked up in the `SystemDictionary`
//      by the symbol in the literal table at *operand*.
//      table in the VM - see below for that list.
//    - `$04: push standard class` Shorthand for `$03`, pushes a class from a list
//      of common classes in the VM (see below).
//    - `$05: push literal` Push the literal from this method's literals list
//      whose index is in *operand*.
//    - `$06: push inline number` Interpret *operand* as a signed 8-bit value and
//      push it. That gives a range of -128 to 127, which is lots.
//    - `$07: push inline character` Interpret *operand* as an unsigned 8-bit
//      ASCII value and push it as a character. Larger characters can be
//      constructed dynamically with runtime code, or as literals in the array.
function pushOp(process: ptr, ctx: ptr, count: number, operand: number) {
  switch (count) {
    case 0:
      switch (operand) {
        case 0: // Push self
          return push(ctx, self(ctx));
        case 1: // Push thisContext.
          return push(ctx, ctx);
        case 2: // Push nil
          return push(ctx, MA_NIL);
        case 3: // Push true
          return push(ctx, MA_TRUE);
        case 4: // Push false
          return push(ctx, MA_FALSE);
        default:
          throw new Error('unknown special push operand: ' + operand);
      }
      throw new Error('cant happen');

    case 1: // Push local whose number is the operand.
      return push(ctx, readLocal(ctx, operand));
    case 2: // Push instance variable whose number is the operand.
      return push(ctx, readIV(self(ctx), operand));
    case 3: // Push global by looking up the literal symbol in SystemDictionary.
      const dict = read(MA_CLASS_DICT);
      const g = lookup(dict, readLiteral(ctx, operand));
      return push(ctx, g);
    case 4: // Push global by the class index.
      const classIndex = operand;
      return push(ctx, read(classTable(classIndex)));
    case 5: // Push literal generally.
      return push(ctx, readLiteral(ctx, operand));
    case 6: // Push inline number - signed 8-bit operand.
      let n = operand < 128 ? operand : operand - 256;
      return push(ctx, toSmallInteger(n));
    default:
      throw new Error('Unknown push type: ' + count);
  }
  throw new Error('cant happen');
}

function storeOp(process: ptr, ctx: ptr, count: number, operand: number) {
  const value = pop(ctx);
  if (count === 0) { // Store local - operand gives the local number to use.
    writeLocal(ctx, operand, value);
  } else if (count === 1) {
    const me = self(ctx);
    writeIV(me, operand, value);
  } else {
    throw new Error('Invalid store destination ' + count);
  }
}

export function sendOp(process: ptr, ctx: ptr, count: number, selector: ptr,
    isSuper: boolean): ptr {
  // The receiver is on the stack, followed by the arguments: rcvr arg1 arg2...
  // We need to create a new array containing 2 + argc + locals cells.

  // SP points AFTER the last argument.
  // rcvr arg1 arg2 ___
  // 2    3    4    5
  // So SP = 5; we want 2. That's SP - argc - 1.
  const ixReceiver = fromSmallInteger(readIV(ctx, CTX_STACK_INDEX)) - count - 1;
  const receiver = readArray(ctx, ixReceiver);
  // We need to pop those off the stack. Conveniently, the empty-ascending new
  // index is ixReceiver.
  writeIV(ctx, CTX_STACK_INDEX, toSmallInteger(ixReceiver));

  // We need to look up the method we're wanting to call.
  let method = null;

  let cls = classOf(receiver);
  if (isSuper) {
    // Super sends begin the search at the super of the owner of current method.
    cls = readIV(readIV(ctx, CTX_METHOD), METHOD_CLASS);
    cls = readIV(cls, BEHAVIOR_SUPERCLASS);
  }

  while (cls && cls !== MA_NIL) {
    if (classOf(cls) === read(classTable(CLS_METACLASS))) {
      // This is a metaclass - grab its class and get *its* name.
      const theClass = readIV(cls, METACLASS_THIS_CLASS);
      const className = readIV(theClass, CLASS_NAME);
    } else {
      // Otherwise this is a Class, ClassDescription, or Behavior.
      const className = readIV(cls, CLASS_NAME);
    }

    const dict = readIV(cls, BEHAVIOR_METHODS);
    const found = lookup(dict, selector);
    if (found && found !== MA_NIL) {
      method = found;
      break;
    }

    // Didn't find it, so escalate to the superclass.
    cls = readIV(cls, BEHAVIOR_SUPERCLASS);
  }

  if (!method) {
    const className = readIV(classOf(receiver), CLASS_NAME);
    throw new Error('failed to look up method ' + asJSString(className) +
        ' >> #' + asJSString(selector));
  }

  // Now we have all the pieces: receiver, target method, the superobject and
  // class where we found it, arguments, and local count.
  // Check that the argument count matches the method.
  const expectedArgs = fromSmallInteger(readIV(method, METHOD_ARGC));
  if (expectedArgs !== count) {
    throw new ArgumentCountMismatchError('FIXME', asJSString(selector),
        expectedArgs, count);
  }

  // Get the number of locals needed.
  const nLocals = fromSmallInteger(readIV(method, METHOD_LOCALS));
  // Create an array of 1 + argc + nLocals.
  const locals = mkInstance(read(classTable(CLS_ARRAY)), 1 + count + nLocals);
  writeArray(locals, 0, receiver);
  for (let i = 0; i < count; i++) {
    const arg = readArray(ctx, ixReceiver + i + 1);
    writeArray(locals, 1 + i, arg);
  }
  // Locals are already initialized to nil.

  // Create a context.
  const newCtx = newContext(method, ctx, locals);

  // Our new context is ready. Push it onto the current process as the new top.
  writeIV(process, PROCESS_CONTEXT, newCtx);
  return newCtx;
}

// Begins a block, whose bytecodes are inlined after this one.
// The *count* gives the argument count of the block; *operand* its argument
// start index. The **next** word gives the count of bytecodes following.
//
// Note that we can't give a pointer directly to the bytecodes in the block,
// and since we don't want to copy them either, we will have to leverage the
// containing method when running a block. To facilitate that, the BlockClosure
// stores a pcStart (a new value not in the original JS implementation).
function startBlock(process: ptr, ctx: ptr, argc: number, argStart: number) {
  const len = readPC(ctx); // PC now points at the first opcode of the block.

  const block = mkInstance(read(classTable(CLS_BLOCK_CLOSURE)));
  writeIV(block, BLOCK_ARGC, toSmallInteger(argc));
  // Index into the parent locals where my args and locals are.
  writeIV(block, BLOCK_ARGV, toSmallInteger(argStart));
  // No need to read and reallocate numbers, this can be a pointer copy if
  // they're pointers.
  writeIV(block, BLOCK_PC_START, readIV(ctx, CTX_PC));

  // Need to find the containing context. That's ctx if this is a top-level
  // block. If ctx is a block, though, pull its method field.
  const parentCtx = hasClass(ctx, CLS_BLOCK_CLOSURE) ?
      readIV(ctx, CTX_METHOD) : ctx;

  // Now parentCtx is really the containing method's context, which holds the
  // locals.
  // Is this an old -> new store? No.
  // Proof: the parent method must have been called before the block, so the
  // parentCtx is older than this new BlockClosure, this always a new -> new or
  // new -> old pointer, even if a BlockClosure long outlives the original call.
  writeIVNew(block, BLOCK_CONTEXT, parentCtx);

  // Final steps:
  // Advance PC past the block's code.
  const pc = fromSmallInteger(readIV(ctx, CTX_PC));
  writeIV(ctx, CTX_PC, toSmallInteger(pc + len));

  // And push the new block onto the stack.
  push(ctx, block);
}



export function answer(process: ptr, ctx: ptr, value: ptr) {
  // We need to pop the sender context to the top of the process chain, and push
  // the value onto its stack.
  // Special case: top-level return to an empty chain. If the sender context is
  // nil, don't try to push!
  const parentCtx = readIV(ctx, CTX_SENDER);
  popContext(process);
  if (parentCtx !== MA_NIL) {
    push(parentCtx, value); // Can be an old -> new store, but push() checks.
  }
}

function blockAnswer(process: ptr, ctx: ptr) {
  // Block returns are only legal if the block's containing method is on the
  // context chain. If the sender chain doesn't contain our parent method's
  // context, then this is an error. (It's legal to be in that situation, but
  // not to try to do a block return from it.)
  const block = readIV(ctx, CTX_METHOD);
  const container = readIV(block, BLOCK_CONTEXT);
  let chain = readIV(ctx, CTX_SENDER);
  while (true) {
    // Good case: this ancestor is the containing method.
    if (chain === container) break;
    chain = readIV(chain, CTX_SENDER);
    // Bad case: we've run out of senders and failed to find the method.
    if (!chain || chain === MA_NIL) {
      throw new Error(
          'Non-local block return - containing method no longer on the stack');
    }
  }

  // If we got to here, we know this is a legal block return.
  // Push the value to the containing method's *parent's* stack, then pop to
  // be running that context.
  const targetCtx = readIV(container, CTX_SENDER);
  push(targetCtx, pop(ctx));
  popContext(process, targetCtx);
}

function skip(process: ptr, ctx: ptr, delta: number) {
  const pc = fromSmallInteger(readIV(ctx, CTX_PC));
  writeIV(ctx, CTX_PC, toSmallInteger(pc + delta));
}

function maybeSkip(process: ptr, ctx: ptr, on: ptr, toPush: ptr, delta: number) {
  const tos = pop(ctx);
  if (tos === on) {
    push(ctx, toPush);
    skip(process, ctx, delta);
  }
  // If it wasn't on, then continue normally and don't push.
}

function miscOp(process: ptr, ctx: ptr, count: number, operand: number) {
  switch (count) {
    case 0: // DUP
      return push(ctx, peek(ctx));
    case 1: // DROP
      return pop(ctx);
    case 2: // answer TOS
      return answer(process, ctx, pop(ctx));
    case 3: // answer self
      return answer(process, ctx, self(ctx));
    case 4: // block answer TOS
      return blockAnswer(process, ctx);

    case 8: // Skip true push nil
      return maybeSkip(process, ctx, MA_TRUE, MA_NIL, operand);
    case 9: // Skip false push nil
      return maybeSkip(process, ctx, MA_TRUE, MA_NIL, operand);
    case 10: // Skip forward unconditionally.
      return skip(process, ctx, operand);
    case 11: // Skip backward unconditionally.
      return skip(process, ctx, -operand);
    case 12: // Skip true push true
      return maybeSkip(process, ctx, MA_TRUE, MA_TRUE, operand);
    case 13: // Skip false push false
      return maybeSkip(process, ctx, MA_TRUE, MA_TRUE, operand);
  }
  throw new Error('unknown misc op ' + count);
}

export type Primitive = (process: ptr, ctx: ptr) => void;
export const primitives: {[key: number]: Primitive} = {};

function primitiveOp(process: ptr, ctx: ptr, count: number, operand: number) {
  const prim = primitives[operand];
  if (!prim) throw new Error('unknown primitive');

  prim(process, ctx);
  // Primitives that fail just return to the Smalltalk code, often a fallback
  // implementation. Primitives that succeed have returned already.
  // Either way there's nothing more to be done here; the failed flag is
  // ignored(?)
}

export function execute(process: ptr, ctx: ptr, bc: stw) {
  // First, we split the bytecode into the three fields.
  const op = (bc >> 12) & 0xf;
  const count = (bc >> 8) & 0xf;
  const operand = bc & 0xff;

  switch (op) {
    case 0: return pushOp(process, ctx, count, operand);
    case 1: return storeOp(process, ctx, count, operand);
    case 2: return sendOp(process, ctx, count, readLiteral(ctx, operand), false);
    case 3: return sendOp(process, ctx, count, readLiteral(ctx, operand), true);
    case 4: throw new Error('canned send is not implemented yet');
    case 5: return startBlock(process, ctx, count, operand);
    case 6: return miscOp(process, ctx, count, operand);
    case 7: return primitiveOp(process, ctx, count, operand);
    default:
      throw new Error('Cannot understand bytecode: ' + bc);
  }
}

