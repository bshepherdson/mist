import {
  stw, stl, ptr,
  classOf, classTable, hasClass, methodFor,
  BEHAVIOR_METHODS, BEHAVIOR_SUPERCLASS,
  BLOCK_ARGC, BLOCK_ARGV, BLOCK_PC_START, BLOCK_CONTEXT,
  CTX_METHOD, CTX_LOCALS, CTX_SENDER, CTX_PC, CTX_STACK_INDEX,
  CLASS_NAME, METACLASS_THIS_CLASS,
  CLS_ARRAY, CLS_BLOCK_CLOSURE, CLS_METACLASS,
  METHOD_LITERALS, METHOD_ARGC, METHOD_CLASS, METHOD_LOCALS, METHOD_BYTECODE,
  PROCESS_CONTEXT,
  MA_CLASS_DICT,
  MA_NIL, MA_TRUE, MA_FALSE,
  mkInstance, peek, pop, push,
  asJSString, fromSmallInteger, toSmallInteger,
  read, readArray, readIV, readWordArray,
  writeArray, writeIV, writeIVNew,
  gcTemps, gcRelease, seq,
} from './memory';
import {ArgumentCountMismatchError} from './errors';
import {lookup, printDict} from './dict';
import {newContext} from './corelib';
import {vm} from './vm';
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
  const method = methodFor(ctx);
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
function pushOp(count: number, operand: number) {
  switch (count) {
    case 0:
      switch (operand) {
        case 0: // Push self
          return push(vm.ctx, self(vm.ctx));
        case 1: // Push thisContext.
          return push(vm.ctx, vm.ctx);
        case 2: // Push nil
          return push(vm.ctx, MA_NIL);
        case 3: // Push true
          return push(vm.ctx, MA_TRUE);
        case 4: // Push false
          return push(vm.ctx, MA_FALSE);
        default:
          throw new Error('unknown special push operand: ' + operand);
      }
      throw new Error('cant happen');

    case 1: // Push local whose number is the operand.
      return push(vm.ctx, readLocal(vm.ctx, operand));
    case 2: // Push instance variable whose number is the operand.
      return push(vm.ctx, readIV(self(vm.ctx), operand));
    case 3: // Push global by looking up the literal symbol in SystemDictionary.
      const dict = read(MA_CLASS_DICT);
      const g = lookup(dict, readLiteral(vm.ctx, operand));
      return push(vm.ctx, g);
    case 4: // Push global by the class index.
      const classIndex = operand;
      return push(vm.ctx, read(classTable(classIndex)));
    case 5: // Push literal generally.
      return push(vm.ctx, readLiteral(vm.ctx, operand));
    case 6: // Push inline number - signed 8-bit operand.
      let n = operand < 128 ? operand : operand - 256;
      return push(vm.ctx, toSmallInteger(n));
    default:
      throw new Error('Unknown push type: ' + count);
  }
  throw new Error('cant happen');
}

function storeOp(count: number, operand: number) {
  const value = pop(vm.ctx);
  if (count === 0) { // Store local - operand gives the local number to use.
    writeLocal(vm.ctx, operand, value);
  } else if (count === 1) {
    const me = self(vm.ctx);
    writeIV(me, operand, value);
  } else {
    throw new Error('Invalid store destination ' + count);
  }
}

export function sendOp(count: number, selector: ptr, isSuper: boolean): void {
  // The receiver is on the stack, followed by the arguments: rcvr arg1 arg2...
  // We need to create a new array containing 2 + argc + locals cells.

  // SP points AFTER the last argument.
  // rcvr arg1 arg2 ___
  // 2    3    4    5
  // So SP = 5; we want 2. That's SP - argc - 1.
  const ixReceiver = fromSmallInteger(readIV(vm.ctx, CTX_STACK_INDEX)) - count - 1;
  const [v_receiver, v_method, v_locals] = seq(3);
  const ptrs = gcTemps(3); // receiver, method, locals
  ptrs[v_receiver] = readArray(vm.ctx, ixReceiver);

  // We need to look up the method we're wanting to call.
  let cls = classOf(ptrs[v_receiver]);
  if (isSuper) {
    // Super sends begin the search at the super of the owner of current method.
    cls = readIV(methodFor(vm.ctx), METHOD_CLASS);
    cls = readIV(cls, BEHAVIOR_SUPERCLASS);
  }

  while (cls && cls !== MA_NIL) {
    const dict = readIV(cls, BEHAVIOR_METHODS);
    //printDict(dict);
    const found = lookup(dict, selector);
    if (found && found !== MA_NIL) {
      ptrs[v_method] = found;
      break;
    }

    // Didn't find it, so escalate to the superclass.
    cls = readIV(cls, BEHAVIOR_SUPERCLASS);
  }

  if (!ptrs[v_method]) {
    const receiverClass = classOf(ptrs[v_receiver]);
    let className;
    if (hasClass(receiverClass, CLS_METACLASS)) {
      className = asJSString(readIV(ptrs[v_method], CLASS_NAME)) + ' class';
    } else {
      className = asJSString(readIV(receiverClass, CLASS_NAME));
    }
    throw new Error('failed to look up method ' + className +
        ' >> #' + asJSString(selector));
  }

  // Now we have all the pieces: receiver, target method, the superobject and
  // class where we found it, arguments, and local count.
  // Check that the argument count matches the method.
  const expectedArgs = fromSmallInteger(readIV(ptrs[v_method], METHOD_ARGC));
  if (expectedArgs !== count) {
    throw new ArgumentCountMismatchError('FIXME', asJSString(selector),
        expectedArgs, count);
  }

  // Now there are two cases: primitive or proper call.
  const firstBC = readWordArray(readIV(ptrs[v_method], METHOD_BYTECODE), 0);
  const hasPrimitive = (firstBC & 0xf000) === 0x7000;

  if (hasPrimitive) {
    // No locals array or stack frame here: we call it directly while the
    // arguments are on the stack.
    const prim = primitives[firstBC&0xff];
    if (!prim) throw new Error('unknown primitive');

    const success = prim();
    // Primitives that fail do actually execute the Smalltalk code. It's usually
    // a fallback implementation. Primitives that succeed have done their work,
    // including consuming the inputs on the stack.
    if (success) {
      gcRelease(ptrs);
      return;
    }
  }

  // We need to pop those off the stack. Conveniently, the empty-ascending new
  // index is ixReceiver.
  writeIV(vm.ctx, CTX_STACK_INDEX, toSmallInteger(ixReceiver));

  // Get the number of locals needed.
  const nLocals = fromSmallInteger(readIV(ptrs[v_method], METHOD_LOCALS));
  // Create an array of 1 + argc + nLocals.
  ptrs[v_locals] = mkInstance(read(classTable(CLS_ARRAY)), 1 + count + nLocals);
  // Re-fetching the receiver in case it's moved.
  writeArray(ptrs[v_locals], 0, readArray(vm.ctx, ixReceiver));
  for (let i = 0; i < count; i++) {
    const arg = readArray(vm.ctx, ixReceiver + i + 1);
    writeArray(ptrs[v_locals], 1 + i, arg);
  }
  // Locals are already initialized to nil.

  // Create a context.
  vm.ctx = newContext(ptrs[v_method], vm.ctx, ptrs[v_locals], hasPrimitive);
  gcRelease(ptrs);

  // Our new context is ready. Push it onto the current process as the new top.
  writeIV(vm.runningProcess, PROCESS_CONTEXT, vm.ctx);
}

// Begins a block, whose bytecodes are inlined after this one.
// The *count* gives the argument count of the block; *operand* its argument
// start index. The **next** word gives the count of bytecodes following.
//
// Note that we can't give a pointer directly to the bytecodes in the block,
// and since we don't want to copy them either, we will have to leverage the
// containing method when running a block. To facilitate that, the BlockClosure
// stores a pcStart (a new value not in the original JS implementation).
function startBlock(argc: number, argStart: number) {
  const len = readPC(vm.ctx); // PC now points at the first opcode of the block.

  const [v_block] = seq(1);
  const ptrs = gcTemps(1);
  ptrs[v_block] = mkInstance(read(classTable(CLS_BLOCK_CLOSURE)));
  writeIV(ptrs[v_block], BLOCK_ARGC, toSmallInteger(argc));
  // Index into the parent locals where my args and locals are.
  writeIV(ptrs[v_block], BLOCK_ARGV, toSmallInteger(argStart));
  // No need to read and reallocate numbers, this can be a pointer copy if
  // they're pointers.
  writeIV(ptrs[v_block], BLOCK_PC_START, readIV(vm.ctx, CTX_PC));

  // Need to find the containing context. That's ctx if this is a top-level
  // block. If ctx is a block, though, pull its method field.
  const methodOrBlock = readIV(vm.ctx, CTX_METHOD);
  const parentCtx = hasClass(methodOrBlock, CLS_BLOCK_CLOSURE) ?
      readIV(methodOrBlock, BLOCK_CONTEXT) : vm.ctx;

  // Now parentCtx is really the containing method's context, which holds the
  // locals.
  // Is this an old -> new store? No.
  // Proof: the parent method must have been called before the block, so the
  // parentCtx is older than this new BlockClosure, this always a new -> new or
  // new -> old pointer, even if a BlockClosure long outlives the original call.
  writeIVNew(ptrs[v_block], BLOCK_CONTEXT, parentCtx);

  // Final steps:
  // Advance PC past the block's code.
  const pc = fromSmallInteger(readIV(vm.ctx, CTX_PC));
  writeIV(vm.ctx, CTX_PC, toSmallInteger(pc + len));

  // And push the new block onto the stack.
  push(vm.ctx, ptrs[v_block]);
  gcRelease(ptrs);
}

export function answer(value: ptr) {
  // We need to pop the sender context to the top of the process chain, and push
  // the value onto its stack.
  // Special case: top-level return to an empty chain. If the sender context is
  // nil, don't try to push!
  const [v_parentCtx, v_value] = seq(2);
  const ptrs = gcTemps(2); // parentCtx, value
  ptrs[v_parentCtx] = readIV(vm.ctx, CTX_SENDER);
  ptrs[v_value] = value;
  popContext();
  if (ptrs[v_parentCtx] !== MA_NIL) {
    push(ptrs[v_parentCtx], ptrs[v_value]); // Can be an old -> new store, but push() checks.
  }
  gcRelease(ptrs);
}

function blockAnswer() {
  // Block returns are only legal if the block's containing method is on the
  // context chain. If the sender chain doesn't contain our parent method's
  // context, then this is an error. (It's legal to be in that situation, but
  // not to try to do a block return from it.)
  const block = readIV(vm.ctx, CTX_METHOD);
  const container = readIV(block, BLOCK_CONTEXT);
  let chain = readIV(vm.ctx, CTX_SENDER);
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
  const [v_sender] = seq(1);
  const ptrs = gcTemps(1);
  ptrs[v_sender] = readIV(container, CTX_SENDER);
  push(ptrs[v_sender], pop(vm.ctx));
  popContext(ptrs[v_sender]);
  vm.ctx = ptrs[v_sender];
  gcRelease(ptrs);
}

function skip(delta: number) {
  const pc = fromSmallInteger(readIV(vm.ctx, CTX_PC));
  writeIVNew(vm.ctx, CTX_PC, toSmallInteger(pc + delta));
}

function maybeSkip(on: ptr, toPush: ptr, delta: number) {
  const [v_on, v_toPush] = seq(2);
  const ptrs = gcTemps(2);
  ptrs[v_on] = on;
  ptrs[v_toPush] = toPush;
  if (pop(vm.ctx) === ptrs[v_on]) {
    push(vm.ctx, ptrs[v_toPush]);
    skip(delta);
  }
  gcRelease(ptrs);
  // If it wasn't on, then continue normally and don't push.
}

function miscOp(count: number, operand: number) {
  switch (count) {
    case 0: // DUP
      return push(vm.ctx, peek(vm.ctx));
    case 1: // DROP
      return pop(vm.ctx);
    case 2: // answer TOS
      return answer(pop(vm.ctx));
    case 3: // answer self
      return answer(self(vm.ctx));
    case 4: // block answer TOS
      return blockAnswer();

    case 8: // Skip true push nil
      return maybeSkip(MA_TRUE, MA_NIL, operand);
    case 9: // Skip false push nil
      return maybeSkip(MA_FALSE, MA_NIL, operand);
    case 10: // Skip forward unconditionally.
      return skip(operand);
    case 11: // Skip backward unconditionally.
      return skip(-operand);
    case 12: // Skip true push true
      return maybeSkip(MA_TRUE, MA_TRUE, operand);
    case 13: // Skip false push false
      return maybeSkip(MA_FALSE, MA_FALSE, operand);
  }
  throw new Error('unknown misc op ' + count);
}

export type Primitive = () => boolean;
export const primitives: {[key: number]: Primitive} = {};

function primitiveOp(count: number, operand: number) {
  // Actual primitives aren't supposed to be called!
  throw new Error('actually tried to execute primitive ' + operand);
}

export function execute(bc: stw) {
  // First, we split the bytecode into the three fields.
  const op = (bc >> 12) & 0xf;
  const count = (bc >> 8) & 0xf;
  const operand = bc & 0xff;

  switch (op) {
    case 0: return pushOp(count, operand);
    case 1: return storeOp(count, operand);
    case 2: return sendOp(count, readLiteral(vm.ctx, operand), false);
    case 3: return sendOp(count, readLiteral(vm.ctx, operand), true);
    case 4: throw new Error('canned send is not implemented yet');
    case 5: return startBlock(count, operand);
    case 6: return miscOp(count, operand);
    case 7: return primitiveOp(count, operand);
    default:
      throw new Error('Cannot understand bytecode: ' + bc);
  }
}

