BYTECODE_HANDLERS.pushLocal = function(ar, bc) {
  ar.stack().push(ar.getLocal(bc.index || 0));
};

BYTECODE_HANDLERS.pushGlobal = function(ar, bc) {
  const g = classes[bc.name];
  if (!g) {
    throw new Error('Unknown global ' + bc.name);
  }
  ar.stack().push(g);
};

BYTECODE_HANDLERS.pushSelf = function(ar, bc) {
  ar.stack().push(ar.getLocal(0));
};

BYTECODE_HANDLERS.pushInstVar = function(ar, bc) {
  ar.stack().push(ar.self().$vars[bc.index || 0]);
};

BYTECODE_HANDLERS.pushNumber = function(ar, bc) {
  ar.stack().push(wrapNumber(bc.value || 0));
};

BYTECODE_HANDLERS.pushString = function(ar, bc) {
  ar.stack().push(wrapString(bc.name || ''));
};

BYTECODE_HANDLERS.pushBool = function(ar, bc) {
  ar.stack().push(bc.super ? classes['true'] : classes['false']);
};

BYTECODE_HANDLERS.pushNil = function(ar, bc) {
  ar.stack().push(stNil);
};

BYTECODE_HANDLERS.pushContext = function(ar, bc) {
  ar.stack().push(ar.context());
};

BYTECODE_HANDLERS.storeLocal = function(ar, bc) {
  ar.setLocal(bc.index || 0, ar.stack().pop());
};

BYTECODE_HANDLERS.storeInstVar = function(ar, bc) {
  ar.self().$vars[bc.index || 0] = ar.stack().pop();
};

BYTECODE_HANDLERS.startBlock = function(ar, bc) {
  // Bytecode gives argc, temps, argStart, and length (in bytecodes).
  // The current PC is the start.
  // We construct a BlockClosure, push it, and move the outer PC.
  const closure = mkInstance(classes['BlockClosure']);
  const pc = ar.pc();
  closure.$vars[CLOSURE_BYTECODE] =
      ar.bytecode().slice(pc, pc + (bc.length || 0));
  closure.$vars[CLOSURE_ARGC] = wrapNumber(bc.argc || 0);
  closure.$vars[CLOSURE_LOCALS] = wrapNumber(bc.temps || 0);
  closure.$vars[CLOSURE_ARGV_START] = wrapNumber(bc.argStart);

  // We're either creating a top-level block or a nested block.
  // If top-level, ar represents the containing method.
  // If nested, ar is the containing block, grab the method from it.
  closure.$vars[CLOSURE_METHOD_RECORD] =
      ar.method().$class === classes['BlockClosure'] ?
          ar.method().$vars[CLOSURE_METHOD_RECORD] : ar.context();

  ar.stack().push(closure);
  ar.pcBump(bc.length || 0);
};


BYTECODE_HANDLERS.send = function(ar, bc) {
  // The receiver is on the stack followed by its arguments: rcvr arg1 arg2...
  const argc = bc.argc || 0;
  const ixReceiver = ar.stack().length - argc - 1;
  const receiver = ar.stack()[ixReceiver];
  const args = ar.stack().splice(ixReceiver); // Removes them from the original, returns the removed items.

  // Args includes the receiver, so we slice it off for this call.
  send(ar, receiver, bc.selector, args.slice(1), bc.super);
  // That pushes the new AR onto the thread, so execution will continue in the
  // called method.
};

BYTECODE_HANDLERS.dup = function(ar, bc) {
  ar.stack().push(ar.stack()[ar.stack().length - 1]);
};

BYTECODE_HANDLERS.drop = function(ar, bc) {
  ar.stack().pop();
};

BYTECODE_HANDLERS.answer = function(ar, bc) {
  // Pop the top of this ar's stack, and push it onto the parent's.
  ar.thread().pop();
  ar.parent().stack().push(ar.stack().pop());
};

BYTECODE_HANDLERS.answerBlock = function(ar, bc) {
  // Block returns are only legal if the block's containing method is on the AR
  // stack. If the parent() chain doesn't contain ar.blockContext() then we need
  // to throw an error for non-local block returns.
  const container = new ActivationRecord(
      ar.thread(), ar.method().$vars[CLOSURE_METHOD_RECORD]);
  let chain = ar.parent();
  while (true) {
    // Good case: this ancestor is the containing method.
    if (chain.equals(container)) break;
    chain = chain.parent();
    // Bad case: we've run out of parents and failed to find the method.
    if (!chain || chain === stNil) {
      throw new Error(
          'Non-local block return - containing method no longer on the stack');
    }
  }

  // If we got to here, we know this is a legal block return.
  // Push the value to the blockContext's *parent's* stack, then pop to it.
  container.parent().stack().push(ar.stack().pop());
  ar.thread().popTo(container.parent());
};

BYTECODE_HANDLERS.answerSelf = function(ar, bc) {
  ar.thread().pop();
  ar.parent().stack().push(ar.self());
};

BYTECODE_HANDLERS.primitive = function(ar, bc) {
  if (bc.keyword === 'builtin:') {
    const builtin = builtins[bc.name];
    if (!builtin) {
      throw new UnknownBuiltinError(bc.name);
    }
    builtin(ar);
  } else {
    throw new UnknownBuiltinError(bc.keyword + ': ' + bc.name);
  }
};

