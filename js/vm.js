// Virtual machine types and code.

// Activation record
// This gives the stack, PC, locals and arguments for method execution.
// {
//   bytecode: Array of bytecode we're accessing.
//   pc: Index into the bytecode array.
//   locals: Array containing [self, args..., temps...]
//   parent: Activation record for the parent to return to.
//   thread: Thread I'm running under.
//   stack: The stack of Smalltalk objects in the VM's working space.
// }

function activationRecord(from, locals, self, args, bytecode, pc) {
  const ls = new Array(locals);
  ls[0] = self;
  for (let i = 0; i < args.length; i++) {
    ls[i+1] = args[i];
  }

  return {
    bytecode: bytecode,
    locals: ls,
    pc: pc || 0,
    parent: from,
    thread: from.thread,
    stack: [],
  };
}

// Block activation records share a pointer to the locals with their parent.
function blockActivationRecord(from, argIndex, args, bytecode, pc) {
  const ls = from.locals;
  for (let i = 0; i < args.length; i++) {
    ls[argIndex + i] = args[i];
  }
  return {
    bytecode: bytecode,
    pc: pc || 0,
    locals: ls,
    parent: from,
    thread: from.thread,
    stack: [],
  };
}



// Thread
// A thread is one line of execution, and it runs concurrently with other
// VM threads.
// A thread is either fresh, running, ready, blocked, or dead.
const T_FRESH = 1;
const T_RUNNING = 2;
const T_READY = 3;
const T_BLOCKED = 4;
const T_DEAD = 5;

// The VM maintains a single running thread, and then a queue of ready and
// blocked threads. Blocked threads are generally waiting for asynchronous
// Javascript events (such as an XHR returning, or user input). The Javascript
// callbacks for those events will unblock the thread and move it onto the ready
// queue.
// To facilitate having a linked list of threads, the threads have a "next"
// pointer.

class Thread {
  constructor() {
    this.activation = null;
    this.state = T_FRESH;
    this.next = null;
  }

  tick() {
    const ar = this.activation;
    const bc = ar.bytecode[ar.pc++];
    execute(vm, ar, bc);
  }
}

function newThread() {
  return {
  };
}


// VM
// The VM itself maintains a set of threads (one running, a list of ready
// threads). Its main job is to do the thread-related bookkeeping, and interact
// with the Javascript environment to run the active thread.
//
// The VM tries to use requestIdleCallback to avoid blocking the UI thread.
// Each threads is allowed a fixed time slice as a number of bytecodes executed
// before the next thread is swapped in to run.

const OPCODES_PER_TIME_SLICE = 100;
const IDLE_DEADLINE_MARGIN = 1.0; // 1 millisecond.

class VM {
  constructor() {
    this.currentThread = null;
    this.nextReady = null;
    this.lastReady = null;
  }

  // Moves the currently running thread to the ready queue, and readies the next
  // ready thread (if any).
  yield() {
    const t = this.currentThread;
    this.currentThread = null;
    this.pushReady(t);
    this.popReady();
  }

  pushReady(thread) {
    if (this.lastReady === null) {
      thread.next = null;
      this.nextReady = this.lastReady = thread;
    } else {
      this.lastReady.next = thread;
      thread.next = null;
      this.lastReady = thread;
    }
  }

  popReady() {
    const t = this.nextReady;
    if (!t) {
      console.log('No threads to run; VM going idle');
      return;
    }

    this.nextReady = t.next;
    if (!this.nextReady) {
      this.lastReady = null;
    }
  }

  // Call this to begin the cycling of the VM.
  run() {
    if (!this.lastIdleCallback) {
      this.lastIdleCallback = window.requestIdleCallback((deadline) => {
        this.tick(deadline);
      }, {timeout: 300});
    }
  }

  // Cancels any future idle callbacks, and stops me running for now.
  stop() {
    if (this.lastIdleCallback) {
      window.cancelIdleCallback(this.lastIdleCallback);
      this.lastIdleCallback = null;
    }
  }

  // Called when we are inside an idle callback, with the deadline.
  // The VM will try to run as many cycles as possible within that limit.
  tick(deadline) {
    const start = performance.now();
    let total = 0;
    while (deadline.timeRemaining() < IDLE_DEADLINE_MARGIN) {
      for (let i = 0; this.currentThread && i < OPCODES_PER_TIME_SLICE; i++) {
        this.currentThread.tick();
        total++;
      }
      this.yield();
      if (!this.currentThread) {
        this.stop();
        break;
      }
    }
    const elapsed = performance.now() - start;
    console.log('ran', total, 'opcodes in', elapsed, 'ms', (total/elapsed*1000), 'per second');
  }
}


window.vm = new VM();

function bootstrap(json) {
  debugger;
}

const BYTECODE_HANDLERS = {};

BYTECODE_HANDLERS.pushLocal = function(ar, bc) {
  ar.stack.push(ar.locals[bc.index]);
};

BYTECODE_HANDLERS.pushGlobal = function(ar, bc) {
  const g = classes[bc.name];
  if (!g) {
    throw new Error('Unknown global ' + bc.name);
  }
  ar.stack.push(g);
};

BYTECODE_HANDLERS.pushSelf = function(ar, bc) {
  ar.stack.push(ar.locals[0]);
};

BYTECODE_HANDLERS.pushInstVar = function(ar, bc) {
  ar.stack.push(ar.locals[0].$vars[bc.index]);
};

BYTECODE_HANDLERS.pushLiteral = function(ar, bc) {
  ar.stack.push(bc.value);
};

BYTECODE_HANDLERS.storeLocal = function(ar, bc) {
  ar.locals[bc.index] = ar.stack.pop();
};

BYTECODE_HANDLERS.storeInstVar = function(ar, bc) {
  ar.locals[0].$vars[bc.index] = ar.stack.pop();
};

BYTECODE_HANDLERS.startBlock = function(ar, bc) {
  // Bytecode gives argc, argStart and length (in bytecodes).
  // The current PC is the start, and we can give it a slice of args.
  // We construct a BlockClosure, push it, and move the outer PC.
  const closure = mkInstance(classes['BlockClosure']);
  closure.$vars[CLOSURE_BYTECODE] = ar.bytecode.slice(ar.pc, ar.pc + bc.length);
  closure.$vars[CLOSURE_ARGC] = bc.argc;
  closure.$vars[CLOSURE_ARGV] = ar.locals.slice(bc.argStart);

  ar.stack.push(closure);
  ar.pc += bc.length;
};

BYTECODE_HANDLERS.startMethod = function(ar, bc) {
  // Bytecode gives: selector, argc, temps count, length in bytecodes.
  // We built the CompiledMethod instance with those values, push it, and skip
  // over the code.
  const method = mkInstance(classes['CompiledMethod']);
  method.$vars[METHOD_BYTECODE] = ar.bytecode.slice(ar.pc, ar.pc + bc.length);
  method.$vars[METHOD_LOCALS] = 1 + bc.argc + bc.temps;
  method.$vars[METHOD_ARGC] = bc.argc;
  method.$vars[METHOD_SELECTOR] = bc.selector;
  ar.stack.push(method);
  ar.pc += bc.length;
};

BYTECODE_HANDLERS.send = function(ar, bc) {
  // First, look up the target method. We need to check its arg count and such.
};

// The fundamental function that executes a single bytecode!
function execute(vm, ar, bc) {

}




