// Virtual machine types and code.


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
    const bc = ar.bytecode[ar.pc];
    if (!bc) {
      throw new InternalError('Compiler failed to insert an answer bytecode.');
    }
    ar.pc++;
    execute(vm, ar, bc);
  }

  push(ar) {
    this.activation = ar;
  }

  // Pops a single activation record.
  pop() {
    this.activation = this.activation.parent;
  }

  // Pops records until the given record is on top.
  popTo(target) {
    this.activation = target;
  }
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
    thread.state = T_READY;
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
    this.currentThread = t;
    t.state = T_RUNNING;
  }

  runningThread() {
    if (this.currentThread && this.currentThread.state === T_RUNNING) {
      return this.currentThread;
    }
    if (this.currentThread) {
      this.yield();
      return this.runningThread();
    }
    this.popReady();
    return this.currentThread;
  }

  // Call this to begin the cycling of the VM.
  run() {
    if (!this.lastIdleCallback) {
      this.lastIdleCallback = global.requestIdleCallback((deadline) => {
        this.tick(deadline);
      }, {timeout: 300});
    }
  }

  // Cancels any future idle callbacks, and stops me running for now.
  stop() {
    if (this.lastIdleCallback) {
      global.cancelIdleCallback(this.lastIdleCallback);
      this.lastIdleCallback = null;
    }
  }

  // Called when we are inside an idle callback, with the deadline.
  // The VM will try to run as many cycles as possible within that limit.
  tick(deadline) {
    const start = performance.now();
    let total = 0;
    while (deadline.timeRemaining() < IDLE_DEADLINE_MARGIN) {
      this.runningThread();
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


global.vm = new VM();

const BYTECODE_HANDLERS = {};

// The fundamental function that executes a single bytecode!
function execute(vm, ar, bc) {
  const h = BYTECODE_HANDLERS[bc.bytecode];
  if (!h) {
    throw new UnknownBytecodeError(bc.bytecode);
  }

  h(ar, bc);
}

