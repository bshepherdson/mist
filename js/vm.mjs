// Virtual machine types and code.

// The virtual machine is comprised of the memory space and a handful of
// registers:
// - processTable
// - runningProcess
// - allocationPointer
//
// Note the conspicious lack of a PC! The PC is accessed as
// runningThread->context->pc.

export const vm = {
  processTable: 0,
  runningProcess: 0,
  allocationPointer: 0,
};

// Processes and priorities
//
// 4 process priority levels are defined:
// - Interrupt
// - UI
// - User
// - Background
//
// The names are fairly self-explanatory. UI threads are used to update the
// interactive system; most things run at User level. Background is for system
// or user jobs that are intended to run in the background using up any idle
// cycles.
//
// Note that there's no fairness between levels! Each level can starve those
// below it; only Interrupt priority is guaranteed to run at all.
//
// The process scheduler reschedules (in principle) on every message send, so
// there's unlikely to be starvation among processes at the same level.
//
// Processes are organized into two doubly-linked lists at each priority level:
// a list of ready-to-run Processes and a list of blocked Processes. These are
// circular lists, used to round-robin the processes. Freshly unblocked
// processes make themselves next to run after the current one.
//
// ProcessPriority captures these two lists as its 'ready' and 'blocked'
// variables. It also has a `nextPriority` link to another ProcessPriority.
//
// The VM goes to `processTable` (the ProcessPriority for Interrupt), and checks
// if it has any `ready` processes. If so, it runs the frontmost one for a
// while, updating the process links.
//
// If there are no `ready` processes at that priority level, it recurses to
// `nextPriority` until that too is nil. If there's nothing ready to run, the VM
// idles until the next machine interrupt fires to wake it up. If the machine
// has a halt instruction, this is the time.

// These JS wrappers of Smalltalk values are always ephemeral - they have no
// state of their own and keep everything in the ST objects.

// Examines the process table to find and return the next Process that should be
// run. Returns JS null if none is ready to run.
function nextThread() {
  let pt = vm.processTable;
  while (pt != MA_NIL) {
    const ready = readAt(pt, PROCESS_TABLE_READY);
    if (ready != MA_NIL) return ready;
    pt = readAt(pt, PROCESS_TABLE_NEXT_PRIORITY);
  }
  // If we reach here, then there's nothing ready to run - so idle.
  return null;
}

// Helper that reads a bytecode and increments PC.
function readPC(ctx) {
  const pc = readSmallInteger(ctx + CTX_PC);
  const method = readAt(ctx, CTX_METHOD);
  const bcArray = readAt(method, METHOD_BYTECODE);
  const bc = readRawArray(bcArray, pc);

  // Increment the PC.
  adjustSmallInteger(ctx + CTX_PC, 1);
  return bc;
}

// Given a pointer to the current process, execute a single step of its
// bytecode. This is the fundamental VM operation.
function tick(process) {
  const ctx = readAt(process, PROCESS_CONTEXT);
  const bc = readPC(ctx);
  // Executes a single bytecode!
  execute(process, ctx, bc);
}

// Given the ST pointer to a new context, makes it the top of this process.
// ASSUMES the new context's sender is already set properly!
function pushContext(process, newContext) {
  writeAt(process, PROCESS_CONTEXT, newContext);
}

// Pops a context off this process, making the sender of the old one the new
// one for the process. If the sender is nil, this thread is expired and we can
// remove this process from the process table.
function popContext(process, opt_ctx) {
  const oldCtx = readAt(process, PROCESS_CONTEXT);
  const sender = opt_ctx || readAt(oldCtx, CTX_SENDER);
  if (sender != MA_NIL) return;

  // If we're still here, this process needs removing from the table.
  // This is why processes form a doubly-linked list!
  const prev = readAt(process, PROCESS_PREV);
  const next = readAt(process, PROCESS_NEXT);
  if (prev != MA_NIL) {
    writeAt(prev, PROCESS_NEXT, next);
  }
  if (next != MA_NIL) {
    writeAt(next, PROCESS_PREV, prev);
  }
  // If either of those is nil, both should be, since the list is circular.
  // Either way, it's correct to make next the head of the process table's ready
  // list.
  const pt = readAt(process, PROCESS_PROCESS_TABLE);
  writeAt(pt, PROCESS_TABLE_READY, next);
}

// TODO popTo, probably?

