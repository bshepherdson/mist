import {
  ptr, stw,
  CTX_METHOD, CTX_PC, CTX_SENDER,
  CLS_PROCESS,
  METHOD_BYTECODE,
  PROCESS_CONTEXT, PROCESS_NEXT, PROCESS_PREV,
  PROCESS_TABLE_READY, PROCESS_TABLE_NEXT_PRIORITY, PROCESS_PROCESS_TABLE,
  MA_NIL,
  methodFor, classTable, mkInstance,
  fromSmallInteger, toSmallInteger,
  read, readWordArray, readIV, writeIV,
} from './memory';
import {execute} from './bytecodes';
import {vm} from './vm';

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
function nextThread(): ptr|null {
  let pt = vm.processTable;
  while (pt != MA_NIL) {
    const ready = readIV(pt, PROCESS_TABLE_READY);
    if (ready != MA_NIL) return ready;
    pt = readIV(pt, PROCESS_TABLE_NEXT_PRIORITY);
  }
  // If we reach here, then there's nothing ready to run - so idle.
  return null;
}

// Helper that reads a bytecode and increments PC.
export function readPC(ctx: ptr): stw {
  const pc = fromSmallInteger(readIV(ctx, CTX_PC));
  const method = methodFor(ctx);
  const bcArray = readIV(method, METHOD_BYTECODE);
  const bc = readWordArray(bcArray, pc);

  // Increment the PC.
  writeIV(ctx, CTX_PC, toSmallInteger(pc + 1));
  return bc;
}

// Given a pointer to the current process, execute a single step of its
// bytecode. This is the fundamental VM operation.
export function tick(process: ptr) {
  const ctx = readIV(process, PROCESS_CONTEXT);
  const bc = readPC(ctx);
  // Executes a single bytecode!
  execute(process, ctx, bc);
}

// Given the ST pointer to a new context, makes it the top of this process.
// ASSUMES the new context's sender is already set properly!
export function pushContext(process: ptr, newContext: ptr) {
  writeIV(process, PROCESS_CONTEXT, newContext);
}

// Pops a context off this process, making the sender of the old one the new
// one for the process. If the sender is nil, this thread is expired and we can
// remove this process from the process table.
export function popContext(process: ptr, opt_ctx?: ptr): boolean {
  const oldCtx = readIV(process, PROCESS_CONTEXT);
  const sender = opt_ctx || readIV(oldCtx, CTX_SENDER);
  if (sender != MA_NIL) {
    writeIV(process, PROCESS_CONTEXT, sender);
    return true;
  }

  // If we're still here, this process needs removing from the table.
  // This is why processes form a doubly-linked list!
  writeIV(process, PROCESS_CONTEXT, MA_NIL);
  const prev = readIV(process, PROCESS_PREV);
  const next = readIV(process, PROCESS_NEXT);
  if (prev != MA_NIL) {
    writeIV(prev, PROCESS_NEXT, next);
  }
  if (next != MA_NIL) {
    writeIV(next, PROCESS_PREV, prev);
  }

  // If either of those is nil, both should be, since the list is circular.
  // Either way, it's correct to make next the head of the process table's ready
  // list.
  const pt = readIV(process, PROCESS_PROCESS_TABLE);
  // Only replace the READY process if it was me!
  // This is because the driver hand-rolls threads.
  if (readIV(pt, PROCESS_TABLE_READY) === process) {
    // This process is dying, so if it was the last one in the circular list,
    // just store nil.
    writeIV(pt, PROCESS_TABLE_READY, process === next ? MA_NIL : next);
  }
  return false;
}

// Adds this context as the root of a new user-priority thread.
// Returns the new Process.
export function fork(ctx: ptr): ptr {
  let pt = readIV(vm.processTable, PROCESS_TABLE_NEXT_PRIORITY);
  pt = readIV(pt, PROCESS_TABLE_NEXT_PRIORITY);
  const proc = mkInstance(read(classTable(CLS_PROCESS)));
  writeIV(proc, PROCESS_CONTEXT, ctx);
  writeIV(proc, PROCESS_PROCESS_TABLE, pt);
  const head = readIV(pt, PROCESS_TABLE_READY);
  if (head == MA_NIL) {
    writeIV(proc, PROCESS_NEXT, proc);
    writeIV(proc, PROCESS_PREV, proc);
  } else {
    const prev = readIV(head, PROCESS_PREV);
    writeIV(proc, PROCESS_NEXT, head);
    writeIV(head, PROCESS_PREV, proc);
    writeIV(prev, PROCESS_NEXT, proc);
  }
  writeIV(pt, PROCESS_TABLE_READY, proc);
  return proc;
}

// TODO popTo, probably?

