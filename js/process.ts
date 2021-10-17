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
  read, readWordArray, readIV, writeIV, writeIVNew,
  gcTemps, gcRelease, seq,
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
export function tick() {
  vm.ctx = readIV(vm.runningProcess, PROCESS_CONTEXT);

  // DEBUG: This isn't needed.
  //const pc = fromSmallInteger(readIV(vm.ctx, CTX_PC));
  const bc = readPC(vm.ctx);
  //console.log('PC ' + pc + ':  ' + bc.toString(16));
  // Executes a single bytecode!
  execute(bc);
}

// Given the ST pointer to a new context, makes it the top of this process.
// ASSUMES the new context's sender is already set properly!
export function pushContext(newContext: ptr) {
  vm.ctx = newContext;
  writeIV(vm.runningProcess, PROCESS_CONTEXT, vm.ctx);
}

// Pops a context off this process, making the sender of the old one the new
// one for the process. If the sender is nil, this thread is expired and we can
// remove this process from the process table.
export function popContext(opt_ctx?: ptr): boolean {
  const oldCtx = readIV(vm.runningProcess, PROCESS_CONTEXT);
  const sender = opt_ctx || readIV(oldCtx, CTX_SENDER);
  if (sender != MA_NIL) {
    vm.ctx = sender;
    writeIV(vm.runningProcess, PROCESS_CONTEXT, sender);
    return true;
  }

  // If we're still here, this process needs removing from the table.
  // This is why processes form a doubly-linked list!
  writeIVNew(vm.runningProcess, PROCESS_CONTEXT, MA_NIL);
  const [v_pt, v_prev, v_next] = seq(3);
  const ptrs = gcTemps(3);
  ptrs[v_prev] = readIV(vm.runningProcess, PROCESS_PREV);
  ptrs[v_next] = readIV(vm.runningProcess, PROCESS_NEXT);
  if (ptrs[v_prev] != MA_NIL) {
    writeIV(ptrs[v_prev], PROCESS_NEXT, ptrs[v_next]);
  }
  if (ptrs[v_next] != MA_NIL) {
    writeIV(ptrs[v_next], PROCESS_PREV, ptrs[v_prev]);
  }

  // If either of those is nil, both should be, since the list is circular.
  // Either way, it's correct to make next the head of the process table's ready
  // list.
  ptrs[v_pt] = readIV(vm.runningProcess, PROCESS_PROCESS_TABLE);
  // Only replace the READY process if it was me!
  // This is because the driver hand-rolls threads.
  if (readIV(ptrs[v_pt], PROCESS_TABLE_READY) === vm.runningProcess) {
    // This process is dying, so if it was the last one in the circular list,
    // just store nil.
    vm.runningProcess = vm.runningProcess === ptrs[v_next] ?
        MA_NIL : ptrs[v_next];
    writeIV(ptrs[v_pt], PROCESS_TABLE_READY, vm.runningProcess);
  }
  gcRelease(ptrs);
  return false;
}

// Adds this context as the root of a new user-priority thread.
// Returns the new Process.
export function fork(ctx: ptr): ptr {
  const [v_pt, v_proc, v_ctx, v_head, v_prev] = seq(5);
  const ptrs = gcTemps(5);
  ptrs[v_ctx] = ctx;
  ptrs[v_pt] = readIV(vm.processTable, PROCESS_TABLE_NEXT_PRIORITY);
  ptrs[v_pt] = readIV(ptrs[v_pt], PROCESS_TABLE_NEXT_PRIORITY);
  ptrs[v_proc] = mkInstance(read(classTable(CLS_PROCESS)));
  writeIVNew(ptrs[v_proc], PROCESS_CONTEXT, ptrs[v_ctx]);
  writeIVNew(ptrs[v_proc], PROCESS_PROCESS_TABLE, ptrs[v_pt]);
  ptrs[v_head] = readIV(ptrs[v_pt], PROCESS_TABLE_READY);
  if (ptrs[v_head] == MA_NIL) {
    writeIV(ptrs[v_proc], PROCESS_NEXT, ptrs[v_proc]);
    writeIV(ptrs[v_proc], PROCESS_PREV, ptrs[v_proc]);
  } else {
    ptrs[v_prev] = readIV(ptrs[v_head], PROCESS_PREV);
    writeIV(ptrs[v_proc], PROCESS_NEXT, ptrs[v_head]);
    writeIV(ptrs[v_head], PROCESS_PREV, ptrs[v_proc]);
    writeIV(ptrs[v_prev], PROCESS_NEXT, ptrs[v_proc]);
  }
  writeIV(ptrs[v_pt], PROCESS_TABLE_READY, ptrs[v_proc]);
  const proc = ptrs[v_proc];
  gcRelease(ptrs);
  return proc;
}

// TODO popTo, probably?

