import {
  ptr, stw,
  CTX_METHOD, CTX_PC, CTX_SENDER,
  CLS_PROCESS,
  LINKED_LIST_HEAD, LINKED_LIST_TAIL,
  METHOD_BYTECODE,
  PROCESS_LINK, PROCESS_SUSPENDED_CONTEXT, PROCESS_MY_LIST,
  PROCESSOR_SCHEDULER_ACTIVE_PROCESS, PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES,
  MA_NIL, MA_GLOBALS,
  methodFor, classTable, mkInstance,
  fromSmallInteger, toSmallInteger,
  read, readArray, readWordArray, readIV, writeIV, writeIVNew,
  gcTemps, gcRelease, seq,
} from './memory';
import {execute} from './bytecodes';
import {SYM_PROCESSOR} from './corelib';
import {lookup} from './dict';
import {vm} from './vm';

// Processes and priorities
//
// There are 7 priority levels built in; the higher the, well, higher.
// These numbers are indexes into the quiescentProcesses array on
// ProcessorScheduler. The elements of this array are linked lists of processes
// waiting to run.
//
// Note that there's no fairness between levels! Each level can starve those
// below it; only the highest priority is guaranteed to run at all.
//
// The process scheduler reschedules (in principle) on every N message sends, so
// there's unlikely to be starvation among processes at the same level.
//
// The VM goes to `Processor` in the globals map, and runs the first process in
// the highest priority that isn't empty.
//
// The currently running process goes in the Processor's activeProcess field.
// Its context goes in the vm.ctx slot (and suspendedContext is nil'd).
//
// If there are no quiescentProcesses ready to run at all, the VM stops running
// until the next frame callback or user event.

// Examines the process table to find and return the next Process that should be
// run. Returns JS null if none is ready to run.
function nextThread(): ptr|null {
  const [v_scheduler, v_quiets, v_list, v_proc, v_next] = seq(5);
  const ptrs = gcTemps(5);

  ptrs[v_scheduler] = lookup(read(MA_GLOBALS), SYM_PROCESSOR);
  ptrs[v_quiets] =
      readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES);
  let i = 6;
  for (; i >= 0; i--) {
    ptrs[v_list] = readArray(ptrs[v_quiets], i);
    ptrs[v_proc] = readIV(ptrs[v_list], LINKED_LIST_HEAD);
    if (ptrs[v_proc] !== MA_NIL) break;
  }
  if (i < 0) {
    gcRelease(ptrs);
    return null;
  }

  ptrs[v_next] = readIV(ptrs[v_proc], PROCESS_LINK);
  writeIV(ptrs[v_list], LINKED_LIST_HEAD, ptrs[v_next]);
  if (ptrs[v_next] === MA_NIL) {
    writeIVNew(ptrs[v_list], LINKED_LIST_TAIL, MA_NIL);
  }

  writeIVNew(ptrs[v_proc], PROCESS_LINK, MA_NIL);
  writeIVNew(ptrs[v_proc], PROCESS_MY_LIST, MA_NIL);
  const ctx = readIV(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT);
  writeIVNew(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT, MA_NIL);
  return ctx;
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

// Executes a single step of the current process.
// This is the fundamental VM operation.
export function tick() {
  // Executes a single bytecode!
  execute(readPC(vm.ctx));
}

function activeProcess(): ptr {
  const scheduler = lookup(read(MA_GLOBALS), SYM_PROCESSOR);
  return readIV(scheduler, PROCESSOR_SCHEDULER_ACTIVE_PROCESS);
}

// Given the ST pointer to a new context, makes it the top of this process.
// ASSUMES the new context's sender is already set properly!
export function pushContext(newContext: ptr) {
  vm.ctx = newContext;
}

// Pops a context off this process, making the sender of the old one the new
// one for the process. If the sender is nil, this thread is expired and we
// should set the activeProcess to nil.
export function popContext(opt_ctx?: ptr) {
  vm.ctx = opt_ctx || readIV(vm.ctx, CTX_SENDER);

  // If that was the last link, this process is over.
  if (vm.ctx === MA_NIL) {
    writeIVNew(lookup(read(MA_GLOBALS), SYM_PROCESSOR),
        PROCESSOR_SCHEDULER_ACTIVE_PROCESS, MA_NIL);
  }
}

// Only called from the driver.
// Throws if there's an active process.
export function executeImmediate(ctx: ptr) {
  const [v_scheduler, v_proc] = seq(2);
  const ptrs = gcTemps(2);
  ptrs[v_scheduler] = lookup(read(MA_GLOBALS), SYM_PROCESSOR);
  ptrs[v_proc] = mkInstance(read(classTable(CLS_PROCESS)));
  if (readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS) !== MA_NIL) {
    throw new Error('Cannot execute top-level code with another thread active');
  }
  writeIVNew(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS, ptrs[v_proc]);
  gcRelease(ptrs);
}

