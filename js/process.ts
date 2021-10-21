import {
  ptr, stw,
  CTX_METHOD, CTX_PC, CTX_SENDER,
  CLS_PROCESS,
  LINKED_LIST_HEAD, LINKED_LIST_TAIL,
  METHOD_BYTECODE,
  PROCESS_LINK, PROCESS_SUSPENDED_CONTEXT, PROCESS_MY_LIST, PROCESS_PRIORITY,
  PROCESSOR_SCHEDULER_ACTIVE_PROCESS, PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES,
  SEMAPHORE_EXCESS_SIGNALS,
  MA_NIL, MA_GLOBALS,
  methodFor, classTable, mkInstance,
  fromSmallInteger, toSmallInteger,
  read, readArray, readWordArray, readIV, writeIV, writeIVNew,
  gcTemps, gcRelease, seq,
} from './memory';
import {execute} from './bytecodes';
import {SYM_PROCESSOR} from './corelib';
import {lookup} from './dict';
import {addLast, removeFirst, removeLink} from './lists';
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
/*
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
*/


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

export let loggy = false;

// Executes a single step of the given process.
// This is the fundamental VM operation.
function tick() {
  // Executes a single bytecode!
  if (loggy) console.log('executing ' + activeProcess());
  execute(readPC(vm.ctx));
}

// Core VM engine:
// We want a quiescent VM, for web-friendliness. When there is no work to do,
// we sit idle. User input (and requestAnimationFrame) will tickle particular
// Semaphores; this is analogous to hardware interrupts on a real machine.
//
// That process is the only one by which the set of runnable threads can change,
// so if it tickles a Semaphore and activeProcess is nil, it should call vmLoop.
export function vmLoop() {
  const [v_scheduler, v_proc, v_quiets, v_list] = seq(4);
  const ptrs = gcTemps(4);
  ptrs[v_scheduler] = lookup(read(MA_GLOBALS), SYM_PROCESSOR);
  ptrs[v_quiets] =
      readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES);
  while (true) {
    if (vm.contextSwitchLocks > 0) {
      tick();
      continue;
    }

    ptrs[v_proc] =
        readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS);
    const minPriority = ptrs[v_proc] === MA_NIL ?
      // If there's no active process, any priority can run next.
      0 :
      // If there is an active process, only higher-priority ones can preempt.
      // NB: Priorities are 1-based, so we need to subtract 1. But we also want
      // to stop 1 higher,
      1 + fromSmallInteger(readIV(ptrs[v_proc], PROCESS_PRIORITY)) - 1;

    for (let i = 6; i >= minPriority; i--) {
      ptrs[v_list] = readArray(ptrs[v_quiets], i);
      if (readIV(ptrs[v_list], LINKED_LIST_HEAD) !== MA_NIL) {
        // Found a process to preempt this one. If the current process is not
        // nil, move it to the end of its own priority list.
        writeIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS,
            removeFirst(ptrs[v_list]));

        // v_proc still holds the old active process, if any.
        // Put it into its own quiescent queue.
        if (ptrs[v_proc] !== MA_NIL) {
          ptrs[v_list] = readArray(ptrs[v_quiets],
              fromSmallInteger(readIV(ptrs[v_proc], PROCESS_PRIORITY)) - 1);
          writeIV(ptrs[v_proc], PROCESS_MY_LIST, ptrs[v_list]);
          writeIV(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT, vm.ctx);
          addLast(ptrs[v_list], ptrs[v_proc]);
        }

        // Update the vm.ctx to reflect this new runnable.
        ptrs[v_proc] =
            readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS);
        vm.ctx = readIV(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT);
        writeIVNew(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT, MA_NIL);
        writeIVNew(ptrs[v_proc], PROCESS_MY_LIST, MA_NIL);
        break;
      }
    }

    // When we reach here, we either:
    // - Found a process to preempt the old one; activeProcess is set.
    // - Did not find anything to preempt, but activeProcess is still set.
    // - Had no active process, and found something to run: activeProcess set.
    // - Had no active process, and found nothing to run: activeProcess is nil.
    // In only that last case, we return.
    //
    // In all cases, v_proc is the active process, and vm.ctx is up to date.
    if (ptrs[v_proc] === MA_NIL) break;
    tick();
  }
  gcRelease(ptrs);
}

// This runs the VM, but in a setTimeout(, 0) callback.
export function startVM() {
  setTimeout(vmLoop, 0);
}


export function activeProcess(): ptr {
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
  writeIVNew(ptrs[v_proc], PROCESS_PRIORITY, toSmallInteger(3));
  if (readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS) !== MA_NIL) {
    throw new Error('Cannot execute top-level code with another thread active');
  }
  writeIVNew(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS, ptrs[v_proc]);
  gcRelease(ptrs);
}

// Stop the process that the receiver represents, in such a way that it can be
// resumed later with #resume. If the receiver represents the activeProcess,
// suspend it. Otherwise remove the receiver from the list of waiting processes.
// Suspended processes are possibly in no list.
export function suspend(proc: ptr) {
  const [v_proc, v_list, v_scheduler] = seq(3);
  const ptrs = gcTemps(3);
  ptrs[v_proc] = proc;
  ptrs[v_list] = readIV(ptrs[v_proc], PROCESS_MY_LIST);

  // If this process is waiting in a list, remove myself from it.
  // Otherwise if it's the active process, set the active process to nil.
  if (ptrs[v_list] !== MA_NIL) {
    removeLink(ptrs[v_list], ptrs[v_proc]);
  } else {
    ptrs[v_scheduler] = lookup(read(MA_GLOBALS), SYM_PROCESSOR);
    const active =
        readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS);
    if (active === ptrs[v_proc]) {
      writeIV(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT, vm.ctx);
      writeIVNew(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS, MA_NIL);
      vm.ctx = MA_NIL;
    }
  }

  writeIVNew(ptrs[v_proc], PROCESS_MY_LIST, MA_NIL);
  writeIVNew(ptrs[v_proc], PROCESS_LINK, MA_NIL);

  // Need to put the list on the stack, or nil.
  gcRelease(ptrs);
}

// Given a process, makes it ready to run again.
// This process should be suspended, and in no lists currently!
export function resume(proc: ptr) {
  const [v_proc, v_list, v_scheduler] = seq(3);
  const ptrs = gcTemps(3);
  ptrs[v_proc] = proc;

  if (readIV(ptrs[v_proc], PROCESS_MY_LIST) !== MA_NIL) {
    throw new Error('cannot resume process still in a list!');
  }

  ptrs[v_scheduler] = lookup(read(MA_GLOBALS), SYM_PROCESSOR);
  if (readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS) === MA_NIL) {
    // Nothing else is active, so this can just become active.
    // This can happen on a "hardware interrupt" such as user input or display
    // frames.
    writeIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_ACTIVE_PROCESS, ptrs[v_proc]);
    vm.ctx = readIV(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT);
    writeIVNew(ptrs[v_proc], PROCESS_SUSPENDED_CONTEXT, MA_NIL);
  } else {
    ptrs[v_list] =
        readIV(ptrs[v_scheduler], PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES);
    const priority = fromSmallInteger(readIV(ptrs[v_proc], PROCESS_PRIORITY));
    ptrs[v_list] = readArray(ptrs[v_list], priority - 1);
    addLast(ptrs[v_list], ptrs[v_proc]);
    writeIV(ptrs[v_proc], PROCESS_MY_LIST, ptrs[v_list]);
  }

  gcRelease(ptrs);
}

