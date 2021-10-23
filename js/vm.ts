// Virtual machine types and code.

// The virtual machine is comprised of the memory space and a handful of
// registers:
// - allocationPointer
// - nextIdentityHash
// - nextClass
//
// Of these, the priority order for storing them in real machine registers is:
// - allocationPointer
// - nextIdentityHash
// - EDEN_LIMIT (see memory)
// The others are accessed infrequently enough to be irrelevant.
// EDEN_LIMIT is a constant, but it's compared with `allocationPointer` very
// often so loading it as a literal in an instruction gets swiftly costly.
// On the other hand, EDEN_LIMIT never changes, while nextIdentityHash does, so
// the latter is better to keep in a register if only two are available.
//
// Note the conspicious lack of a PC! The PC is accessed as
// runningThread->context->pc.

export const vm = {
  ctx: 0,
  allocationPointer: 0,
  nextIdentityHash: 0,
  nextClass: 0,

  // Set by valueNoContextSwitch to make sure the block gets properly started
  // before any context switch. Cleared after the next vmLoop iteration.
  blockContextSwitch: false,
  // A process of higher priority than the active one has become ready to run.
  preempting: false,

  // The start time of the current frame. Used to manage the frame budget.
  frameStart: performance.now(),
  bootstrapComplete: false,
};

