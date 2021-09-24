// Virtual machine types and code.

// The virtual machine is comprised of the memory space and a handful of
// registers:
// - processTable
// - runningProcess
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
  processTable: 0,
  runningProcess: 0,
  allocationPointer: 0,
  nextIdentityHash: 0,
  nextClass: 0,
};

