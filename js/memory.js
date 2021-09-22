// Memory access and management for our Smalltalk VM.


const MEM_SIZE = 1024 * 1024 * 64; // 64MW = 128MB for now.
const MEM_SPECIAL_TOP = 0x10000; // 64KW of magic should be plenty.

const MW4 = 4 * 1024 * 1024; // 4 MW is our unit size for several things.

const MEM_TENURED_TOP = MW4; // 4MW of tenure + special.
const MEM_G1_TOP = MEM_TENURED_TOP + MW4; // 4MW for each generation
const MEM_G2_TOP = MEM_G1_TOP + MW4;      // 4MW for each generation
const MEM_EDEN_BASE = MEM_G2_TOP;         // Eden gets the rest = 42MW
const MEM_EDEN_TOP = MEM_SIZE;

const mem = new Uint16Array(MEM_SIZE);

// Bunch of values in the special space, pointers into the different memory
// areas.
// MA_ is "mem addr".

// Pointers to the ends of the ranges for new and old generations. (Indirected
// because they get swapped.)
const MA_OLD_GEN_START = 0x1000;
const MA_OLD_GEN_ALLOC = 0x1002;
const MA_NEW_GEN_START = 0x1004;
const MA_NEW_GEN_ALLOC = 0x1006;
const MA_TENURE_ALLOC = 0x1008;

// Points to the first escaped pointer linked list entry; see GC design.
const MA_ESCAPED_HEAD = 0x100a;

// Some canned ST objects that exist at known places in memory.
const MA_OBJECT_INSTANCE = 0x2000; // Size: 8, no instance variables.
const MA_NIL = 0x2008; // Size 8, no instance variables.
const MA_TRUE = 0x2010; // Size 8, no instance variables.
const MA_FALSE = 0x2018; // Size 8, no instance variables.


const CLASSTABLE_BASE = 0x3000;
const CLASS_SIZE = 8 + 2*2; // 2 members + standard kit.
let nextClass_ = CLASSTABLE_BASE;
function nextClass() {
  const p = nextClass;
  nextClass += CLASS_SIZE;
  return p;
}

const CLS_PROTO_OBJECT = nextClass();
const CLS_OBJECT = nextClass();
const CLS_CLASS = nextClass();
const CLS_BEHAVIOR = nextClass();
const CLS_CLASS_DESCRIPTION = nextClass();

const CLS_CONTEXT = nextClass();
const CLS_COMPILED_METHOD = nextClass();
const CLS_BLOCK_CLOSURE = nextClass();


// Offsets into some standard objects.
const OBJ_HEADER = 0;
const OBJ_GC = 2;
const OBJ_SUPER = 4;
const OBJ_CLASS = 6;
const OBJ_MEMBERS_BASE = 8;

const CLASS_NAME = OBJ_MEMBERS_BASE;
const CLASS_METHODS = OBJ_MEMBERS_BASE + 2;
const CLASS_SUPERCLASS = OBJ_MEMBERS_BASE + 4;
const CLASS_INSTVAR_COUNT = OBJ_MEMBERS_BASE + 6;

const GC_FIELD_0 = 0; // Initial value for the GC word.

const PROCESS_TABLE_READY = OBJ_MEMBERS_BASE;
const PROCESS_TABLE_BLOCKED = OBJ_MEMBERS_BASE + 2;
const PROCESS_TABLE_NEXT_PRIORITY = OBJ_MEMBERS_BASE + 4;

const PROCESS_CONTEXT = OBJ_MEMBERS_BASE;
const PROCESS_NEXT = OBJ_MEMBERS_BASE + 2;
const PROCESS_PREV = OBJ_MEMBERS_BASE + 4;
const PROCESS_PROCESS_TABLE = OBJ_MEMBERS_BASE + 6;

// Contexts are stored as 24-pointer objects.
// The first 5 values are reserved: method, locals, pc, sender, stack index.
// The remaining 19 values are the stack. The stack pointer is empty-ascending
// and in values, not words. Eg. it starts out at 0, then becomes 1, 2, etc.
const CTX_METHOD = OBJ_MEMBERS_BASE;
const CTX_LOCALS = CTX_METHOD + 2;
const CTX_PC = CTX_LOCALS + 2;
const CTX_SENDER = CTX_PC + 2;
const CTX_STACK_INDEX = CTX_SENDER + 2;
const CTX_STACK_BASE = CTX_STACK_INDEX + 2;
const STACK_MAX_SIZE = 19;

// CompiledMethods have six instance variables: bytecode, literals, name,
// class, argc, and locals.
// bytecode is a raw array, literals a pointer array.
const METHOD_BYTECODE = OBJ_MEMBERS_BASE;
const METHOD_LITERALS = METHOD_BYTECODE + 2;
const METHOD_NAME = METHOD_LITERALS + 2;
const METHOD_CLASS = METHOD_NAME + 2;
const METHOD_ARGC = METHOD_CLASS + 2;
const METHOD_LOCALS = METHOD_ARGC + 2;


const BLOCK_CONTEXT = OBJ_MEMBERS_BASE;
const BLOCK_PC_START = BLOCK_METHOD + 2;
const BLOCK_ARGC = BLOCK_PC_START + 2;
const BLOCK_ARGV = BLOCK_ARGC + 2;



const TYPE_MASK = 0xf0000000;
const TYPE_OBJ = 0;
const TYPE_RA = 1 << 28;
const TYPE_PA = 2 << 28;


const RA_TYPE = 1;
const RA_HEADER = 0;
const RA_GC = 2;
const RA_BASE = 4;

const PA_HEADER = 0;
const PA_GC = 2;
const PA_BASE = 4;

const SMALL_INTEGER_RAW = RA_BASE;



// Functions for accessing the memory.
// Longs are stored big-endian, ie. the high word is at the lower address.
function readWord(addr) {
  return mem[addr];
}

function read(addr) {
  return (mem[addr] << 16) | mem[addr+1];
}

function writeWord(addr, value) {
  return mem[addr] = value & 0xffff;
}

function write(addr, value) {
  writeWord(addr, value >> 16);
  writeWord(addr + 1, value);
}


// Given the address of the start of any object, get its size in words.
// This is simply the size field for non-special objects.
// Easy mode since there are no special objects yet!
function objSize(addr) {
  const header = read(addr);
  return header & 0x0fffffff;
}


// Some helpers for offsetting based on a pointer, since that's pretty common.
function readAt(base, offset) {
  // Defensive programming: checks the size long and disallows reads past the
  // end of non-special values.
  if (offset + 1 > objSize(base)) {
    throw new Error('reading off the end of object');
  }

  return read(base + offset);
}

// DOES NOT handle GC updates; that needs to be handled in eg. store bytecodes.
function writeAt(base, offset, value) {
  if (offset + 1 > objSize(base)) {
    throw new Error('writing off the end of object');
  }

  return write(base + offset, value);
}

// Reads into a raw array at a given index. Returns a single word.
function readRawArray(base, index) {
  return readWord(base + RA_BASE + index);
}

// Helper to extract a ST number to a JS number. Abstracted since this is
// likely to evolve in the VM.
// For now, numbers are stored as pointers to heap-allocated raw arrays of
// inner length 2, with the special type -1.
// NB: These are passed the pointer of the number in some other object's
// instance variables! Since for now numbers are stored on the heap, these need
// to do a nested read. They also mutate numbers on the heap(!)
function readSmallInteger(addr) {
  return readAt(read(addr), SMALL_INTEGER_RAW);
}

function writeSmallInteger(addr, value) {
  return writeAt(read(addr), SMALL_INTEGER_RAW, value);
}


// Working with the stack on a MethodContext.
function push(ctx, value) {
  const sp = readSmallInteger(ctx + CTX_STACK_INDEX);
  if (sp >= STACK_MAX_SIZE) throw new Error('VM stack overflow');
  writeAt(ctx, CTX_STACK_BASE + 2*sp, value);
  checkOldToNew(ctx + CTX_STACK_BASE + 2*sp, value);
  // Increment the stack pointer.
  writeSmallInteger(ctx + CTX_STACK_INDEX, sp + 1);
}

function pop(ctx) {
  const sp = readSmallInteger(ctx + CTX_STACK_INDEX);
  if (sp <= 0) throw new Error('VM stack underflow');
  const value = readAt(ctx, CTX_STACK_BASE + 2*sp);
  writeSmallInteger(ctx + CTX_STACK_INDEX, sp - 1);
  return value;
}

function peek(ctx) {
  const sp = readSmallInteger(ctx + CTX_STACK_INDEX);
  const value = readAt(ctx, CTX_STACK_BASE + 2*sp);
  return value;
}



// Allocation! This is vital but not hard: given a necessary size, allocate
// space for it in the Eden and return a pointer to it.
function alloc(size) {
  const p = allocationPointer - size;
  if (p < MEM_EDEN_BASE) {
    throw new Error('Minor GC needed but not implemented!');
  }
  allocationPointer = p;
  return p;
}

// Allocates size words in tenured space, returning the pointer to it.
// This is used for tenuring, but also for things like Symbols that are
// permanently cached.
function tenure(size) {
  const p = read(MA_TENURE_ALLOC); // Raw number, not a Smalltalk value.
  write(MA_TENURE_ALLOC, p + size);
  return p;
}

function initObject(p, cls, superObj, instVars) {
  writeAt(p, OBJ_HEADER, size | TYPE_OBJ);
  writeAt(p, OBJ_GC, GC_FIELD_0);
  writeAt(p, OBJ_SUPER, superObj);
  writeAt(p, OBJ_CLASS, cls);

  for (let i = 0; i < instVars; i++) {
    writeAt(p, OBJ_MEMBERS_BASE + 2 * i, MA_NIL);
  }
}

function allocObject(cls, superObj, instVars) {
  const size = OBJ_MEMBERS_BASE + 2 * instVars;
  const p = alloc(size);
  initObject(p, cls, superObj, instVars);
  return p;
}

// Given the pointer to a Class, constructs a new instance of it.
// This is essentially basicNew.
// Only works for regular objects! Special types should be calling other
// primitives.
function mkInstance(cls) {
  // Special case: if cls is Object, return the canned object instance.
  if (cls === CLS_OBJECT) return MA_OBJECT_INSTANCE;

  // Recursively build the superObj instances.
  const superclass = readAt(cls, CLASS_SUPERCLASS);
  const sup = mkInstance(superclass);
  const instVars = readSmallInteger(cls + CLASS_INSTVAR_COUNT);
  return allocObject(cls, sup, instVars);
}

// Size in *pointers*!
function allocPointerArray(size, opt_uninitialized) {
  const wordSize = PA_BASE + 2 * size;
  const p = alloc(wordSize);
  writeAt(p, PA_HEADER, wordSize | TYPE_PA);
  writeAt(p, PA_GC, GC_FIELD_0);

  if (!opt_uninitialized) {
    for (let i = 0; i < size; i++) {
      writeAt(p, PA_BASE + 2 * i, MA_NIL);
    }
  }

  return p;
}

function allocRawArray(size) {
  const wordSize = RA_BASE + wordSize;
  const p = alloc(wordSize);
  writeAt(p, RA_HEADER, wordSize | TYPE_RA);
  writeAt(p, RA_GC, GC_FIELD_0);
  return p;
}


// When we're storing a pointer at p that points to target, we check to see if
// p is in the old space (< MEM_EDEN_BASE) and target in new (>= MEM_EDEN_BASE).
function checkOldToNew(p, target) {
  const isOld = p < MEM_EDEN_BASE;
  const isNew = target >= MEM_EDEN_BASE;
  if (isOld && isNew) {
    // We store these records as 2-pointer arrays.
    const link = allocPointerArray(2, true); // No need to initialize to nil.
    const head = read(MA_ESCAPED_HEAD);
    writeAt(link, PA_BASE, p);
    writeAt(link, PA_BASE + 2, head);
    write(MA_ESCAPED_HEAD, link);
  }
}


// Wrap numbers and other special values. This requires allocation!
function wrapSmallInteger(n) {
  // SmallIntegers are represented as raw arrays of size RA_BASE + 2.
  const p = allocRawArray(2);
  writeAt(p, SMALL_INTEGER_RAW, n);
  return p;
}

