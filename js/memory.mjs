// Memory access and management for our Smalltalk VM.
import {vm} from './vm.mjs';

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
// Class dictionary.
const MA_CLASS_DICT = 0x100c;

// Some canned ST objects that exist at known places in memory.
export const MA_OBJECT_INSTANCE = 0x2000; // Size: 8, no instance variables.
export const MA_NIL = 0x2008; // Size 8, no instance variables.
export const MA_TRUE = 0x2010; // Size 8, no instance variables.
export const MA_FALSE = 0x2018; // Size 8, no instance variables.
export const MA_PROTO_OBJECT_INSTANCE = 0x2020 // Size 8, no instance variables.
export const MA_BOOLEAN = 0x2028; // Size 8, no instance variables.

// Offsets into some standard objects.
export const OBJ_HEADER = 0;
export const OBJ_GC = 2;
export const OBJ_SUPER = 4;
export const OBJ_CLASS = 6;
export const OBJ_MEMBERS_BASE = 8;

const GC_FIELD_0 = 0; // Initial value for the GC word.


// Each Class has a bunch of component parts:
// Object -> Behavior -> ClassDescription -> Class, Metaclass.
// So any given class also has its superobjects, plus its metaclass and *its*
// superobjects. Since the number and size of superobjects varies, only the
// actual named class gets stored here. All the others are reachable from it.
//
// The classes are *not* tenured, since they can be replaced, and the cost of
// copying them is less bad than burning up tenured space for them.
//
// START HERE: Figure out the classes - it's still a bit of a mess.
//
// Behavior has 2 instance vars, ClassDescription 0, Class 2 and Metaclass 1.
// So the total size per class row is (0 + 2 + 1) + 3 headers, and the metaclass
// trio always follows the original one.
export const IV_BEHAVIOR = 3;
export const IV_CLASS_DESCRIPTION = 0;
export const IV_CLASS = 2;
export const IV_METACLASS = 1;


export const MA_METAMETA_BEHAVIOR = 0x2030; // 8 + 6
export const MA_METAMETA_DESCRIPTION =
    MA_METAMETA_BEHAVIOR + OBJ_MEMBERS_BASE + 2 * IV_BEHAVIOR;
export const MA_METAMETA =
  MA_METAMETA_DESCRIPTION + OBJ_MEMBERS_BASE + 2 * IV_CLASS_DESCRIPTION;

// Bunch of space here.

const CLASSTABLE_BASE = 0x3000;

let nextClass_ = CLASSTABLE_BASE;


function nextClass() {
  const p = nextClass_;
  nextClass_ += OBJ_MEMBERS_BASE + 2 * IV_CLASS;
  return p;
};

export const CLS_PROTO_OBJECT = nextClass();
export const CLS_OBJECT = nextClass();
export const CLS_CLASS = nextClass();
export const CLS_BEHAVIOR = nextClass();
export const CLS_CLASS_DESCRIPTION = nextClass();
export const CLS_METACLASS = nextClass();
export const CLS_UNDEFINED_OBJECT = nextClass();

export const CLS_COLLECTION = nextClass();
export const CLS_DICTIONARY = nextClass();
export const CLS_IDENTITY_DICTIONARY = nextClass();

export const CLS_CONTEXT = nextClass();
export const CLS_COMPILED_METHOD = nextClass();
export const CLS_BLOCK_CLOSURE = nextClass();

export const CLS_AA_NODE = nextClass();

export const CLS_BOOLEAN = nextClass();
export const CLS_TRUE = nextClass();
export const CLS_FALSE = nextClass();

export const CLS_CHARACTER = nextClass();
export const CLS_MAGNITUDE = nextClass();
export const CLS_NUMBER = nextClass();
export const CLS_INTEGER = nextClass();
export const CLS_SMALL_INTEGER = nextClass();




// Returns an array of offsets, to be used with destructuring. See below.
function objInstVars(n) {
  const ret = [];
  for (let i = 0; i < n; i++) {
    ret.push(OBJ_MEMBERS_BASE + 2*i);
  }
  return ret;
}

export const [BEHAVIOR_SUPERCLASS, BEHAVIOR_METHODS, BEHAVIOR_INST_VARS] =
    objInstVars(3);
export const [CLASS_NAME, CLASS_SUBCLASSES] = objInstVars(2);
export const METACLASS_THIS_CLASS = OBJ_MEMBERS_BASE;

export const [
    PROCESS_TABLE_READY, PROCESS_TABLE_BLOCKED, PROCESS_TABLE_NEXT_PRIORITY,
] = objInstVars(3);

export const [
    PROCESS_CONTEXT, PROCESS_NEXT, PROCESS_PREV, PROCESS_PROCESS_TABLE] =
    objInstVars(4);

// Contexts are stored as 24-pointer objects.
// The first 5 values are reserved: method, locals, pc, sender, stack index.
// The remaining 19 values are the stack. The stack pointer is empty-ascending
// and in values, not words. Eg. it starts out at 0, then becomes 1, 2, etc.
export const [
  CTX_METHOD,
  CTX_LOCALS,
  CTX_PC,
  CTX_SENDER,
  CTX_STACK_INDEX,
  CTX_STACK_BASE,
] = objInstVars(6);

export const STACK_MAX_SIZE = 19;

// CompiledMethods have six instance variables: bytecode, literals, name,
// class, argc, and locals.
// bytecode is a raw array, literals a pointer array.
export const [
  METHOD_BYTECODE,
  METHOD_LITERALS,
  METHOD_NAME,
  METHOD_CLASS,
  METHOD_ARGC,
  METHOD_LOCALS,
] = objInstVars(6);


export const [BLOCK_CONTEXT, BLOCK_PC_START, BLOCK_ARGC, BLOCK_ARGV] = objInstVars(4);
export const [AA_KEY, AA_VALUE, AA_LEVEL, AA_LEFT, AA_RIGHT] = objInstVars(5);

export const CHAR_VALUE = OBJ_MEMBERS_BASE;

export const ASCII_TABLE = [];

const TYPE_MASK = 0xf0000000;
const TYPE_LEN_MASK = ~TYPE_MASK;
const TYPE_OBJ = 0;
const TYPE_RA = 1 << 28;
const TYPE_PA = 2 << 28;
const TYPE_STRING = 3 << 28;
const TYPE_SYMBOL = 4 << 28;


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
export function readWord(addr) {
  return mem[addr];
}

export function read(addr) {
  return (mem[addr] << 16) | mem[addr+1];
}

export function writeWord(addr, value) {
  return mem[addr] = value & 0xffff;
}

export function write(addr, value) {
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
export function readAt(base, offset) {
  // Defensive programming: checks the size long and disallows reads past the
  // end of non-special values.
  if (offset + 1 > objSize(base)) {
    throw new Error('reading off the end of object');
  }

  return read(base + offset);
}

// DOES NOT handle GC updates; that needs to be handled in eg. store bytecodes.
export function writeAt(base, offset, value) {
  // Special case: writing to offset 0 is always allowed, since we use it to
  // write the size!
  if (offset > 0 && offset + 1 > objSize(base)) {
    throw new Error('writing off the end of object');
  }

  return write(base + offset, value);
}

// Reads into a raw array at a given index. Returns a single word.
export function readRawArray(base, index) {
  return readWord(base + RA_BASE + index);
}

// Helper to extract a ST number to a JS number. Abstracted since this is
// likely to evolve in the VM.
// For now, numbers are stored as pointers to heap-allocated raw arrays of
// inner length 2, with the special type -1.
// NB: These are passed the pointer of the number in some other object's
// instance variables! Since for now numbers are stored on the heap, these need
// to do a nested read. They also mutate numbers on the heap(!)
export function readSmallInteger(addr) {
  return readAt(read(addr), SMALL_INTEGER_RAW);
}

export function writeSmallInteger(addr, value) {
  return writeAt(read(addr), SMALL_INTEGER_RAW, value);
}


// Working with the stack on a MethodContext.
export function push(ctx, value) {
  const sp = readSmallInteger(ctx + CTX_STACK_INDEX);
  if (sp >= STACK_MAX_SIZE) throw new Error('VM stack overflow');
  writeAt(ctx, CTX_STACK_BASE + 2*sp, value);
  checkOldToNew(ctx + CTX_STACK_BASE + 2*sp, value);
  // Increment the stack pointer.
  adjustSmallInteger(ctx + CTX_STACK_INDEX, 1);
}

export function pop(ctx) {
  const sp = readSmallInteger(ctx + CTX_STACK_INDEX);
  if (sp <= 0) throw new Error('VM stack underflow');
  const value = readAt(ctx, CTX_STACK_BASE + 2*sp);
  adjustSmallInteger(ctx + CTX_STACK_INDEX, -1);
  return value;
}

export function peek(ctx) {
  const sp = readSmallInteger(ctx + CTX_STACK_INDEX);
  const value = readAt(ctx, CTX_STACK_BASE + 2*sp);
  return value;
}



// Allocation! This is vital but not hard: given a necessary size, allocate
// space for it in the Eden and return a pointer to it.
export function alloc(size) {
  const p = vm.allocationPointer - size;
  if (p < MEM_EDEN_BASE) {
    throw new Error('Minor GC needed but not implemented!');
  }
  vm.allocationPointer = p;
  return p;
}

// Allocates size words in tenured space, returning the pointer to it.
// This is used for tenuring, but also for things like Symbols that are
// permanently cached.
export function tenure(size) {
  const p = read(MA_TENURE_ALLOC); // Raw number, not a Smalltalk value.
  write(MA_TENURE_ALLOC, p + size);
  return p;
}

export function initObject(p, cls, superObj, instVars) {
  const size = OBJ_MEMBERS_BASE + 2 * instVars;
  writeAt(p, OBJ_HEADER, size | TYPE_OBJ);
  writeAt(p, OBJ_GC, GC_FIELD_0);
  writeAt(p, OBJ_SUPER, superObj);
  writeAt(p, OBJ_CLASS, cls);

  for (let i = 0; i < instVars; i++) {
    writeAt(p, OBJ_MEMBERS_BASE + 2 * i, MA_NIL);
  }
}

// Don't call this outside of bootstrap, use mkInstance/At instead.
export function allocObject(instVars) {
  return alloc(OBJ_MEMBERS_BASE + 2 * instVars);
}

// Given the pointer to a Class, constructs a new instance of it.
// This is essentially basicNew.
// Only works for regular objects! Special types should be calling other
// primitives.
export function mkInstance(cls) {
  return mkInstanceAt(cls, allocObject);
}

// Special case of mkInstance that defines its initial pointer.
// Probably should only be called from bootstrap.mjs.
export function mkInstanceAt(cls, allocator) {
  // Special case: if cls is Object, return the canned object instance.
  if (cls === CLS_PROTO_OBJECT) return MA_PROTO_OBJECT_INSTANCE;
  if (cls === CLS_OBJECT) return MA_OBJECT_INSTANCE;

  // Recursively build the superObj instances.
  const classDescription = readAt(cls, OBJ_SUPER);
  const behavior = readAt(classDescription, OBJ_SUPER);
  const superclass = readAt(behavior, BEHAVIOR_SUPERCLASS);

  // Using the default allocator for the superclass, even if the parent class
  // is at a specific spot.
  const sup = superclass !== MA_NIL ? mkInstance(superclass) : MA_NIL;
  const instVars = readSmallInteger(behavior + BEHAVIOR_INST_VARS);
  const p = allocator(instVars);
  initObject(p, cls, sup, instVars);
  return p;
}

// Size in *pointers*!
export function allocPointerArray(size, opt_uninitialized) {
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

export function allocRawArray(size) {
  const wordSize = RA_BASE + size;
  const p = alloc(wordSize);
  writeAt(p, RA_HEADER, wordSize | TYPE_RA);
  writeAt(p, RA_GC, GC_FIELD_0);
  return p;
}


// When we're storing a pointer at p that points to target, we check to see if
// p is in the old space (< MEM_EDEN_BASE) and target in new (>= MEM_EDEN_BASE).
export function checkOldToNew(p, target) {
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
export function wrapSmallInteger(n) {
  // SmallIntegers are represented as raw arrays of size RA_BASE + 2.
  const p = allocRawArray(2);
  writeAt(p, SMALL_INTEGER_RAW, n);
  return p;
}

// Applies a (raw) delta to the small integer at p.
export function adjustSmallInteger(p, delta) {
  const n = readSmallInteger(p);
  writeSmallInteger(p, n + delta);
}

// Strings and symbols
// These are stored as raw arrays with custom types.
//
// Copies a JS string s into a (sufficiently large!) blank space at p.
// p points to the header, not the first data word.
function copyString(p, s) {
  for (let i = 0; i < s.length; i++) {
    writeWord(p + RA_BASE + i, s.charCodeAt(i));
  }
}

export function allocString(length) {
  const p = allocRawArray(length);
  writeAt(p, RA_HEADER, TYPE_STRING | (s.length + RA_BASE));
  writeAt(p, RA_GC, GC_FIELD_0);
  return p;
}

// Allocate and return the pointer to a new String containing the same
// characters as the JS string provided.
export function wrapString(s) {
  const arr = allocString(s.length);
  copyString(arr, s);
  return arr;
}

// Allocate and return the pointer to a new Symbol containing the same
// characters as the JS string provided.
// NB: Symbols are allocated in tenured space! This means their pointers are
// fixed and can be used to as raw keys in a search tree.
// Symbols are also deduplicated, currently by using a JS object.
const symbolCache = {};
export function wrapSymbol(s) {
  if (symbolCache[s]) return symbolCache[s];

  const arr = tenure(s.length + RA_BASE);
  writeAt(arr, RA_HEADER, TYPE_SYMBOL | (s.length + RA_BASE));
  writeAt(arr, RA_GC, GC_FIELD_0);
  copyString(arr, s);

  symbolCache[s] = arr;
  return arr;
}


// Some tests
export function isPointerArray(p) {
  return (readAt(arr, PA_HEADER, p) & TYPE_MASK) === TYPE_PA;
}


// Utilities
export function pointerArrayLength(p) {
  if (!isPointerArray(p)) throw new Error('pointer array required');
  const header = readAt(arr, PA_HEADER);
  return ((header & TYPE_LEN_MASK) - PA_BASE) / 2;
}

// Find the superObject in the chain that's an instance of cls.
// Throws if not found, this shouldn't be speculation!
export function findSuperObj(p, cls) {
  while (readAt(p, OBJ_CLASS) !== cls) {
    p = readAt(p, OBJ_SUPER);
    if (p === MA_NIL) throw new Error('cant happen: findSuperObj failed');
  }
  return p;
}



// Initialization
// This sets up some key values in the special area.
write(MA_TENURE_ALLOC, MEM_SPECIAL_TOP);
write(MA_OLD_GEN_START, MEM_TENURED_TOP);
write(MA_OLD_GEN_ALLOC, MEM_TENURED_TOP);
write(MA_NEW_GEN_START, MEM_G1_TOP);
write(MA_NEW_GEN_ALLOC, MEM_G1_TOP);

write(MA_ESCAPED_HEAD, MA_NIL);

vm.allocationPointer = MEM_EDEN_TOP;

