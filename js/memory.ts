// Memory access and management for our Smalltalk VM.
import {vm} from './vm';

export type ptr = number;
export type stw = number;
export type stl = number;
export type sti = number;
export type IdentityHash = number;

const MEM_SIZE = 1024 * 1024 * 64; // 64MW = 128MB for now.
const MEM_SPECIAL_TOP: ptr = 0x10000; // 64KW of magic should be plenty.

const MW4 = 4 * 1024 * 1024; // 4 MW is our unit size for several things.

const MEM_TENURED_TOP: ptr = MW4; // 4MW of tenure + special.
const MEM_G1_TOP: ptr = MEM_TENURED_TOP + MW4; // 4MW for each generation
const MEM_G2_TOP: ptr = MEM_G1_TOP + MW4;      // 4MW for each generation
const MEM_EDEN_BASE: ptr = MEM_G2_TOP;         // Eden gets the rest = 42MW
const MEM_EDEN_TOP: ptr = MEM_SIZE;

const mem = new Uint16Array(MEM_SIZE);

const GC_FIELD_0 = 0; // Initial value of the GC field in the object header.

// Bunch of values in the special space, pointers into the different memory
// areas.
// MA_ is "mem addr".

// Pointers to the ends of the ranges for new and old generations. (Indirected
// because they get swapped.)
const MA_OLD_GEN_START: ptr = 0x1000;
const MA_OLD_GEN_ALLOC: ptr = 0x1002;
const MA_NEW_GEN_START: ptr = 0x1004;
const MA_NEW_GEN_ALLOC: ptr = 0x1006;
const MA_TENURE_ALLOC: ptr = 0x1008;

// Points to the first escaped pointer linked list entry; see GC design.
const MA_ESCAPED_HEAD: ptr = 0x100a;
// Class dictionary.
export const MA_GLOBALS: ptr = 0x100c;
export const MA_NEXT_CLASS_INDEX: ptr = 0x100e;

export const MASK_CLASS_INDEX: ptr = 0x3fffff;

// Some canned ST objects that exist at known places in memory.
export const MA_NIL: ptr   = 0x2000; // Size 4, zero-size object
export const MA_TRUE: ptr  = 0x2004; // Size 4, zero-size object
export const MA_FALSE: ptr = 0x2008; // Size 4, zero-size object

// The classes are *not* tenured, since they can be replaced, and the cost of
// copying them is less bad than burning up tenured space for them.
//
// Instead, we keep a table of class pointers in special memory, with the
// classes at arbitrary locations. They get treated like ordinary old->new
// pointers, updated, etc. The identity hash of a class is a (pointer) index
// into this table, and MA_NEXT_CLASS_INDEX holds the next empty slot.
//
// Behavior has 3 instance vars, ClassDescription 0, Class 2 and Metaclass 1.
export const IV_BEHAVIOR = 3;
export const IV_CLASS_DESCRIPTION = 0;
export const IV_CLASS = 2;
export const IV_METACLASS = 1;


const CLASSTABLE_BASE: ptr = 0x3000;
const CLASSTABLE_SIZE = 0x1000; // 4096 words = 2048 classes
const CLASSTABLE_TOP: ptr = CLASSTABLE_BASE + CLASSTABLE_SIZE;

// Some canned class table entries, since we need to refer statically to a bunch
// of them.

let nextClass_ = 0;

function nextClass(): number {
  return nextClass_++;
};

// These class literals are class table *indexes*, from 0 to N.
// They can be turned into pointers into the class table with
// classTable(CLS_FOO) and read to produce real object pointers with
// read(classTable(CLS_FOO)).
// BE CAREFUL REORDERING THESE - This list is duplicated in the compiler.
// If you reorder it or add things, it'll need to be updated.
export const CLS_PROTO_OBJECT = nextClass();
export const CLS_OBJECT = nextClass();
export const CLS_BEHAVIOR = nextClass();
export const CLS_CLASS_DESCRIPTION = nextClass();
export const CLS_CLASS = nextClass();
export const CLS_METACLASS = nextClass();
export const CLS_UNDEFINED_OBJECT = nextClass();

// Class index: 7
export const CLS_COLLECTION = nextClass();
export const CLS_SEQUENCEABLE_COLLECTION = nextClass();
export const CLS_ARRAYED_COLLECTION = nextClass();
export const CLS_ARRAY = nextClass();
export const CLS_STRING = nextClass();
export const CLS_SYMBOL = nextClass();
export const CLS_HASHED_COLLECTION = nextClass();
export const CLS_DICTIONARY = nextClass();
export const CLS_IDENTITY_DICTIONARY = nextClass();

// Class index 16:
export const CLS_CONTEXT = nextClass();
export const CLS_COMPILED_METHOD = nextClass();
export const CLS_BLOCK_CLOSURE = nextClass();

// Class index 19:
export const CLS_BOOLEAN = nextClass();
export const CLS_TRUE = nextClass();
export const CLS_FALSE = nextClass();

// Class index 22:
export const CLS_MAGNITUDE = nextClass();
export const CLS_CHARACTER = nextClass();
export const CLS_NUMBER = nextClass();
export const CLS_INTEGER = nextClass();
export const CLS_SMALL_INTEGER = nextClass();
export const CLS_ASSOCIATION = nextClass();

// Class index 28:
export const CLS_WORD_ARRAY = nextClass();
export const CLS_PROCESS = nextClass();
export const CLS_PROCESS_TABLE = nextClass();
// BE CAREFUL REORDERING - see above note.


// Returns an array of ordered indexes.
export function seq(n: number): Array<number> {
  const ret = [];
  for (let i = 0; i < n; i++) {
    ret.push(i);
  }
  return ret;
}

export const [
  BEHAVIOR_SUPERCLASS, BEHAVIOR_METHODS, BEHAVIOR_FORMAT,
  CLASS_NAME, CLASS_SUBCLASSES, CLASS_VAR1, // Index for the first class var.
] = seq(6);

export const METACLASS_THIS_CLASS = CLASS_NAME; // Same field, after Behavior.

export const [
    PROCESS_TABLE_READY, PROCESS_TABLE_BLOCKED, PROCESS_TABLE_NEXT_PRIORITY,
] = seq(3);

export const [
    PROCESS_CONTEXT, PROCESS_NEXT, PROCESS_PREV, PROCESS_PROCESS_TABLE] =
    seq(4);

export const CHARACTER_ASCII_VALUE = 0;

// Contexts are stored as 24-pointer objects.
// They are (currently?) the only Format.VARIABLE_IV object.
// They have 5 instVars: method, locals, pc, sender, stack index.
// The remaining 19 values are the stack. The stack pointer is empty-ascending
// and in values, not words. Eg. it starts out at 0, then becomes 1, 2, etc.
export const [
  CTX_METHOD,
  CTX_LOCALS,
  CTX_PC,
  CTX_SENDER,
  CTX_STACK_INDEX,
] = seq(5);

export const STACK_MAX_SIZE = 19;

// CompiledMethods have six instance variables: bytecode, literals, name,
// class, argc, and locals.
// bytecode is a raw array, literals a pointer array.
export const IV_METHOD = 6;
export const [
  METHOD_BYTECODE,
  METHOD_LITERALS,
  METHOD_NAME,
  METHOD_CLASS,
  METHOD_ARGC,
  METHOD_LOCALS,
] = seq(IV_METHOD);


export const IV_BLOCK = 5;
export const [BLOCK_CONTEXT, BLOCK_PC_START, BLOCK_ARGC, BLOCK_ARGV, BLOCK_HANDLER_ACTIVE] = seq(IV_BLOCK);

export const [DICT_ARRAY, DICT_TALLY] = seq(2);
export const [ASSOC_KEY, ASSOC_VALUE] = seq(2);

export const [POINT_X, POINT_Y] = seq(2);
export const [COLOR_STRING] = seq(1);
export const [RECT_ORIGIN, RECT_EXTENT] = seq(2);

// Functions for accessing the memory.
// Longs are stored big-endian, ie. the high word is at the lower address.
export function readWord(addr: ptr): stw {
  return mem[addr];
}

export function read(addr: ptr): stl {
  return (mem[addr] << 16) | mem[addr+1];
}

export function writeWord(addr: ptr, value: stw) {
  mem[addr] = value & 0xffff;
}

export function write(addr: ptr, value: stl) {
  writeWord(addr, value >> 16);
  writeWord(addr + 1, value);
}


// Object header format
// ssssssss __hhhhhh hhhhhhhh hhhhhhhh
// ggggffff __cccccc cccccccc cccccccc


// Functions for deriving properties of objects, given a pointer to the header.
// Turns a class table index into a pointer into the *class table*. It must be
// read() to get the real pointer to the class.
export function classTable(index: number): ptr {
  return CLASSTABLE_BASE + 2 * index;
}

// Returns the real pointer to the class of this object, indirecting through the
// class table.
export function classOf(p: ptr): ptr {
  const classIndex = isSmallInteger(p) ? // Special case for inlined integers.
      CLS_SMALL_INTEGER :
      read(p+2) & MASK_CLASS_INDEX;
  return read(classTable(classIndex));
}

export function hasClass(p: ptr, classIndex: number): boolean {
  return isSmallInteger(p) ?
      classIndex === CLS_SMALL_INTEGER :
      (read(p+2) & MASK_CLASS_INDEX) === classIndex;
}

// Returns the format tag
function format(p: ptr): Format {
  return (readWord(p+2) >> 8) & 0xf;
}

export enum Format {
  ZERO = 0,
  FIXED_IV = 1,
  VARIABLE = 2,
  VARIABLE_IV = 3,
  WORDS_EVEN = 6,
  WORDS_ODD = 7,
}

// "Decodes" the header for a pointer-type object: that is, one with a header
// that isn't holding words and isn't a special type like 8+.
// The returned value gives:
// {
//   iv: [count, ptr to first]  count in pointers
//   pa: [count, ptr to first]  count in pointers
//   wa: [count, ptr to first]  count in words
//   start: the absolute starting pointer, which might be before p.
//   length: the length in words from start to the end.
// }
// but these values will be missing if they're not relevant.
interface header {
  pa?: [number, ptr];
  wa?: [number, ptr];
  iv?: [number, ptr];
  start: ptr;
  length: number;
}

function decodeHeader(p: ptr): header {
  let size = readWord(p) >>> 8;
  let varSize: number;
  const fmt = format(p);
  switch (fmt) {
    case Format.ZERO:
      return {start: p, length: 4};
    case Format.VARIABLE_IV:
      varSize = read(p - 2);
      return {
        iv: [size, p + 4],
        pa: [varSize, p + 4 + 2 * size],
        start: p - 4,
        length: (size + varSize) * 2 + 8,
      };
    case Format.VARIABLE:
      varSize = size === 255 ? read(p - 2) : size;
      return {
        pa: [varSize, p + 4],
        start: size === 255 ? p - 4 : p,
        length: varSize * 2 + (size === 255 ? 8 : 4),
      };
    case Format.FIXED_IV:
      const ivs = size === 255 ? read(p + 4) : size;
      return {
        iv: [ivs, p + 4],
        start: size === 255 ? p - 4 : p,
        length: ivs * 2 + (size === 255 ? 8 : 4),
      };
    case Format.WORDS_ODD:
    case Format.WORDS_EVEN:
      let start = p;
      let length = size*2 + 4;
      const ret: [number, ptr] = [2*size, p + 4];
      if (size === 255) {
        ret[0] = read(p - 2) * 2;
        length = ret[0] + 8;
        start -= 4;
      }
      if (fmt === Format.WORDS_ODD) {
        ret[0]--; // Odd means the last slot isn't filled.
        length--;
      }
      return {wa: ret, start, length};
    default:
      throw new Error('Don\'t know how to decode header format ' + fmt);
  }
  throw new Error('cant happen');
}


// Some helpers for reading from objects.
// These have sanity checks for eg. trying to read the IVs of a variable type.
// 0-based indexing.
export function readIV(p: ptr, index: number): stl {
  const hdr = decodeHeader(p);
  if (!hdr.iv) throw new Error('Cannot read IVs of non-IV object: ' + p);
  if (hdr.iv[0] <= index) throw new Error('Reading off the end of object');

  return read(hdr.iv[1] + 2 * index);
}

export function readArray(p: ptr, index: number): stl {
  const hdr = decodeHeader(p);
  if (!hdr.pa) {
    throw new Error('Cannot read variable-length part of fixed object');
  }
  if (hdr.pa[0] <= index) throw new Error('Reading off the end of array');

  return read(hdr.pa[1] + 2 * index);
}

export function readWordArray(p: ptr, index: number): stw {
  const hdr = decodeHeader(p);
  if (!hdr.wa) throw new Error('Cannot read word array of non-words object');
  if (hdr.wa[0] <= index) throw new Error('Reading off the end of word array');

  return readWord(hdr.wa[1] + index);
}


// The writes come in two forms: write*New when we can guarantee the write is
// happening into a new space, and the other when it might be into an old space
// where a check needs to be performed.
//
// These return the address actually written, mostly to make checkOldToNew easy.
export function writeIVNew(p: ptr, index: number, value: stl): ptr {
  const hdr = decodeHeader(p);
  if (!hdr.iv) throw new Error('Cannot write IVs of non-IV object');
  if (hdr.iv[0] <= index) throw new Error('Writing off the end of IVs');
  const addr = hdr.iv[1] + 2 * index;
  write(addr, value);
  return addr;
}

export function writeIV(p: ptr, index: number, value: stl): ptr {
  const addr = writeIVNew(p, index, value);
  checkOldToNew(addr, value);
  return addr;
}

export function writeArrayNew(p: ptr, index: number, value: stl): ptr {
  const hdr = decodeHeader(p);
  if (!hdr.pa) throw new Error('Cannot write array of non-array');
  if (hdr.pa[0] <= index) throw new Error('Writing off the end of pointer array');
  const addr = hdr.pa[1] + 2 * index;
  write(addr, value);
  return addr;
}

export function writeArray(p: ptr, index: number, value: stl): ptr {
  const addr = writeArrayNew(p, index, value);
  checkOldToNew(addr, value);
  return addr;
}

// No need for the "new" variant of a word array; they're not pointers.
export function writeWordArray(p: ptr, index: number, value: stl): ptr {
  const hdr = decodeHeader(p);
  if (!hdr.wa) throw new Error('Cannot write word array of non-words object');
  if (hdr.wa[0] <= index) throw new Error('Writing off the end of word array');
  const addr = hdr.wa[1] + index;
  writeWord(addr, value);
  return addr;
}


// Helper to extract a ST number to a JS number. Abstracted since this is
// subject to evolution in the VM.
// SmallIntegers are not pointers, they are stored as 31-bit 2s complement
// numbers with the top bit set. Since there's unlikely to be 2GW of memory,
// this bit is clear on pointers.
//
// Given a would-be pointer, decode it as a number. Throws if it's not a number!
export function fromSmallInteger(p: ptr): sti {
  if (p >= 0) throw new Error('pointers found in readSmallInteger');
  // Shift left, then *arithmetic* shift right.
  return (p << 1) >> 1;
}

// Given a JS number, encodes it as a SmallInteger, a pseudo-pointer with the
// top bit always set. The top bit is lost.
export function toSmallInteger(n: sti): ptr {
  return n | 0x80000000;
}

export function isSmallInteger(p: ptr): boolean {
  return p < 0;
}


// Working with the stack on a MethodContext. The stack pointer is in an IV,
// giving a 0-based index into the variable portion.
export function push(ctx: ptr, value: stl) {
  const ptrs = gcTemps(2);
  ptrs[0] = ctx;
  ptrs[1] = value;
  const sp = fromSmallInteger(readIV(ptrs[0], CTX_STACK_INDEX));
  if (sp >= STACK_MAX_SIZE) throw new Error('VM stack overflow');
  writeArray(ptrs[0], sp, ptrs[1]);
  // Always a number, safe to skip the checks.
  writeIVNew(ptrs[0], CTX_STACK_INDEX, toSmallInteger(sp + 1));
  gcRelease(ptrs);
}

export function pop(ctx: ptr): stl {
  const sp = fromSmallInteger(readIV(ctx, CTX_STACK_INDEX));
  if (sp <= 0) throw new Error('VM stack underflow');
  const value = readArray(ctx, sp - 1);
  // Always a number, safe to skip the checks.
  writeIVNew(ctx, CTX_STACK_INDEX, toSmallInteger(sp - 1));
  return value;
}

export function peek(ctx: ptr, nesting = 0): stl {
  const sp = fromSmallInteger(readIV(ctx, CTX_STACK_INDEX));
  return readArray(ctx, sp - 1 - (2*nesting));
}

export function adjustSP(ctx: ptr, deltaSlots: number) {
  const sp = fromSmallInteger(readIV(ctx, CTX_STACK_INDEX));
  writeIVNew(ctx, CTX_STACK_INDEX, toSmallInteger(sp - 2*deltaSlots));
}



// Allocation! This is vital but not hard: given a necessary size, allocate
// space for it in the Eden and return a pointer to it.
// This is the raw allocation function - it allocates in *words*. It rounds up
// to keep the Eden 32-bit aligned.
export function alloc(size: number): ptr {
  const p = (vm.allocationPointer - size) & ~1;
  if (p < MEM_EDEN_BASE) {
    minorGC();
    return alloc(size); // Recursively alloc after the GC.
  }
  vm.allocationPointer = p;
  return p;
}

// Allocates size words in tenured space, returning the pointer to it.
// This is used for tenuring, but also for things like Symbols that are
// permanently cached.
// Keeps the tenure pointer 32-bit aligned.
export function tenure(size: number): ptr {
  const p = read(MA_TENURE_ALLOC); // Raw number, not a Smalltalk value.
  write(MA_TENURE_ALLOC, (p + size + 1) & ~1); // Keep it aligned.
  return p;
}

// Computes the size required for a regular object with the specified instVars
// and array length. Both can be 0, in which case a Format.ZERO object is
// assumed. This accounts for the size word and so on.
// Generally you can call allocObject() to do this all in one, but this is
// useful if allocating to the tenured space or a special area.
// Returns the length in *words*, suitable for passing to tenure() or alloc().
export function sizeRequired(instVars: number, arrayLength: number): number {
  if (instVars === 0 && arrayLength === 0) return 4; // Just the header.

  const total = instVars + arrayLength;
  // Variables plus IVs requires the length word every time.
  if (instVars > 0 && arrayLength > 0) {
    return 8 + 2 * total;
  }
  return 4 + 2 * total + (total >= 255 ? 4 : 0);
}

// Just a round robin of the 22-bit space for each allocation.
// Perhaps this should be a PRNG for better scattering? This plan keeps the low
// bits changing fast, which is I guess okay for hash tables and truncation.
function nextIdentityHash(): IdentityHash {
  const hash = vm.nextIdentityHash;
  vm.nextIdentityHash = (vm.nextIdentityHash + 1) & MASK_CLASS_INDEX;
  return hash;
}

// Reads the identityHash out of an object header.
export function identityHash(p: ptr): IdentityHash {
  return read(p) & MASK_CLASS_INDEX;
}

// Inits any pointer-valued object. instVars, arrayLength or both can be 0.
// p is assumed to already point to a large enough memory area!
// You probably want to call allocObject() instead if allocating to Eden.
// You can compute the necessary size for same with sizeRequired().
// Returns updated p! This should be used, as there might be an extra size at
// the input p, which is no longer the object's pointer.
export function initObject(p: ptr, clsHash: IdentityHash,
    instVars: number, arrayLength: number): ptr {
  const hash = nextIdentityHash();
  let fmt, sizeField;
  let extraLength = null;

  if (instVars > 0 && arrayLength > 0) {
    fmt = Format.VARIABLE_IV;
    sizeField = instVars;
    extraLength = arrayLength;
  } else if (instVars === 0 && arrayLength === 0) {
    fmt = Format.ZERO;
    sizeField = 0;
  } else {
    fmt = instVars > 0 ? Format.FIXED_IV : Format.VARIABLE;
    const count = instVars + arrayLength;
    sizeField = Math.min(count, 255);
    extraLength = count >= 255 ? count : null;
  }

  if (extraLength !== null) {
    write(p, 0xff000000);      // Marker long
    write(p + 2, extraLength); // Actual size
    p += 4; // p is now the actual object pointer.
  }

  write(p, (sizeField << 24) | hash);

  // clsHash is the identity hash for this class.
  const gcField = GC_FIELD_0;
  write(p + 2, (gcField << 28) | (fmt << 24) | clsHash);

  // Then write all the instVars and array values as nil!
  // We're writing nil, so these raw writes are safe (and aren't old->new).
  for (let i = 0; i < instVars + arrayLength; i++) {
    write(p + 4 + 2 * i, MA_NIL);
  }

  return p;
}

function allocWordArray(clsHash: IdentityHash, arrayLength: number,
    allocator: (size: number) => ptr) {
  // The size field actually is in words, so it only overflows for 255 * 2 = 510
  const size = 4 + arrayLength + (arrayLength >= 510 ? 4 : 0);
  let p = allocator(size);

  // The size field is in words, and the last bit is captured by the even/odd
  // formats.
  const longSize = (arrayLength + 1) >> 1;
  const fmt = (arrayLength & 1) === 1 ? Format.WORDS_ODD : Format.WORDS_EVEN;

  if (longSize >= 255) {
    write(p, 0xff000000);   // Marker long.
    write(p + 2, longSize); // Actual size.
    p += 4;
  }

  const hash = nextIdentityHash();
  const sizeField = Math.min(longSize, 255);
  write(p, (sizeField << 24) | hash);

  // The class index is the identity hash of the class's own header.
  write(p + 2, (GC_FIELD_0 << 28) | (fmt << 24) | clsHash);

  return p;
}

// Allocates an instance of the specified class, with the specified number of
// instVars and variable array. Allocates in the Eden (use sizeRequired() +
// tenure()/a fixed area + initObject() to roll your own; that's all this
// method does.
// NB: DO NOT CALL outside of bootstrap. Use basicNew(cls) instead; it knows how
// to look up this information from the class.
export function allocObject(clsHash: IdentityHash, instVars: number,
    arrayLength: number, allocator: (size: number) => ptr) {
  const size = sizeRequired(instVars, arrayLength);
  const p = allocator(size);
  return initObject(p, clsHash, instVars, arrayLength);
}

// Creates a new object with the inst vars from its format field, and with the
// optional array length (eg. from #new:)
// Throws if trying to set arrayLength on a non-variable format!
// The length is in words for word-type formats and pointers otherwise.
//
// opt_allocator is alloc, tenure, or something else with the same signature.
export function mkInstance(cls: ptr, arrayLength = 0, allocator = alloc) {
  // Read the format number out of the class. This is defined on Behavior, and
  // is encoded as ____ffff sssssss ssssssss ssssssss, a 24-bit size field.
  const fmtRaw = fromSmallInteger(readIV(cls, BEHAVIOR_FORMAT));
  const fmt = (fmtRaw >> 24) & 0xf;
  const instVars = fmtRaw & 0xffffff;

  const isFixed = fmt === Format.ZERO || fmt === Format.FIXED_IV;
  if (arrayLength > 0 && isFixed) {
    throw new Error('Illegal: non-variable class with nonzero array length');
  }
  // Creating a 0-length Array etc. is technically legal, so no error for that.

  // Allocate and initialize such an object.
  const clsHash = identityHash(cls);
  return (fmt === Format.WORDS_ODD || fmt === Format.WORDS_EVEN) ?
      allocWordArray(clsHash, arrayLength, allocator) :
      allocObject(clsHash, instVars, arrayLength, allocator);
}

// Given the pointer to a Class, constructs a new instance of it.
// This is essentially basicNew.
// Only works for regular objects! Special types should be calling other
// primitives.
export function basicNew(cls: ptr): ptr {
  return mkInstance(cls);
}


// When we're storing a pointer at p that points to target, we check to see if
// p is in the old space (< MEM_EDEN_BASE) and target in new (>= MEM_EDEN_BASE).
// If it is, we create a new Association in Eden that points to *p*. Then later
// the GC can check if p still contains a newspace pointer, recursively copy it,
// and update p.
export function checkOldToNew(p: ptr, target: ptr) {
  if (isSmallInteger(target)) return; // No pointer to track.

  const isOld = p < MEM_EDEN_BASE;
  const isNew = target >= MEM_EDEN_BASE;
  if (isOld && isNew) {
    // We store these records as Associations, which have 2 IVs.
    const [v_input, v_head, v_link] = seq(3);
    const ptrs = gcTemps(3);
    ptrs[v_input] = p;
    ptrs[v_link] = mkInstance(read(classTable(CLS_ASSOCIATION)));
    ptrs[v_head] = read(MA_ESCAPED_HEAD);
    writeIV(ptrs[v_link], ASSOC_KEY, ptrs[v_input]);
    writeIV(ptrs[v_link], ASSOC_VALUE, ptrs[v_head]);
    write(MA_ESCAPED_HEAD, ptrs[v_link]);
    gcRelease(ptrs);
  }
}

// Copies an object from src to dst. Returns the total length in words copied,
// and a possibly-adjusted destination pointer. This allows for copying objects
// with extra length words.
function copyObject(src: ptr, dst: ptr): [ptr, number] {
  const hdr = decodeHeader(src);
  for (let i = 0; i < hdr.length; i++) {
    writeWord(dst + i, readWord(hdr.start + i));
  }

  const realDst = src === hdr.start ? dst : dst + 4;
  gcMap[realDst] = {
    origin: src,
    ptrFields: [],
  };
  if (hdr.iv) {
    for (let i = 0; i < hdr.iv[0]; i++) {
      gcMap[realDst].ptrFields.push(read(hdr.iv[1] + 2*i));
    }
  }
  if (hdr.pa) {
    for (let i = 0; i < hdr.pa[0]; i++) {
      gcMap[realDst].ptrFields.push(read(hdr.pa[1] + 2*i));
    }
  }

  return [realDst, hdr.length];
}

// Given a pointer to a variable-sized object, return the length of its variable
// portion in the relevant unit - pointers or words.
export function arraySize(p: ptr): number {
  const hdr = decodeHeader(p);
  if (hdr.pa) return hdr.pa[0];
  if (!hdr.wa && !hdr.iv) return 0; // Special case: 0-length array
  throw new Error('Cannot take the array size of a non-array object');
}

export function wordArraySize(p: ptr): number {
  const hdr = decodeHeader(p);
  if (hdr.wa) return hdr.wa[0];
  throw new Error('Cannot take the word array size of a non-word-array object');
}


// Strings and symbols
// These are stored as raw arrays with custom types.
//
// Copies a JS string s into a (sufficiently large!) blank space at p.
// p points to the header, not the first data word.
function copyString(p: ptr, s: string) {
  for (let i = 0; i < s.length; i++) {
    writeWordArray(p, i, s.charCodeAt(i));
  }
}

// Allocate and return the pointer to a new String containing the same
// characters as the JS string provided.
export function wrapString(s: string): ptr {
  const arr = mkInstance(read(classTable(CLS_STRING)), s.length);
  copyString(arr, s);
  return arr;
}

// Given a pointer to a String or Symbol, read it into a JS string.
export function asJSString(p: ptr): string {
  const len = wordArraySize(p);
  let s = '';
  for (let i = 0; i < len; i++) {
    s += String.fromCharCode(readWordArray(p, i));
  }
  return s;
}

// Allocate and return the pointer to a new Symbol containing the same
// characters as the JS string provided.
// NB: Symbols are allocated in tenured space! This means their pointers are
// fixed and can be used to as raw keys in a search tree.
// Symbols are also deduplicated, currently by using a JS object.
const symbolCache: {[sym: string]: ptr} = {};
export function wrapSymbol(s: string): ptr {
  if (symbolCache[s]) return symbolCache[s];

  // Allocate our symbol in the tenured space.
  const arr = mkInstance(read(classTable(CLS_SYMBOL)), s.length, tenure);
  copyString(arr, s);

  symbolCache[s] = arr;
  return arr;
}


export function addClassToTable(cls: ptr, opt_classHash?: IdentityHash):
    IdentityHash {
  const next = opt_classHash || read(MA_NEXT_CLASS_INDEX); // Raw number.

  // Read the object header of the class, and rewrite its identity hash.
  const hdr1 = read(cls);
  write(cls, (hdr1 & ~MASK_CLASS_INDEX) | next);
  if (typeof opt_classHash === 'undefined') {
    write(MA_NEXT_CLASS_INDEX, next + 1);
  }

  // Store the class itself into the table.
  const addr = classTable(next);
  if (addr >= CLASSTABLE_TOP) throw new Error('class table full!');
  write(addr, cls);
  // The class table is already GC roots, so no need to old->new it.
  return next;
}

// Given a pointer to a class, return the number of inst vars it defines.
// This is complicated because the format field is not a simple number.
export function behaviorToInstVars(cls: ptr): number {
  const fmtRaw = fromSmallInteger(readIV(cls, BEHAVIOR_FORMAT));
  const fmt = (fmtRaw >> 24) & 0xf;
  return fmtRaw & 0xffffff;
}

// Encodes a Behavior's format field for a Format.FIXED_IV
export function fixedInstVarsFormat(instVars: number): stl {
  return instVars === 0 ?
      (Format.ZERO << 24) :
      (Format.FIXED_IV << 24) | instVars;
}

export function methodFor(ctx: ptr): ptr {
  const methodOrBlock = readIV(ctx, CTX_METHOD);
  return hasClass(methodOrBlock, CLS_COMPILED_METHOD) ?
      methodOrBlock :
      readIV(readIV(methodOrBlock, BLOCK_CONTEXT), CTX_METHOD);
}

export function className(cls: ptr): string {
  if (hasClass(cls, CLS_METACLASS)) {
    return className(readIV(cls, METACLASS_THIS_CLASS)) + ' class';
  }
  return asJSString(readIV(cls, CLASS_NAME));
}

export function edenFreeSpaceRatio(): number {
  return (vm.allocationPointer - MEM_EDEN_BASE) /
      (MEM_EDEN_TOP - MEM_EDEN_BASE);
}


// Initialization
// This sets up some key values in the special area.
write(MA_TENURE_ALLOC, MEM_SPECIAL_TOP);
write(MA_OLD_GEN_START, MEM_TENURED_TOP);
write(MA_OLD_GEN_ALLOC, MEM_TENURED_TOP);
write(MA_NEW_GEN_START, MEM_G1_TOP);
write(MA_NEW_GEN_ALLOC, MEM_G1_TOP);
write(MA_NEXT_CLASS_INDEX, nextClass_);

write(MA_ESCAPED_HEAD, MA_NIL);

vm.allocationPointer = MEM_EDEN_TOP;



// GC system!

// The space is divided into four unequal regions: Tenured, G1, G2 and Eden.
// Currently the scheme divides the available memory into 4MW chunks, and to
// make Tenured, G1 and G2 each 4MW. The rest of the space (eg. 42MW in a 64MW
// address space) is Eden.
//
// - Tenured is permanent, never collected. (For now, eventually it could be
//   subject to mark-and-sweep for a long-running ST image.)
// - G1 and G2 are the old gen and new gen, whose roles swap during major
//   collections.
// - Eden is the new space where objects are allocated. It grows downward, which
//   is handy for checking whether we've overrun because we have a pointer to
//   the head of an object to compare directly against the minimum.
//
// Minor collections happen when we want to allocate something but there's not
// enough room in the Eden for it.
//
// - Shelve the in-progress allocation by saving registers.
// - Scan the GC roots (see below) for pointers into Eden. All reachable Eden
//   values get copied into new gen (one of G1 and G2).
// - Deep scan of the linked list of pointers-of-interest. These are pointers
//   from old spaces (non-Eden) into Eden. They point **at the source in old
//   space**, so it can be updated proprly.
//   - If the source pointer no longer points into Eden, we can just drop this
//     entry from the chain.
//   - Surviving links in Eden get (re)allocated in new gen.
//
// GC roots:
// - Class table
// - Linked list of oldspace pointers.
// - Active processes and their contexts.
// - The chain of temporaries inside the VM.

const gcTempList: Array<ptr[]> = [];

export function gcTemps(size: number): ptr[] {
  const list: ptr[] = [];
  for (let i = 0; i < size; i++) {
    list.push(MA_NIL);
  }
  gcTempList.push(list);
  return list;
}

export function gcRelease(temps: ptr[]) {
  if (gcTempList[gcTempList.length - 1] !== temps) {
    throw new Error('temp list stacking error');
  }
  gcTempList.pop();
}


// These are internal to the GC process, and treated as registers.
// They don't need to be saved into the memory proper, since they aren't used
// outside the (atomic) GC operations.
let scan, next: ptr;

// The fixed memory regions are called G1 and G2. "Old" and "new" generations
// get swapped around, so they have pointers. A minor GC allocates to the tail
// end of the new gen.
// TODO What happens if it doesn't fit? Can we swap to a major collection on the
// fly? Or do we just cross our fingers and set a generous major GC fullness
// threshold? I'm running with the latter for now.
function minorGC() {
  console.time('minor GC');
  scan = next = read(MA_NEW_GEN_ALLOC); // The lowest free word of new space.
  const __start = scan;

  // Run through all the root pointers and copy them into the new space.
  const classTop = read(MA_NEXT_CLASS_INDEX); // A raw number, not SmallInteger.
  for (let i = 0; i < classTop; i++) {
    const cls = classTable(i);
    write(cls, forward(read(cls)));
  }

  // And the process table (which includes all recursively-reachable objects).
  vm.processTable = forward(vm.processTable);
  write(MA_GLOBALS, forward(read(MA_GLOBALS)));

  // And the chain of (possible) old->new pointers.
  let chain = read(MA_ESCAPED_HEAD);
  while (chain != MA_NIL) {
    // The key (first cell) holds the non-Eden pointer that (at one time)
    // pointed to Eden. We check if it still does, and if so we forward it.
    // Either way, this Association is no longer needed.
    const p = readIV(chain, ASSOC_KEY); // Old space pointer!
    const atP = read(p); // Pointer stored there, possible aimed at Eden.
    if (p < MEM_EDEN_BASE && atP >= MEM_EDEN_BASE) {
      write(p, forward(atP));
    }
    // If p is in Eden, it's either garbage and can be ignored, or it's not and
    // both p and atP will get copied.
    // If atP has changed to be no longer in Eden, there's nothing to do either.
    // Either way, the chain can be discarded.
    chain = readIV(chain, ASSOC_VALUE);
  }
  write(MA_ESCAPED_HEAD, MA_NIL);

  for (let i = 0; i < gcTempList.length; i++) {
    for (let j = 0; j < gcTempList[i].length; j++) {
      gcTempList[i][j] = forward(gcTempList[i][j]);
    }
  }

  // Now the queue is populated with all the roots. We can proceed with the
  // breadth-first search. SCAN chases NEXT until it catches up.
  while (scan !== next) {
    // Scan points at the start of an object's record. It might have a long
    // size, in which case this first word has 0xff00 in it.
    let longLength = false;
    if (readWord(scan) === 0xff00) {
      scan += 4;
      longLength = true;
    }

    const hdr = decodeHeader(scan);
    scan += 4; // Pointing at either the next scan or the first field.
    if (hdr.iv) {
      scan += hdr.iv[0] * 2;
      forwardArray(hdr.iv[1], hdr.iv[0]);
    }
    if (hdr.pa) {
      scan += hdr.pa[0] * 2;
      forwardArray(hdr.pa[1], hdr.pa[0]);
    }
    if (hdr.wa) {
      scan += (hdr.wa[0] + 1) & ~1; // Rounding up to keep it aligned.
      // Word arrays don't need to be copied, just skipped over.
    }
    if (scan % 2 === 1) debugger;
    // Scan now points at the next object in the queue.
  }

  // Should already have been copied, just grabbing the forwarded value.
  vm.runningProcess = forward(vm.runningProcess);
  vm.ctx = forward(vm.ctx);
  // processTable was already done above.

  // GC complete! We're ready to start allocating into the Eden again.
  write(MA_NEW_GEN_ALLOC, next);
  vm.allocationPointer = MEM_EDEN_TOP;
  console.timeEnd('minor GC');
  console.log('copied ' + (next - __start) + ' words. NewGen at ' +
      ((next - read(MA_NEW_GEN_START)) / (MEM_G2_TOP-MEM_G1_TOP)));

  // Sanity-check time: all copied objects should have had their pointers
  // forwarded properly. We can use gcMap to check them all.
  gcSanityCheck(__start);
}

function forwardArray(array: ptr, count: number) {
  for (let i = 0; i < count; i++) {
    const x = read(array + 2*i);
    if (!isSmallInteger(x)) {
      write(array + 2*i, forward(x));
    }
  }
}


// Copy process
// ============
//
// The copy uses the values copied into the new space as the queue for a
// breadth-first search.
//
// We define two pointers: scan and next, which start at the beginning of the
// new space. (Remember, old space is any non-Eden area; new space is the
// generation we're copying into right now.)
//
// For each root pointer R, we perform the following steps, replacing R by
// forward(R):
// forward(R) =
//   if R points into the Eden:
//     if the header at [R] is already redirected (see below):
//       return [R], since it's already forwarded.
//     else:
//       copy the record pointed to by R to NEXT
//       assign NEXT into [R] and -1 into [R+2].
//       increment NEXT to point past the copy of the record.
//       return [R] (the old NEXT)
//   else:
//     return R (this pointer doesn't need to change)
//
// After forward() is applied to each root pointer, populating the BFS queue,
// we start scanning that queue and recursively copying all reachable values.
// The SCAN pointer is successively incremented over new space, and forward() is
// applied to each pointer in it. That may of course copy more records, moving
// NEXT. When SCAN catches up with NEXT, the copy is complete.
//
// Advancing SCAN needs to be smart for a few reasons:
// - It has to handle objects with IVs, pointer arrays, and word arrays.
// - It has to handle the length overflow words.
//
// Redirection:
// Old values in the Eden get defaced by the forward() process to point to where
// they can be found in the new space. Its header long [p] becomes the
// redirection pointer, and its second word is all 1s to signal it's been
// redirected. (This is impossible to confuse for a regular object since the GC
// field in the upper 8 bits is never otherwise ff.)
interface gcObj {
  origin: ptr;
  ptrFields: ptr[];
}
const gcMap: {[dstPtr: ptr]: gcObj} = {};
function forward(p: ptr): ptr {
  if (p < MEM_EDEN_BASE) return p; // No need to change!

  // Already redirected
  const redirected = (readWord(p + 2) & 0xff00) == 0xff00;
  if (redirected) return read(p);

  // Needs redirecting.
  //if ((readWord(p+2) & 0x0f00) === 0x700) debugger;
  const [dst, len] = copyObject(p, next); // dst might be next + 4 for long objs

  // DEBUG: Remove me later.
  //if (dst !== next) {
  //  console.log('sanity check', p, decodeHeader(p), next, dst, len);
  //  console.log([p-4, p-2, p, p+2].map(x => hexPad(read(x))).join(' '));
  //  console.log([dst-4, dst-2, dst, dst+2].map(x => hexPad(read(x))).join(' '));
  //  console.log([next, next+2, next+4, next+6].map(x => hexPad(read(x))).join(' '));
  //}

  write(p, dst);
  write(p+2, -1);
  const oldNext = next;
  next = (next + len + 1) & ~1; // Rounding up for alignment.
  // DEBUG: Sanity checks
  if (next - oldNext < len) debugger;
  if (next % 2 === 1) debugger;
  return dst;
}

// start-9    $03 000404     length 3
// start-7    $07 00001c     format 7 = words odd
// start-5    two words
// start-3    two words
// start-1    last word + padding

const HEX_DIGITS = '0123456789abcdef';

function hexPad(x: number): string {
  const hex = [];
  for (let i = 0; i < 8; i++) {
    hex[i] = HEX_DIGITS[(x >>> ((7-i)*4)) & 0xf];
  }
  return hex.join('');
}

function gcSanityCheck(start: ptr) {
  const end = read(MA_NEW_GEN_ALLOC); // The lowest free word of new space.
  while (start < end) {
    // Advance over values with lengths.
    if (readWord(start) === 0xff00) {
      start += 4;
    }

    // start is a destination pointer, the key of gcMap.
    const original = gcMap[start];
    if (!original) throw new Error('unknown dest pointer!?');

    // Now we compare the pointer fields. All the pointer fields of this
    // copied object at start should correspond with the saved values.
    if (((readWord(start+2) >> 8) & 0xf) >= 8) debugger;
    const hdr = decodeHeader(start);
    let ix = 0;
    for (const [len, arr] of [hdr.iv || [0, 0], hdr.pa || [0, 0]]) {
      for (let i = 0; i < len; i++) {
        const dstPtr = read(arr + 2*i);
        const oldCounterpart = original.ptrFields[ix++];

        // Either there was no copying (dstPtr === oldCounterpart) or
        // oldCounterpart should be gcMap[dstPtr].origin.
        if (dstPtr !== oldCounterpart) {
          // They're not equal, so check the copying is sound.
          const mapped = gcMap[dstPtr];
          if (!mapped) {
            throw new Error('Could not find source record for ' + dstPtr);
          }
          if (mapped.origin !== oldCounterpart) {
            throw new Error(`Mismatch! ${oldCounterpart} copied wrong`);
          }
        }

        if (dstPtr >= MEM_EDEN_BASE) {
          debugger;
        }
      }
    }

    const delta = hdr.start < start ?
        // If there's a bonus length, we need to knock 4 off.
        hdr.length - 4 : hdr.length;
    start += (delta + 1) & ~1; // Rounding up.
  }
  console.log('GC sanity check complete');
}

