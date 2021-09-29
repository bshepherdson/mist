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

const GC_FIELD_0 = 0; // Initial value of the GC field in the object header.

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
export const MA_CLASS_DICT = 0x100c;
const MA_NEXT_CLASS_INDEX = 0x100e;

export const MASK_CLASS_INDEX = 0x3fffff;

// Some canned ST objects that exist at known places in memory.
export const MA_NIL   = 0x2000; // Size 4, zero-size object
export const MA_TRUE  = 0x2004; // Size 4, zero-size object
export const MA_FALSE = 0x2008; // Size 4, zero-size object

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


const CLASSTABLE_BASE = 0x3000;
const CLASSTABLE_SIZE = 0x1000; // 4096 words = 2048 classes
const CLASSTABLE_TOP = CLASSTABLE_BASE + CLASSTABLE_SIZE;

// Some canned class table entries, since we need to refer statically to a bunch
// of them.

let nextClass_ = 0;

function nextClass() {
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

export const CLS_COLLECTION = nextClass();
export const CLS_SEQUENCEABLE_COLLECTION = nextClass();
export const CLS_ARRAYED_COLLECTION = nextClass();
export const CLS_ARRAY = nextClass();
export const CLS_STRING = nextClass();
export const CLS_SYMBOL = nextClass();
export const CLS_HASHED_COLLECTION = nextClass();
export const CLS_DICTIONARY = nextClass();
export const CLS_IDENTITY_DICTIONARY = nextClass();

export const CLS_CONTEXT = nextClass();
export const CLS_COMPILED_METHOD = nextClass();
export const CLS_BLOCK_CLOSURE = nextClass();

export const CLS_BOOLEAN = nextClass();
export const CLS_TRUE = nextClass();
export const CLS_FALSE = nextClass();

export const CLS_MAGNITUDE = nextClass();
export const CLS_CHARACTER = nextClass();
export const CLS_NUMBER = nextClass();
export const CLS_INTEGER = nextClass();
export const CLS_SMALL_INTEGER = nextClass();
export const CLS_ASSOCIATION = nextClass();

export const CLS_WORD_ARRAY = nextClass();
export const CLS_PROCESS = nextClass();
export const CLS_PROCESS_TABLE = nextClass();
// BE CAREFUL REORDERING - see above note.


// Returns an array of ordered indexes.
function seq(n) {
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

// Contexts are stored as 24-pointer objects.
// They are (currently?) the only FORMAT_VARIABLE_IV object.
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

export const [AA_KEY, AA_VALUE, AA_LEVEL, AA_LEFT, AA_RIGHT] = seq(5);
export const [DICT_ARRAY, DICT_TALLY] = seq(2);
export const ASCII_TABLE = [];


// Functions for accessing the memory.
// Longs are stored big-endian, ie. the high word is at the lower address.
export function readWord(addr) {
  return mem[addr];
}

export function read(addr) {
  return (mem[addr] << 16) | mem[addr+1];
}

export function writeWord(addr, value) {
  if (67106912 <= addr && addr <= 67106920) debugger;
  return mem[addr] = value & 0xffff;
}

export function write(addr, value) {
  writeWord(addr, value >> 16);
  writeWord(addr + 1, value);
}




// Object header format
// ssssssss __hhhhhh hhhhhhhh hhhhhhhh
// ggggffff __cccccc cccccccc cccccccc


// Functions for deriving properties of objects, given a pointer to the header.
// Turns a class table index into a pointer into the *class table*. It must be
// read() to get the real pointer to the class.
export function classTable(index) {
  return CLASSTABLE_BASE + 2 * index;
}

// Returns the real pointer to the class of this object, indirecting through the
// class table.
export function classOf(p) {
  const classIndex = isSmallInteger(p) ? // Special case for inlined integers.
      CLS_SMALL_INTEGER :
      read(p+2) & MASK_CLASS_INDEX;
  return read(classTable(classIndex));
}

export function hasClass(p, classIndex) {
  return isSmallInteger(p) ?
      classIndex === CLS_SMALL_INTEGER :
      (read(p+2) & MASK_CLASS_INDEX) === classIndex;
}

// Returns the format tag
function format(p) {
  return (readWord(p+2) >> 8) & 0xf;
}

export const FORMAT_ZERO = 0;
export const FORMAT_FIXED_IV = 1;
export const FORMAT_VARIABLE = 2;
export const FORMAT_VARIABLE_IV = 3;
export const FORMAT_WORDS_EVEN = 6;
export const FORMAT_WORDS_ODD = 7;

// "Decodes" the header for a pointer-type object: that is, one with a header
// that isn't holding words and isn't a special type like 8+.
// The returned value gives:
// {
//   ivc: instance variables count
//   ivp: pointer to first instance variable.
//   pac: pointer array element count (in longwords/pointers)
//   pap: pointer array first element pointer
//   wac: word array element count (in words)
//   wap: word array first element pointer
// }
// but these values will be missing if they're not relevant.
function decodeHeader(p) {
  let size = readWord(p) >> 8;
  const fmt = format(p);
  switch (fmt) {
    case FORMAT_ZERO:
      return {};
    case FORMAT_VARIABLE_IV:
      const varSize = read(p - 2);
      return {
        ivc: size,
        ivp: p + 4,
        pac: varSize,
        pap: p + 4 + 2 * size,
      };
    case FORMAT_VARIABLE:
      return {
        pac: size === 255 ? read(p - 2) : size,
        pap: p + 4,
      };
    case FORMAT_FIXED_IV:
      return {
        ivc: size === 255 ? read(p + 4) : size,
        ivp: p + 4,
      };
    case FORMAT_WORDS_ODD:
    case FORMAT_WORDS_EVEN:
      const ret = {wac: 2*size, wap: p + 4};
      if (size === 255) {
        ret.wac = read(p + 4);
      }
      if (fmt === FORMAT_WORDS_ODD) {
        ret.wac--; // Odd means the last slot isn't filled.
      }
      return ret;
    default:
      throw new Error('Don\'t know how to decode header format ' + fmt);
  }
  throw new Error('cant happen');
}


// Some helpers for reading from objects.
// These have sanity checks for eg. trying to read the IVs of a variable type.
// 0-based indexing.
export function readIV(p, index) {
  const hdr = decodeHeader(p);
  if (!hdr.ivc) throw new Error('Cannot read IVs of non-IV object');
  if (hdr.ivc <= index) throw new Error('Reading off the end of object');

  return read(hdr.ivp + 2 * index);
}

export function readArray(p, index) {
  const hdr = decodeHeader(p);
  if (!hdr.pac) {
    throw new Error('Cannot read variable-length part of fixed object');
  }
  if (hdr.pac <= index) throw new Error('Reading off the end of array');

  return read(hdr.pap + 2 * index);
}

export function readWordArray(p, index) {
  const hdr = decodeHeader(p);
  if (!hdr.wac) throw new Error('Cannot read word array of non-words object');
  if (hdr.wac <= index) throw new Error('Reading off the end of word array');

  return readWord(hdr.wap + index);
}


// The writes come in two forms: write*New when we can guarantee the write is
// happening into a new space, and the other when it might be into an old space
// where a check needs to be performed.
//
// These return the address actually written, mostly to make checkOldToNew easy.
export function writeIVNew(p, index, value) {
  const hdr = decodeHeader(p);
  if (!hdr.ivc) throw new Error('Cannot write IVs of non-IV object');
  const addr = hdr.ivp + 2 * index;
  write(addr, value);
  return addr;
}

export function writeIV(p, index, value) {
  const addr = writeIVNew(p, index, value);
  checkOldToNew(addr, value);
  return addr;
}

export function writeArrayNew(p, index, value) {
  const hdr = decodeHeader(p);
  if (!hdr.pac) throw new Error('Cannot write array of non-array');
  const addr = hdr.pap + 2 * index;
  write(addr, value);
  return addr;
}

export function writeArray(p, index, value) {
  const addr = writeArrayNew(p, index, value);
  checkOldToNew(addr, value);
  return addr;
}

// No need for the "new" variant of a word array; they're not pointers.
export function writeWordArray(p, index, value) {
  const hdr = decodeHeader(p);
  if (!hdr.wac) throw new Error('Cannot write word array of non-words object');
  const addr = hdr.wap + index;
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
export function fromSmallInteger(p) {
  if (p >= 0) throw new Error('pointers found in readSmallInteger');
  // Shift left, then *arithmetic* shift right.
  return (p << 1) >> 1;
}

// Given a JS number, encodes it as a SmallInteger, a pseudo-pointer with the
// top bit always set. The top bit is lost.
export function toSmallInteger(n) {
  return n | 0x80000000;
}

export function isSmallInteger(p) {
  return p < 0;
}


// Working with the stack on a MethodContext. The stack pointer is in an IV,
// giving a 0-based index into the variable portion.
export function push(ctx, value) {
  const sp = fromSmallInteger(readIV(ctx, CTX_STACK_INDEX));
  if (sp >= STACK_MAX_SIZE) throw new Error('VM stack overflow');
  writeArray(ctx, sp, value);
  // Always a number, safe to skip the checks.
  writeIVNew(ctx, CTX_STACK_INDEX, toSmallInteger(sp + 1));
}

export function pop(ctx) {
  const sp = fromSmallInteger(readIV(ctx, CTX_STACK_INDEX));
  if (sp <= 0) throw new Error('VM stack underflow');
  const value = readArray(ctx, sp);
  // Always a number, safe to skip the checks.
  writeIVNew(ctx, CTX_STACK_INDEX, toSmallInteger(sp - 1));
  return value;
}

export function peek(ctx) {
  const sp = fromSmallInteger(readIV(ctx, CTX_STACK_INDEX));
  return readArray(ctx, sp);
}



// Allocation! This is vital but not hard: given a necessary size, allocate
// space for it in the Eden and return a pointer to it.
// This is the raw allocation function - it allocates in *words*. It rounds up
// to keep the Eden 32-bit aligned.
export function alloc(size) {
  const p = (vm.allocationPointer - size) & ~1;
  if (p < MEM_EDEN_BASE) {
    throw new Error('Minor GC needed but not implemented!');
  }
  vm.allocationPointer = p;
  return p;
}

// Allocates size words in tenured space, returning the pointer to it.
// This is used for tenuring, but also for things like Symbols that are
// permanently cached.
// Keeps the tenure pointer 32-bit aligned.
export function tenure(size) {
  const p = read(MA_TENURE_ALLOC); // Raw number, not a Smalltalk value.
  write(MA_TENURE_ALLOC, (p + size + 1) & ~1); // Keep it aligned.
  return p;
}

// Computes the size required for a regular object with the specified instVars
// and array length. Both can be 0, in which case a FORMAT_ZERO object is
// assumed. This accounts for the size word and so on.
// Generally you can call allocObject() to do this all in one, but this is
// useful if allocating to the tenured space or a special area.
// Returns the length in *words*, suitable for passing to tenure() or alloc().
export function sizeRequired(instVars, arrayLength) {
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
function nextIdentityHash() {
  const hash = vm.nextIdentityHash;
  vm.nextIdentityHash = (vm.nextIdentityHash + 1) & MASK_CLASS_INDEX;
  return hash;
}

// Reads the identityHash out of an object header.
export function identityHash(p) {
  return read(p) & MASK_CLASS_INDEX;
}

// Inits any pointer-valued object. instVars, arrayLength or both can be 0.
// p is assumed to already point to a large enough memory area!
// You probably want to call allocObject() instead if allocating to Eden.
// You can compute the necessary size for same with sizeRequired().
// Returns updated p! This should be used, as there might be an extra size at
// the input p, which is no longer the object's pointer.
export function initObject(p, clsHash, instVars, arrayLength) {
  const hash = nextIdentityHash();
  let fmt, sizeField;
  let extraLength = null;

  if (instVars > 0 && arrayLength > 0) {
    fmt = FORMAT_VARIABLE_IV;
    sizeField = instVars;
    extraLength = arrayLength;
  } else if (instVars === 0 && arrayLength === 0) {
    fmt = FORMAT_ZERO;
    sizeField = 0;
  } else {
    fmt = instVars > 0 ? FORMAT_FIXED_IV : FORMAT_VARIABLE;
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

function allocWordArray(clsHash, arrayLength, allocator) {
  // The size field actually is in words, so it only overflows for 255 * 2 = 510
  const size = 4 + arrayLength + (arrayLength >= 510 ? 4 : 0);
  let p = allocator(size);

  // The size field is in words, and the last bit is captured by the even/odd
  // formats.
  const longSize = (arrayLength + 1) >> 1;
  const fmt = (arrayLength & 1) === 1 ? FORMAT_WORDS_ODD : FORMAT_WORDS_EVEN;

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
export function allocObject(clsHash, instVars, arrayLength, allocator) {
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
export function mkInstance(cls, opt_arrayLength, opt_allocator) {
  // Read the format number out of the class. This is defined on Behavior, and
  // is encoded as ____ffff sssssss ssssssss ssssssss, a 24-bit size field.
  const allocator = opt_allocator || alloc;
  const fmtRaw = fromSmallInteger(readIV(cls, BEHAVIOR_FORMAT));
  const fmt = (fmtRaw >> 24) & 0xf;
  const instVars = fmtRaw & 0xffffff;

  const isFixed = fmt === FORMAT_ZERO || fmt === FORMAT_FIXED_IV;
  if (opt_arrayLength && opt_arrayLength > 0 && isFixed) {
    throw new Error('Illegal: non-variable class with nonzero array length');
  }
  // Creating a 0-length Array etc. is technically legal, so no error for that.

  // Allocate and initialize such an object.
  const clsHash = identityHash(cls);
  return (fmt === FORMAT_WORDS_ODD || fmt === FORMAT_WORDS_EVEN) ?
      allocWordArray(clsHash, opt_arrayLength || 0, allocator) :
      allocObject(clsHash, instVars, opt_arrayLength || 0, allocator);
}

// Given the pointer to a Class, constructs a new instance of it.
// This is essentially basicNew.
// Only works for regular objects! Special types should be calling other
// primitives.
export function basicNew(cls) {
  return mkInstance(cls);
}


// When we're storing a pointer at p that points to target, we check to see if
// p is in the old space (< MEM_EDEN_BASE) and target in new (>= MEM_EDEN_BASE).
// If it is, we create a new Association in Eden that points to *p*. Then later
// the GC can check if p still contains a newspace pointer, recursively copy it,
// and update p.
export function checkOldToNew(p, target) {
  if (isSmallInteger(target)) return; // No pointer to track.

  const isOld = p < MEM_EDEN_BASE;
  const isNew = target >= MEM_EDEN_BASE;
  if (isOld && isNew) {
    // We store these records as Associations, which are 2-element arrays.
    const link = mkInstance(read(classTable(CLS_ASSOCIATION)), 2);
    const head = read(MA_ESCAPED_HEAD);
    writeArrayNew(link, 0, p);
    writeArrayNew(link, 1, head);
    write(MA_ESCAPED_HEAD, link);
  }
}

// Given a pointer to a variable-sized object, return the length of its variable
// portion in the relevant unit - pointers or words.
export function arraySize(p) {
  return decodeHeader(p).pac;
}

export function wordArraySize(p) {
  return decodeHeader(p).wac;
}


// Strings and symbols
// These are stored as raw arrays with custom types.
//
// Copies a JS string s into a (sufficiently large!) blank space at p.
// p points to the header, not the first data word.
function copyString(p, s) {
  for (let i = 0; i < s.length; i++) {
    writeWordArray(p, i, s.charCodeAt(i));
  }
}

// Allocate and return the pointer to a new String containing the same
// characters as the JS string provided.
export function wrapString(s) {
  const arr = mkInstance(read(classTable(CLS_STRING)), s.length);
  copyString(arr, s);
  return arr;
}

// Given a pointer to a String or Symbol, read it into a JS string.
export function asJSString(p) {
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
const symbolCache = {};
export function wrapSymbol(s) {
  if (symbolCache[s]) return symbolCache[s];

  // Allocate our symbol in the tenured space.
  const arr = mkInstance(read(classTable(CLS_SYMBOL)), s.length, tenure);
  copyString(arr, s);

  symbolCache[s] = arr;
  return arr;
}


export function addClassToTable(cls, opt_classHash) {
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
export function behaviorToInstVars(cls) {
  const fmtRaw = fromSmallInteger(readIV(cls, BEHAVIOR_FORMAT));
  const fmt = (fmtRaw >> 24) & 0xf;
  return fmtRaw & 0xffffff;
}

// Encodes a Behavior's format field for a FORMAT_FIXED_IV
export function fixedInstVarsFormat(instVars) {
  return instVars === 0 ?
      (FORMAT_ZERO << 24) :
      (FORMAT_FIXED_IV << 24) | instVars;
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

