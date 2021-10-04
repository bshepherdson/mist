// This is a real, non-hobo implementation of IdentityDictionary as a hash map.
// These are not (currently) called by HashedCollection (TODO but that might be
// a useful optimization in the future?) but they happen to be
// binary-compatible.
//
// First inst var is array, second is tally.
// The array stores associations, indexed by identity hash of the key. It uses
// linear probing.
import {
  ptr,
  ASSOC_KEY, ASSOC_VALUE,
  CLS_ASSOCIATION, CLS_ARRAY, CLS_IDENTITY_DICTIONARY,
  DICT_ARRAY, DICT_TALLY,
  MA_NIL,
  asJSString, arraySize, classTable, identityHash, mkInstance,
  fromSmallInteger, toSmallInteger,
  read, readArray, readIV,
  writeArray, writeIV, writeIVNew,
} from './memory';

export function mkDict(size = 16): ptr {
  const dict = mkInstance(read(classTable(CLS_IDENTITY_DICTIONARY)));
  const arr = mkInstance(read(classTable(CLS_ARRAY)), size);
  writeIV(dict, DICT_ARRAY, arr);
  writeIVNew(dict, DICT_TALLY, toSmallInteger(0));
  return dict;
}

// IdentityDictionary, so we're comparing the pointers of keys and values.
export function lookup(dict: ptr, key: ptr): ptr {
  const arr = readIV(dict, DICT_ARRAY);
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));
  const hash = identityHash(key);
  const capacity = arraySize(arr);
  const mask = capacity - 1;
  let index = hash & mask;
  while (true) {
    const asc = readArray(arr, index);
    if (asc === MA_NIL) return MA_NIL; // Not found.
    if (readIV(asc, ASSOC_KEY) === key) return readIV(asc, ASSOC_VALUE);
    index = (index + 1) & mask;
  }
}

// Doubles the size of the array, rehashing the contents.
function grow(dict: ptr): void {
  const oldArray = readIV(dict, DICT_ARRAY);
  const oldSize = arraySize(oldArray);
  const newArray = mkInstance(read(classTable(CLS_ARRAY)), 2 * oldSize);
  writeIV(dict, DICT_ARRAY, newArray);
  writeIV(dict, DICT_TALLY, toSmallInteger(0));

  for (let i = 0; i < oldSize; i++) {
    const asc = readArray(oldArray, i);
    if (asc !== MA_NIL) {
      insertAssoc(dict, asc);
    }
  }
}

export function insert(dict: ptr, key: ptr, value: ptr): void {
  const asc = mkInstance(read(classTable(CLS_ASSOCIATION)));
  writeIV(asc, ASSOC_KEY, key);
  writeIV(asc, ASSOC_VALUE, value);
  insertAssoc(dict, asc);
}

function insertAssoc(dict: ptr, asc: ptr): void {
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));
  const arr = readIV(dict, DICT_ARRAY);
  const capacity = arraySize(arr);
  if (tally / capacity > 0.5) {
    grow(dict);
    insertAssoc(dict, asc);
    return;
  }

  const mask = capacity - 1;
  const key = readIV(asc, ASSOC_KEY);
  let index = identityHash(key) & mask;
  while (true) {
    const found = readArray(arr, index);
    if (found === MA_NIL || readIV(found, ASSOC_KEY) === key) {
      writeArray(arr, index, asc);
      if (found === MA_NIL) {
        writeIV(dict, DICT_TALLY, toSmallInteger(tally + 1));
      }
      break;
    }
    index = (index + 1) & mask;
  }
}

export function printDict(dict: ptr): void {
  const arr = readIV(dict, DICT_ARRAY);
  const size = arraySize(arr);

  for (let i = 0; i < size; i++) {
    const asc = readArray(arr, i);
    if (asc !== MA_NIL) {
      console.log(asJSString(readIV(asc, ASSOC_KEY)), readIV(asc, ASSOC_VALUE));
    }
  }
}

// TODO Might as well make these dictionaries compatible with upstream
// dictionaries, it's not that hard to rewrite the hashtable logic.

