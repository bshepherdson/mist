// A hobo implementation of dictionaries on top of raw arrays. No hashing or
// anything clever, just [k1, v1, k2, v2, ...] in arbitrary order.
// This is used for method dictionaries until we have real dictionaries defined.
import {
  ptr,
  CLS_ARRAY, CLS_IDENTITY_DICTIONARY,
  DICT_ARRAY, DICT_TALLY,
  MA_NIL,
  arraySize, classTable, mkInstance, wordArraySize,
  fromSmallInteger, toSmallInteger,
  read, readArray, readIV, readWordArray,
  writeArray, writeArrayNew, writeIV, writeIVNew,
} from './memory';

export function mkDict(size = 16): ptr {
  const dict = mkInstance(read(classTable(CLS_IDENTITY_DICTIONARY)));
  const arr = mkInstance(read(classTable(CLS_ARRAY)), 2 * size);
  writeIV(dict, DICT_ARRAY, arr);
  writeIVNew(dict, DICT_TALLY, toSmallInteger(0));
  return dict;
}

// IdentityDictionary, so we're comparing the pointers of keys and values.
export function lookup(dict: ptr, key: ptr): ptr {
  const arr = readIV(dict, DICT_ARRAY);
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));
  for (let i = 0; i < tally; i++) {
    const k = readArray(arr, 2 * i);
    if (k === key) {
      return readArray(arr, 2 * i + 1);
    }
  }
  return MA_NIL;
}

// Doubles the size of the array, copying the contents.
function grow(dict: ptr) {
  const src = readIV(dict, DICT_ARRAY);
  const oldSize = arraySize(src);
  const dst = mkInstance(read(classTable(CLS_ARRAY)), 2 * oldSize);
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));
  for (let i = 0; i < tally * 2; i++) {
    const value = readArray(src, i);
    writeArrayNew(dst, i, value);
  }
  writeIV(dict, DICT_ARRAY, dst);
}

export function insert(dict: ptr, key: ptr, value: ptr): undefined {
  const arr = readIV(dict, DICT_ARRAY);
  const size = arraySize(arr);
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));
  if (tally * 2 >= size) {
    grow(dict);
    // Just recurse to update the fields.
    return insert(dict, key, value);
  }

  // There's still space, so just add it.
  writeArray(arr, tally * 2, key);
  writeArray(arr, tally * 2 + 1, value);
  writeIV(dict, DICT_TALLY, toSmallInteger(tally + 1));
}

export function printDict(dict: ptr) {
  const arr = readIV(dict, DICT_ARRAY);
  const size = arraySize(arr);
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));

  for (let i = 0; i < tally; i++) {
    const k = readArray(arr, 2*i);
    const v = readArray(arr, 2*i+1);
    let s = '';
    for (let j = 0; j < wordArraySize(k); j++) {
      s += String.fromCharCode(readWordArray(k, j));
    }
    console.log(s, v);
  }
}

// TODO Might as well make these dictionaries compatible with upstream
// dictionaries, it's not that hard to rewrite the hashtable logic.

