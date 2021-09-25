// A hobo implementation of dictionaries on top of raw arrays. No hashing or
// anything clever, just [k1, v1, k2, v2, ...] in arbitrary order.
// This is used for method dictionaries until we have real dictionaries defined.
import {CLS_ARRAY, arraySize, classTable, read, readArray, readIV, writeArray, writeIV} from './memory.mjs';

export function mkDict(opt_size) {
  const size = opt_size || 16;
  const dict = mkInstance(read(classTable(CLS_IDENTITY_DICTIONARY)));
  const arr = mkInstance(read(classTable(CLS_ARRAY)), 2 * size);
  writeIV(dict, DICT_ARRAY, arr);
  writeIVNew(dict, DICT_TALLY, toSmallInteger(0));
  return dict;
}

// IdentityDictionary, so we're comparing the pointers of keys and values.
export function lookup(dict, key) {
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
function grow(dict) {
  const src = readIV(dict, DICT_ARRAY);
  const oldSize = arraySize(src);
  const dst = mkDict(oldSize * 2);
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));
  for (let i = 0; i < tally * 2; i++) {
    writeArrayNew(dst, i, readArray(src, i));
  }
  writeIV(dict, DICT_ARRAY, dst);
}

export function insert(dict, key, value) {
  const arr = readIV(dict, DICT_ARRAY);
  const size = arraySize(arr);
  const tally = fromSmallInteger(readIV(dict, DICT_TALLY));
  if (tally * 2 >= size) {
    grow();
    // Just recurse to update the fields.
    return insert(dict, key, value);
  }

  // There's still space, so just add it.
  writeArray(arr, tally * 2, key);
  writeArray(arr, tally * 2 + 1, value);
  writeIV(dict, DICT_TALLY, toSmallInteger(tally + 1));
}
