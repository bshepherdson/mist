import {AA_VALUE, MA_NIL, readAt} from './memory.mjs';
import {insert, lookupNode, treeSize} from './tree.mjs';
import './bootstrap.mjs';

// JS-only tests of the tree system. Does randomized sanity-checking against a
// JS object, using numerical keys and values.
function rnd() {
  return Math.floor(Math.random() * 1000000);
}

export function testOnce(n) {
  // Generates n random key-value pairs and inserts them.
  let t = MA_NIL;
  const obj = {};
  const keys = [];
  for (let i = 0; i < n; i++) {
    const k = rnd();
    const v = rnd();
    t = insert(t, k, v);
    obj[k] = v;
    keys.push(k);
  }

  if (treeSize(t) !== Object.keys(obj).length) {
    throw new Error(
        'Size mismatch: ' + treeSize(t) + ' vs. ' + Object.keys(obj).length);
  }

  for (const k of keys) {
    const node = lookupNode(t, k);
    if (node === MA_NIL) {
      throw new Error('Failed to look up ' + k);
    }
    if (readAt(node, AA_VALUE) !== obj[k]) {
      throw new Error('Value mismatch at ' + k + ': ' + readAt(node, AA_VALUE) +
          ' != ' + obj[k]);
    }
  }
}

