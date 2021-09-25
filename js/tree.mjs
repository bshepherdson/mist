import {
  CLS_AA_NODE, AA_KEY, AA_VALUE, AA_LEVEL, AA_LEFT, AA_RIGHT,
  MA_NIL,
  mkInstance, readAt, writeAt,
  adjustSmallInteger, readSmallInteger, wrapSmallInteger,
} from './memory.mjs';

// Implementation of AA trees as ST objects, used as the foundation for the
// IdentityDictionary. It uses the literal pointer values as keys, so it cannot
// be used for any relocatable objects. Symbols only for now; SmallIntegers
// later if they're rewritten in a tagged pointer way.

// Nodes are instances of the CLS_AA_NODE class, which has
// AA_KEY, AA_VALUE, AA_LEVEL, AA_LEFT, and AA_RIGHT instance variables.
//
// On GC: Note that the GC already handles the raw pointers properly. They're
// either pointers to old, immovable things like Symbols, or tagged values like
// SmallIntegers (eventually). The GC can copy around the nodes and their
// values; the keys are not copied and are handled correctly.

// Skew is a right rotation to turn a left "horizontal" link into a right one
// instead.
//        v           v
//   L <- T           L -> T
//  / \    \    =>   /    / \
//  A B    R         A   B   R
function skew(t) {
  if (t === MA_NIL) return t;
  if (readAt(t, AA_LEFT) === MA_NIL) return t;

  const myLevel = readSmallInteger(t + AA_LEVEL);
  const left = readAt(t, AA_LEFT);                 // L = left(t)
  const leftLevel = readSmallInteger(left + AA_LEVEL);
  if (myLevel !== leftLevel) return t;

  // Skewing is really necessary.
  // Swap the pointers of this horizontal left link.
  writeAt(t, AA_LEFT, readAt(left, AA_RIGHT)); // left(t) := right(L)
  writeAt(left, AA_RIGHT, t);                  // right(L) := t
  return left;
}

// Split is a left rotation and level increase to replace a subtree containing
// two or more consecutive right horizontal links with one containing two fewer.
//   v                        v
//   T -> R -> X   =>         R
//  /    /                   / \
// A    B                   T   X
//                         / \
//                        A   B
function split(t) {
  if (t === MA_NIL) return t;

  const right = readAt(t, AA_RIGHT);
  if (right === MA_NIL) return t;
  const grandRight = readAt(right, AA_RIGHT);
  if (grandRight === MA_NIL) return t;

  const myLevel = readSmallInteger(t + AA_LEVEL);
  const grandLevel = readSmallInteger(grandRight + AA_LEVEL);
  if (myLevel !== grandLevel) return t;

  // Split is necessary.
  // right(t) := left(R)
  writeAt(t, AA_RIGHT, readAt(right, AA_LEFT));
  // left(R) := t
  writeAt(right, AA_LEFT, t);
  // level(R) += 1
  adjustSmallInteger(right + AA_LEVEL, 1);

  return right;
}


// Insertion is a typical binary tree search and insertion. Then on the way back
// up the call stack, we skew and then split. These usually do nothing, but will
// check the invariants and rotate as necessary.
// NB The adjustment of level(T).
export function insert(t, k, v) {
  if (t === MA_NIL) {
    const node = mkInstance(read(classTable(CLS_AA_NODE)));
    writeAt(node, AA_LEVEL, wrapSmallInteger(1));
    writeAt(node, AA_KEY, k);
    writeAt(node, AA_VALUE, v);
    // Left and right are already initialized to nil.
    return node;
  }

  // We're comparing the *raw pointers*, not the objects pointed at!
  const myKey = readAt(t, AA_KEY);
  if (k < myKey) {
    const left = readAt(t, AA_LEFT);
    writeAt(t, AA_LEFT, insert(left, k, v));
  } else if (k > myKey) {
    const right = readAt(t, AA_RIGHT);
    writeAt(t, AA_RIGHT, insert(right, k, v));
  } else {
    // Equals - update the value pointed to.
    writeAt(t, AA_VALUE, v);
  }

  // Now skew, then split, then return.
  return split(skew(t));
}



// Decrease level is fairly complex so it gets its own function.
function decreaseLevel(t) {
  const myLevel = readSmallInteger(t + AA_LEVEL);

  const left = readAt(t, AA_LEFT);
  const leftLevel = readSmallInteger(left + AA_LEVEL);
  const right = readAt(t, AA_RIGHT);
  const rightLevel = readSmallInteger(right + AA_LEVEL);
  const shouldBe = Math.min(leftLevel, rightLevel) + 1;

  if (shouldBe < myLevel) {
    writeAt(t, AA_LEVEL, wrapSmallInteger(shouldBe));
    if (shouldBe < rightLevel) {
      writeAt(right, AA_LEVEL, wrapSmallInteger(shouldBe));
    }
  }
  return t;
}

// Right once, then all the way left. Returns the node, not the key.
function successor(t) {
  let n = readAt(t, AA_RIGHT);
  while (true) {
    const next = readAt(n, AA_LEFT);
    if (next === MA_NIL) return n;
    n = next;
  }
}

// Left once, then all the way right. Returns the node, not the key.
function predecessor(t) {
  let n = readAt(t, AA_LEFT);
  while (true) {
    const next = readAt(n, AA_RIGHT);
    if (next === MA_NIL) return n;
    n = next;
  }
}

// Delete works by swapping an internal node with its closest successor, then
// deleting that leaf node. The closest successor is going right, then
// recursively left.
export function remove(t, k) {
  if (t === MA_NIL) return t;

  const key = readAt(t, AA_KEY); // Again, comparing raw pointers.
  if (k < key) {
    const left = readAt(t, AA_LEFT);
    writeAt(t, AA_LEFT, remove(left, k));
  } else if (k > key) {
    const right = readAt(t, AA_RIGHT);
    writeAt(t, AA_RIGHT, remove(right, k));
  } else {
    // It's me! If I'm a leaf, easy, just return nil.
    const left = readAt(t, AA_LEFT);
    const right = readAt(t, AA_RIGHT);
    if (left === MA_NIL && right === MA_NIL) {
      return MA_NIL;
    }

    if (left === MA_NIL) {
      const replacement = successor(t);
      const repKey = readAt(replacement, AA_KEY);
      writeAt(t, AA_RIGHT, remove(right, repKey));
      writeAt(t, AA_KEY, repKey);
      writeAt(t, AA_VALUE, readAt(replacement, AA_VALUE));
    } else {
      const replacement = predecessor(t);
      const repKey = readAt(replacement, AA_KEY);
      writeAt(t, AA_LEFT, remove(left, repKey));
      writeAt(t, AA_KEY, repKey);
      writeAt(t, AA_VALUE, readAt(replacement, AA_VALUE));
    }
  }

  // Rebalance the tree. Decrease the level of all nodes in this level if
  // necessary, and then skew and split all nodes in the new level.
  let ret = skew(decreaseLevel(t));
  writeAt(ret, AA_RIGHT, skew(readAt(ret, AA_RIGHT))); // Bonus skew.

  if (readAt(ret, AA_RIGHT) !== MA_NIL) {
    const right = readAt(ret, AA_RIGHT);
    writeAt(right, AA_RIGHT, skew(readAt(right, AA_RIGHT))); // Grand-right skew
  }
  ret = split(ret);
  writeAt(ret, AA_RIGHT, split(readAt(ret, AA_RIGHT)));
  return ret;
}

// Looks up a given key in this search tree! Returns the pointer to the node!
// The actual value stored there is at AA_VALUE. Returns MA_NIL if not found.
export function lookupNode(t, k) {
  if (t === MA_NIL) return MA_NIL;

  const key = readAt(t, AA_KEY);
  if (k === key) return t;

  const side = k < key ? AA_LEFT : AA_RIGHT;
  return lookupNode(readAt(t, side), k);
}

// Returns the number of nodes in the tree as a raw number.
export function treeSize(t) {
  if (t === MA_NIL) return 0;
  return 1 + treeSize(readAt(t, AA_LEFT)) + treeSize(readAt(t, AA_RIGHT));
}

