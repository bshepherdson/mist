import {
  PROCESS_LINK, LINKED_LIST_TAIL, LINKED_LIST_HEAD, MA_NIL,
  ptr, gcTemps, seq, gcRelease, readIV, writeIV, writeIVNew,
} from './memory';

export function addLast(list: ptr, proc: ptr) {
  const [v_list, v_proc, v_tail] = seq(3);
  const ptrs = gcTemps(3);
  ptrs[v_list] = list;
  ptrs[v_proc] = proc;
  if (readIV(list, LINKED_LIST_HEAD) === MA_NIL) {
    writeIV(list, LINKED_LIST_HEAD, ptrs[v_proc]);
    writeIV(list, LINKED_LIST_TAIL, ptrs[v_proc]);
  } else {
    ptrs[v_tail] = readIV(list, LINKED_LIST_TAIL);
    writeIV(ptrs[v_tail], PROCESS_LINK, ptrs[v_proc]);
    writeIV(ptrs[v_list], LINKED_LIST_TAIL, ptrs[v_proc]);
  }
  gcRelease(ptrs);
}

export function removeFirst(list: ptr): ptr {
  const [v_list, v_proc, v_next] = seq(3);
  const ptrs = gcTemps(3);
  ptrs[v_list] = list;
  ptrs[v_proc] = readIV(ptrs[v_list], LINKED_LIST_HEAD);
  if (ptrs[v_proc] === MA_NIL) throw new Error('removeFirst on empty list');

  ptrs[v_next] = readIV(ptrs[v_proc], PROCESS_LINK);
  writeIV(ptrs[v_list], LINKED_LIST_HEAD, ptrs[v_next]);
  if (ptrs[v_next] === MA_NIL) {
    writeIVNew(ptrs[v_list], LINKED_LIST_TAIL, MA_NIL);
  }
  const removed = ptrs[v_proc];
  gcRelease(ptrs);
  return removed;
}

export function removeLink(list: ptr, proc: ptr) {
  const [v_list, v_proc, v_next] = seq(3);
  const ptrs = gcTemps(3);
  ptrs[v_list] = list;
  ptrs[v_proc] = proc;
  ptrs[v_next] = readIV(ptrs[v_list], LINKED_LIST_HEAD);

  if (ptrs[v_next] === ptrs[v_proc]) {
    removeFirst(list);
    gcRelease(ptrs);
    return;
  }

  while (ptrs[v_next] !== MA_NIL) {
    const next = readIV(ptrs[v_next], PROCESS_LINK);
    if (next === ptrs[v_proc]) {
      writeIV(ptrs[v_next], PROCESS_LINK, readIV(next, PROCESS_LINK));

      if (readIV(ptrs[v_list], LINKED_LIST_TAIL) === ptrs[v_proc]) {
        writeIV(ptrs[v_list], LINKED_LIST_TAIL, ptrs[v_next]);
      }
      gcRelease(ptrs);
      return;
    }
    ptrs[v_next] = next;
  }
  throw new Error('not found');
}

