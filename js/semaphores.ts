import {
  ptr, gcTemps, gcRelease, seq,
  MA_NIL, LINKED_LIST_HEAD, PROCESS_MY_LIST, PROCESS_LINK,
  SEMAPHORE_EXCESS_SIGNALS,
  readIV, writeIVNew, fromSmallInteger, toSmallInteger,
} from './memory';
import {removeFirst} from './lists';
import {resume} from './process';

export function signal(semaphore: ptr) {
  const [v_sem, v_proc] = seq(2);
  const ptrs = gcTemps(2);
  ptrs[v_sem] = semaphore;

  if (readIV(ptrs[v_sem], LINKED_LIST_HEAD) !== MA_NIL) {
    // There's processes waiting, so resume the first of them.
    ptrs[v_proc] = removeFirst(ptrs[v_sem]);
    writeIVNew(ptrs[v_proc], PROCESS_MY_LIST, MA_NIL);
    writeIVNew(ptrs[v_proc], PROCESS_LINK, MA_NIL);
    resume(ptrs[v_proc]);
  } else {
    // Nobody waiting, so make a note of the excess signal.
    const signals =
        fromSmallInteger(readIV(ptrs[v_sem], SEMAPHORE_EXCESS_SIGNALS));
    writeIVNew(ptrs[v_sem], SEMAPHORE_EXCESS_SIGNALS,
        toSmallInteger(signals + 1));
  }

  gcRelease(ptrs);
}
