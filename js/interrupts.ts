// "Interrupts" is a bit notional. We treat JS input events and
// requestAnimationFrame callbacks like hardware interrupts.
//
// The global variable InputHand holds a HandMorph. It has three pairs of
// semaphores and values:
// - displayFrameSem, frameCount   (30 bits gives thousands of hours at 60fps)
// - keysSem, lastKey
// - mouseSem, lastMouse
//
// When an event comes in, the event is set into the value, and the semaphore
// is signaled.
import {
  ptr, mkInstance, gcTemps, gcRelease, seq,
  MA_GLOBALS, read, readIV, writeIV, writeIVNew,
  HAND_FRAME_COUNT, HAND_FRAME_SEM,
  HAND_LAST_KEY, HAND_KEYS_SEM,
  HAND_LAST_MOUSE, HAND_MOUSE_SEM,
  toSmallInteger, fromSmallInteger,
} from './memory';

import {
  SYM_INPUT_HAND, SYM_MOUSE_CLICK, SYM_MOUSE_BUTTON_EVENT, SYM_POINT,
} from './corelib';
import {checkHealth, lookup, printDict} from './dict';
import {vmLoop} from './process';
import {signal} from './semaphores';
import {vm} from './vm';

function hand(): ptr {
  return lookup(read(MA_GLOBALS), SYM_INPUT_HAND);
}

export function frameLoop() {
  window.requestAnimationFrame((start: DOMHighResTimeStamp) => {
    vm.frameStart = start;
    //printDict(read(MA_GLOBALS));
    //checkHealth(read(MA_GLOBALS));
    signal(readIV(hand(), HAND_FRAME_SEM));
    frameLoop();
    vmLoop();
  });
}

// Morphic events IVs:
// MorphicEvent: timeStamp source
//   UserInputEvent: type buttons position handler wasHandled wasIgnored
//     MouseEvent:
//       MouseButtonEvent: whichButton nClicks

const [
  MORPHIC_EVENT_TIMESTAMP, MORPHIC_EVENT_SOURCE,
  USER_INPUT_EVENT_TYPE, USER_INPUT_EVENT_BUTTONS,
  USER_INPUT_EVENT_POSITION, USER_INPUT_EVENT_HANDLER,
  USER_INPUT_EVENT_WAS_HANDLED, USER_INPUT_EVENT_WAS_IGNORED,
  MOUSE_BUTTON_EVENT_WHICH_BUTTON, MOUSE_BUTTON_EVENT_N_CLICKS,
] = seq(10);

export function initEvents(canvas: HTMLCanvasElement) {
  canvas.addEventListener('click', (e) => {
    const [v_event, v_pos, v_hand] = seq(3);
    const ptrs = gcTemps(3);
    ptrs[v_hand] = hand();
    ptrs[v_event] = mkInstance(lookup(read(MA_GLOBALS), SYM_MOUSE_BUTTON_EVENT));
    writeIVNew(ptrs[v_event], MORPHIC_EVENT_TIMESTAMP,
        toSmallInteger(Math.floor(e.timeStamp)));
    writeIVNew(ptrs[v_event], USER_INPUT_EVENT_TYPE, SYM_MOUSE_CLICK);

    ptrs[v_pos] = mkInstance(lookup(read(MA_GLOBALS), SYM_POINT));
    writeIVNew(ptrs[v_pos], 0, toSmallInteger(Math.floor(e.offsetX)));
    writeIVNew(ptrs[v_pos], 1, toSmallInteger(Math.floor(e.offsetY)));
    writeIVNew(ptrs[v_event], USER_INPUT_EVENT_POSITION, ptrs[v_pos]);
    // TODO: whichButton, nClicks - for now we assume left single clicks

    writeIV(ptrs[v_hand], HAND_LAST_MOUSE, ptrs[v_event]);
    signal(readIV(ptrs[v_hand], HAND_MOUSE_SEM));
  });
}

