// A later stage of bootstrapping, which runs after the initial bootstrap, then
// the AA tree functionality.

import {defClass, impoverishedClasses, lateBinding} from './bootstrap';
import {insert, mkDict} from './dict';
import {
  ptr, Format,
  CLS_BOOLEAN, CLS_TRUE, CLS_FALSE,
  CLS_CONTEXT, CLS_BLOCK_CLOSURE, CLS_MAGNITUDE,
  CLS_ARRAY, CLS_CHARACTER, CLS_PROCESS, CLS_PROCESSOR_SCHEDULER, CLS_OBJECT,
  CLS_LINKED_LIST, CLS_LINK, CLS_SEMAPHORE,
  CTX_PC, CTX_STACK_INDEX, CTX_METHOD, CTX_LOCALS, CTX_SENDER,
  MA_TRUE, MA_FALSE, MA_GLOBALS, MA_NIL,
  PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES,
  BEHAVIOR_FORMAT, BEHAVIOR_METHODS, CLASS_NAME,
  CLASS_DESCRIPTION_INSTANCE_VARIABLES, CLASS_POOL, IV_BLOCK,
  basicNew, classOf, classTable, mkInstance,
  fromSmallInteger, toSmallInteger, wrapSymbol, wrapString,
  read, readIV, write, writeIV, writeIVNew, writeArrayNew,
  gcTemps, gcRelease, seq,
} from './memory';
import {vm} from './vm';

lateBinding.dictFactory = () => mkDict();
lateBinding.symbolize = wrapSymbol;
lateBinding.varArray = (name: ptr|string, names: string[]): ptr => {
  const [v_array] = seq(1);
  const ptrs = gcTemps(1);
  ptrs[v_array] = mkInstance(read(classTable(CLS_ARRAY)), names.length);
  for (let i = 0; i < names.length; i++) {
    writeArrayNew(ptrs[v_array], i, wrapString(names[i]));
  }
  const array = ptrs[v_array];
  gcRelease(ptrs);
  return array;
};
lateBinding.register = (cls, name) => {};

// And the class dictionary appender.

lateBinding.addToClassDict = (sym, cls) => {
  const dict = read(MA_GLOBALS);
  insert(dict, sym, cls);
};

write(MA_GLOBALS, mkDict(256));

// And update the nil method dictioaries we just created for those classes (and
// their metaclasses).

for (const name of Object.keys(impoverishedClasses)) {
  const {cls, vars} = impoverishedClasses[name];
  writeIV(cls, BEHAVIOR_METHODS, mkDict());
  const metaclass = classOf(cls);
  writeIV(metaclass, BEHAVIOR_METHODS, lateBinding.dictFactory());
  const symbol = wrapSymbol(name);
  writeIV(cls, CLASS_NAME, symbol);
  lateBinding.addToClassDict(symbol, cls);
  writeIV(cls, CLASS_DESCRIPTION_INSTANCE_VARIABLES,
      lateBinding.varArray(name, vars || []));
}

const object = read(classTable(CLS_OBJECT));
defClass(CLS_BLOCK_CLOSURE, 'BlockClosure', object,
    ['ctx', 'pc0', 'argc', 'argv', 'handlerActive']);

const ctx = defClass(CLS_CONTEXT, 'MethodContext', object,
    ['method', 'locals', 'pc', 'sender', 'sp']);
const fmt = fromSmallInteger(readIV(ctx, BEHAVIOR_FORMAT));
writeIVNew(ctx, BEHAVIOR_FORMAT,
    toSmallInteger((fmt & 0xffffff) | (Format.VARIABLE_IV << 24)));


const bool = defClass(CLS_BOOLEAN, 'Boolean', object, []);
const true_ = defClass(CLS_TRUE, 'True', bool, []);
const false_ = defClass(CLS_FALSE, 'False', bool, []);

// Create the built-in instances.
mkInstance(true_, undefined, (_) => MA_TRUE);
mkInstance(false_, undefined, (_) => MA_FALSE);

const chr = defClass(CLS_CHARACTER, 'Character',
    read(classTable(CLS_MAGNITUDE)), ['asciiValue']);
const charTable = mkInstance(read(classTable(CLS_ARRAY)), 256);
const charTableSym = wrapSymbol('AsciiTable');
const dict = mkDict(8);
insert(dict, charTableSym, charTable);
writeIV(chr, CLASS_POOL, dict);

const link = defClass(CLS_LINK, 'Link', object, ['link']);
defClass(CLS_PROCESS, 'Process', link,
    ['suspendedContext', 'priority', 'myList', 'threadId']);
const procSched = defClass(CLS_PROCESSOR_SCHEDULER, 'ProcessorScheduler',
    object, ['quiescentProcesses', 'activeProcess']);

defClass(CLS_SEMAPHORE, 'Semaphore', read(classTable(CLS_LINKED_LIST)),
    ['excessSignals']);

// Populate the process table.
const scheduler = mkInstance(procSched);
const priorities = mkInstance(read(classTable(CLS_ARRAY)), 7);
writeIVNew(scheduler, PROCESSOR_SCHEDULER_QUIESCENT_PROCESSES, priorities);
for(let i = 0; i < 7; i++) {
  writeArrayNew(priorities, i, mkInstance(read(classTable(CLS_LINKED_LIST))));
}


// Populate the character table.
for (let i = 0; i < 256; i++) {
  const c = basicNew(chr);
  writeIV(c, 0, toSmallInteger(i));
  writeArrayNew(charTable, i, c);
}

// Tricky to initialize these, so capturing it in a function.
export function newContext(method: ptr, sender: ptr, locals: ptr, opt_hasPrimitive = false): ptr {
  const [v_method, v_sender, v_locals, v_ctx] = seq(4);
  const ptrs = gcTemps(4);
  ptrs[v_method] = method;
  ptrs[v_sender] = sender;
  ptrs[v_locals] = locals;

  ptrs[v_ctx] = mkInstance(read(classTable(CLS_CONTEXT)), 19);
  writeIV(ptrs[v_ctx], CTX_PC, toSmallInteger(opt_hasPrimitive ? 1 : 0));
  writeIV(ptrs[v_ctx], CTX_STACK_INDEX, toSmallInteger(0));
  writeIV(ptrs[v_ctx], CTX_METHOD, ptrs[v_method]);
  writeIV(ptrs[v_ctx], CTX_LOCALS, ptrs[v_locals]);
  writeIV(ptrs[v_ctx], CTX_SENDER, ptrs[v_sender]);
  const ctx = ptrs[v_ctx];
  gcRelease(ptrs);
  return ctx;
}


export const SYM_SYSTEM_DICTIONARY = wrapSymbol('SystemDictionary');
export const SYM_PROCESSOR = wrapSymbol('Processor');
export const SYM_INPUT_HAND = wrapSymbol('InputHand');
export const SYM_MOUSE_CLICK = wrapSymbol('mouseClick');
export const SYM_MOUSE_BUTTON_EVENT = wrapSymbol('MouseButtonEvent');
export const SYM_POINT = wrapSymbol('Point');

insert(read(MA_GLOBALS), SYM_PROCESSOR, scheduler);


