import * as m from './memory';

export function printStackTrace(ctx: m.ptr) {
  // Print the details of each context in the sender chain, with its method, PC etc.
  while (ctx !== m.MA_NIL) {
    const method = printMethod(m.readIV(ctx, m.CTX_METHOD));
    const pc = m.fromSmallInteger(m.readIV(ctx, m.CTX_PC));
    const stack = [];
    const sp = m.fromSmallInteger(m.readIV(ctx, m.CTX_STACK_INDEX));
    for (let i = 0; i < sp; i++) {
      stack.push(printValue(m.readArray(ctx, i)));
    }

    const localArr = m.readIV(ctx, m.CTX_LOCALS);
    const nLocals = m.arraySize(localArr);
    const locals = [];
    for (let i = 0; i < nLocals; i++) {
      locals.push(printValue(m.readArray(localArr, i)));
    }

    console.log('stack frame', {
      method,
      pc,
      locals,
      stack,
    });

    ctx = m.readIV(ctx, m.CTX_SENDER);
  }
}

(window as any)['printStackTrace'] = printStackTrace;

interface StackFrame {
  name: string;
  literals: string[];
  bytecode: string[];
}

function printMethod(methodOrBlock: m.ptr): StackFrame {
  let block: m.ptr|null = null;
  let method: m.ptr = methodOrBlock;
  if (m.hasClass(methodOrBlock, m.CLS_BLOCK_CLOSURE)) {
    block = methodOrBlock;
    method = m.readIV(m.readIV(methodOrBlock, m.BLOCK_CONTEXT), m.CTX_METHOD);
  }

  const cls = m.readIV(method, m.METHOD_CLASS);
  const className = m.className(cls);
  const selector = m.asJSString(m.readIV(method, m.METHOD_NAME));
  const bc = m.readIV(method, m.METHOD_BYTECODE);
  const bytecode = [];
  for (let i = 0; i < m.wordArraySize(bc); i++) {
    bytecode.push(m.readWordArray(bc, i).toString(16));
  }

  const litArr = m.readIV(method, m.METHOD_LITERALS);
  const literals = [];
  for (let i = 0; i < m.arraySize(litArr); i++) {
    literals.push(printValue(m.readArray(litArr, i)));
  }

  let name = className + ' >> #' + selector;
  if (block) {
    name = 'Block at ' + m.fromSmallInteger(m.readIV(block, m.BLOCK_PC_START)) +
        ' of ' + name;
  }
  return {name, literals, bytecode};
}

export function printValue(p: m.ptr): string {
  const cls = m.classOf(p);
  const clsHash = m.identityHash(cls);
  switch (clsHash) {
    case m.CLS_SMALL_INTEGER:
      return '' + m.fromSmallInteger(p);
    case m.CLS_TRUE:
      return 'true';
    case m.CLS_FALSE:
      return 'false';
    case m.CLS_UNDEFINED_OBJECT:
      return 'nil';
    case m.CLS_ARRAY:
      const parts = [];
      for (let i = 0; i < m.arraySize(p); i++) {
        parts.push(printValue(m.readArray(p, i)));
      }
      return '#( ' + parts.join('. ') + ' )';
    case m.CLS_WORD_ARRAY:
      const words = [];
      for (let i = 0; i < m.wordArraySize(p); i++) {
        words.push(m.readWordArray(p, i).toString(16));
      }
    case m.CLS_STRING:
      return '\'' + m.asJSString(p) + '\'';
    case m.CLS_SYMBOL:
      return '#' + m.asJSString(p);
    default:
      return 'a ' + m.className(cls);
  }
  throw new Error('cant happen');
}
