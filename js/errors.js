
class DoesNotUnderstandError extends Error {
  constructor(receiverClassName, selector) {
    super(receiverClassName + ' does not understand #' + selector);
  }
}

class ArgumentCountMismatchError extends Error {
  constructor(receiverClassName, selector, expected, actual) {
    super(receiverClassName + '>>#' + selector + ' expected ' + expected +
        ' arguments, but got ' + actual);
  }
}

class UnknownBuiltinError extends Error {
  constructor(builtinName) {
    super('Unknown builtin: ' + builtinName);
  }
}

class UnknownBytecodeError extends Error {
  constructor(bytecodeName) {
    super('Unknown bytecode: ' + bytecodeName);
  }
}

// For compiler and VM errors, not the user's fault.
class InternalError extends Error {
  constructor(msg) {
    super(msg);
  }
}
