export class ArgumentCountMismatchError extends Error {
  constructor(receiverClassName: string, selector: string, expected: number, actual: number) {
    super(receiverClassName + '>>#' + selector + ' expected ' + expected +
        ' arguments, but got ' + actual);
  }
}

export class BlockArgumentCountMismatchError extends Error {
  constructor(expected: number, actual: number) {
    super('Block expected ' + expected + ' arguments, but got ' + actual);
  }
}

