// Expects the receiver and arguments to already be removed from the AR's stack.
// The AR is only used to get the containing method (super sends) and to chain
// the new AR on top.
// Returns the new AR for this message send (it's already pushed on the thread).
function send(ar, receiver, selector, opt_args, opt_superSend, opt_parentAR) {
  // First, look up the target method. We need to check its arg count and such.
  const args = opt_args || [];
  const argc = args.length;
  let startingClass = receiver.$class;

  // Super sends start looking for the parent class of *this* method's owner,
  // not the superclass of the receiver. This allows for chained super
  // initialize calls.
  if (opt_superSend) {
    startingClass = ar.method().$vars[METHOD_CLASS].$vars[CLASS_VAR_SUPERCLASS];
  }
  const method = methodLookup(selector, startingClass);

  if (!method) {
    throw new DoesNotUnderstandError(
        startingClass.$vars[CLASS_VAR_NAME].$vars[STRING_RAW], selector);
  }

  const methodArgc = method.$vars[METHOD_ARGC].$vars[NUMBER_RAW];
  if (methodArgc !== argc) {
    throw new ArgumentCountMismatchError(
        startingClass.$vars[CLASS_VAR_NAME].$vars[STRING_RAW],
        selector, methodArgc, argc);
  }

  // All is good: found the method and it has the right arg count for this send.
  // So we build a new activation record and set it up.
  args.unshift(receiver);

  const newAR = new ActivationRecord(ar.thread())
      .init(opt_parentAR || ar, args, method);
  newAR.thread().push(newAR);
  return newAR;
  // Execution will continue at this new location, then continue after the send.
}

