const builtins = {};

function answer(ar, value) {
  ar.thread().pop();
  ar.parent().stack().push(value);
}

