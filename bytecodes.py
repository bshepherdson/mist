class BytecodeStream:
  def __init__(self):
    self.contents = []
    self.scope = Scope()

  def add(self, x):
    self.contents.append(x.emit())

  def addAll(self, xs):
    for x in xs:
      self.add(x)

class Scope:
  def __init__(self, parent=None):
    self.parent = parent
    self.map = {}

  def add(self, key, value):
    self.map[key] = value

  def sub(self):
    return Scope(self)

  def lookup(self, key):
    if key in self.map:
      return self.map[key]
    if self.parent is not None:
      return self.parent.lookup(key)
    return None


class Bytecode:
  def __init__(self, codename, bcnum):
    self.codename = codename
    self.bcnum = bcnum

  def emit(self):
    # Default case that just emits the codename as an empty object.
    return {"bytecode": self.codename}

class BCPushLocal(Bytecode):
  def __init__(self, index):
    super().__init__("pushLocal", 1)
    self.index = index

  def emit(self):
    return {**super().emit(), "index": self.index}


class BCPushGlobal(Bytecode):
  def __init__(self, name):
    super().__init__("pushGlobal", 2)
    self.name = name

  def emit(self):
    return {**super().emit(), "name": self.name}


class BCPushSelf(BCPushLocal):
  def __init__(self):
    super().__init__(0)

class BCPushInstVar(Bytecode):
  def __init__(self, index):
    super().__init__("pushInstVar", 3)
    self.index = index

  def emit(self):
    return {**super().emit(), "index": self.index}


class BCPushLiteral(Bytecode):
  def __init__(self, value):
    super().__init__("pushLiteral", 10)
    self.value = value

  def emit(self):
    return {**super().emit(), "value": self.value}


class BCStoreLocal(Bytecode):
  def __init__(self, index):
    super().__init__("storeLocal", 8)
    self.index = index

  def emit(self):
    return {**super().emit(), "index": self.index}

class BCStoreInstVar(Bytecode):
  def __init__(self, index):
    super().__init__("storeInstVar", 9)
    self.index = index

  def emit(self):
    return {**super().emit(), "index": self.index}


class BCStartBlock(Bytecode):
  def __init__(self, argc, argStart, codeLen):
    super().__init__("startBlock", 4)
    self.argc = argc
    self.argStart = argStart
    self.codeLen = codeLen

  def emit(self):
    return {**super().emit(), "argc": self.argc, "argStart": self.argStart, "length": self.codeLen}

class BCCreateMethod(Bytecode):
  def __init__(self, selector, argc, tempCount, codeLen):
    super().__init__("startMethod", 13)
    self.selector = selector
    self.argc = argc
    self.tempCount = tempCount
    self.codeLen = codeLen

  def emit(self):
    return {
        **super().emit(),
        "selector": self.selector,
        "argc": self.argc,
        "temps": self.tempCount,
        "length": self.codeLen,
        }

class BCSend(Bytecode):
  def __init__(self, selector, values):
    """Remember that the values includes the receiver, so it can't be 0."""
    super().__init__("send", 5)
    self.selector = selector
    self.values = values

  def emit(self):
    return {**super().emit(), "selector": self.selector, "values": self.values}

class BCSuperSend(BCSend):
  def emit(self):
    return {**super().emit(), "super": true}


class BCDup(Bytecode):
  def __init__(self):
    super().__init__("dup", 6)

class BCDrop(Bytecode):
  def __init__(self):
    super().__init__("drop", 7)


class BCAnswer(Bytecode):
  def __init__(self):
    super().__init__("answer", 11)

class BCAnswerBlock(Bytecode):
  def __init__(self):
    super().__init__("answerBlock", 12)

class BCAnswerSelf(Bytecode):
  """Slightly premature optimization; this gets inserted at the end of method
  bodies that don't end with an answer. Blocks get a normal answer."""
  def __init__(self):
    super().__init__("answerSelf", 15)

class BCPrimitive(Bytecode):
  def __init__(self, keyword, name):
    super().__init__("primitive", 14)
    self.keyword = keyword
    self.name = name

  def emit(self):
    return {**super().emit(), "keyword": self.keyword, "name": self.name}

# next ID: 16
