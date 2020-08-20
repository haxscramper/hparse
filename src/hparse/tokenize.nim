import strutils, parseutils

# TODO add stateful tokenizer with some kind of stack for states.

# TODO tokenize string literal in nim code an automatically generate
# positional information for `hmisc/hexception` to use.

proc tokenize*[C](
  str: string,
  catmap: openarray[(set[char], C)],
  skip: set[char] = Whitespace): seq[(C, string)] =
  var pos = 0
  while pos < str.len:
    let start = pos
    for (ch, cat) in catmap:
      if str[pos] in ch:
        pos = start + skipWhile(str, ch, pos)
        result.add (cat, str[start ..< pos])
        break

    pos = pos + skipWhile(str, skip, pos)

    if start == pos:
      # ERROR?
      break
