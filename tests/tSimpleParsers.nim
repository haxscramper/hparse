import hparse, hparse/llstar_gen, options, parseutils, sequtils

type
  TagParts = enum
    tpIdent
    tpPunct

proc tokenize(
  str: string,
  catmap: openarray[(set[char], TagParts)]): seq[(TagParts, string)] =
  var pos = 0
  while pos < str.len:
    let start = pos
    for (ch, cat) in catmap:
      if str[pos] in ch:
        pos = start + skipWhile(str, ch, pos)
        result.add (cat, str[start ..< pos])
        break

    if start == pos:
      echo "no sets match character "
      echo str[pos], " at position ", pos
      break



import unittest

suite "Simple parsers":
  test "Nested tags":
    const defaultCategory = tpPunct

    let tokens = "#eee##[ee,ee]".tokenize({
      {'#'} : tpPunct,
      {'['} : tpPunct,
      {']'} : tpPunct,
      {','} : tpPunct,
      {'a'..'z', 'A'..'Z', '0'..'9'} : tpIdent
    }).mapIt:
      makeTokenNoInfo(it[0], it[1])


    initGrammarConst[TagParts, string](grammar):
      Tag ::= !"#" & ident & @*(@(!"##" & Subtag))
      Subtag ::= (!"[" & ident & @*(@(!"," & ident)) & !"]") | ident

    let parser = newLLStarParser[TagParts, string, void](grammar)

    var toks = tokens.makeStream()

    let tree = parser.parse(toks)
    echo tree.treeRepr()
