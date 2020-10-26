import hparse, hparse/llstar_gen
import std/[macros, options, parseutils, sequtils]
import hparse/tokenize

type
  TagParts = enum
    tpIdent
    tpPunct

import unittest

template staticAndRuntime(body: untyped): untyped =
  static:
    body

  body

suite "Simple parsers":
  test "Nested tags":
    staticAndRuntime: # Check both runtime and compile-time parsing at
                      # once.
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
