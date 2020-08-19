import sugar, strutils, sequtils, strformat, options

import hmisc/helpers
import ../src/hparse/[
  token,
  parse_tree,
  grammars,
  bnf_grammars,
  llstar_gen,
  grammar_dsl,
  lexer,
  parse_primitives
]

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "LL(*) gen":
  const defaultCategory = catNoCategory
  test "test":
    initGrammarConst[NOCategory, string](grammar):
      A ::= "(" | "0"

    let parser = newLLStarParser[NoCategory, string, void](grammar)
    let tree = makeTokens(@["0"]).makeStream().withResIt:
      parser.parse(it)

  test "Ambiguous":
    initGrammarConst[NOCategory, string](grammar):
      A ::= B | C
      B ::= D & "(" & ")"
      C ::= D & "[" & "]"
      D ::= "proc" & "_"

    let parser = newLLStarParser[NoCategory, string, void](grammar)

    block:
      let tree = makeTokens(@["proc", "_", "[", "]"]
        ).makeStream().withResIt:
          parser.parse(it)

      echo tree.treeRepr()


  test "List DSL":
    initGrammarConst[NoCategory, string](grammar):
      List ::= !"[" & Elements & !"]"
      Elements ::= Element & @*(@(!"," & Element))
      Element ::= "i" | List

    var toks = @["[", "i", ",", "i", ",", "i", ",", "i", "]"
      ].makeTokens().makeStream()


    let parser = newLLStarParser[NoCategory, string, void](grammar)
    let tree = parser.parse(toks)
    echo tree.treeRepr()

  test "Elisp funcall":
    initGrammarConst[NoCategory, string](grammar):
      List ::= !"(" & "f" & @*(Element) & !")"
      # Elements ::= Element & @*(@(!"," & Element))
      Element ::= "i" | List

    let parser = newLLStarParser[NoCategory, string, void](grammar)

    proc testToks(tok: seq[string]): void =
      let tree = tok.makeTokens().makeStream().withResIt:
        parser.parse(it)

      echo tree.treeRepr()

    testToks @["(", "f", "i", "i", "i", ")"]
    testToks @["(", "f", "i", "(", "f", "i", ")", ")"]
