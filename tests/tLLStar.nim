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
    # block:
    #   let tree = makeTokens(@["proc", "_", "(", ")"]
    #     ).makeStream().withResIt:
    #       parser.parse(it)

    #   echo tree.treeRepr()

    block:
      let tree = makeTokens(@["proc", "_", "[", "]"]
        ).makeStream().withResIt:
          parser.parse(it)

      echo tree.treeRepr()
