import sugar, strutils, sequtils, strformat

import hmisc/helpers
import ../src/hparse/[
  token,
  parse_tree,
  grammars,
  bnf_grammars,
  llstar_gen,
  grammar_dsl,
  lexer
]

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "LL(*) gen":
  test "test":
    initGrammarConst[NOCategory, string](grammar):
      A ::= "("

    let parser = newLLStarParser[NoCategory, string, void](grammar)
    let tree = makeTokens(@["(", "0", ")"]).makeStream().withResIt:
      parser.parse(it)

    echo tree.treeRepr()
