import sugar, strutils, sequtils, strformat, sets
import hmisc/algo/halgorithm
import ../src/hparse/[
  grammar_dsl,
  parse_tree,
  grammars,
  token,
  lexer,
  ll1_gen,
  ll1_table,
  parse_primitives
]

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Parser generation tests":
  type
    En = enum
      en1
      en2

  test "LL(1) codegen manual grammar":
    let parser = newLL1RecursiveParser[En, string, void]({
      "A" : andP(tok[En, string](en1), tok[En, string](en2))
    })

    let tree = makeTokens[En, string](@[en1, en2]).makeStream().withResIt:
      parser.parse(it)

  test "LL(1) codegen grammar dsl":
    makeGrammarConst[En, string](grammar):
      A ::= en1 & en2

    let parser = newLL1RecursiveParser[En, string, void](grammar)
    let tree = makeTokens[En, string](@[en1, en2]).makeStream().withResIt:
      parser.parse(it)
