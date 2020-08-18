import sugar, strutils, sequtils, strformat, macros, options

import ../src/hparse, ../src/hparse/[ll1_gen, grammar_dsl]
import ../src/hparse/nimrx

#===========================  implementation  ============================#

#================================  tests  ================================#


import unittest

const defaultCategory = catNoCategory
suite "Compile-time parsing":
  test "Parser construction":

    initGrammarConst[NoCategory, string](grammar):
      A ::= "hello" & "world"

    let parser = newLL1RecursiveParser[NoCategory, string, void](grammar)
    let tree = "hello world".split(
      " ").makeTokens().makeStream().withResIt:
      parser.parse(it)

  macro parseTest(a: string): untyped =
    initGrammarConst[NoCategory, string](grammar):
      A ::= "hello" & "world"

    let parser = newLL1RecursiveParser[NoCategory, string, void](grammar)
    var input = ($a).split(" ").makeTokens().makeStream()
    # echo input.exprRepr()
    # echo typeof(input)
    # echo typeof(parser)
    let tree = parser.parse(input)
    echo tree.treeRepr()

  test "test":
    parseTest("hello world")

  test "RX macro":
    echo rx"(and word (| word))"
