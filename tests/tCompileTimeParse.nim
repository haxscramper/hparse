import sugar, strutils, sequtils, strformat, macros

import ../src/hparse, ../src/hparse/[ll1_gen, grammar_dsl]

#===========================  implementation  ============================#

#================================  tests  ================================#


import unittest

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
