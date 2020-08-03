import sugar, strutils, sequtils, strformat, sets
import hmisc/algo/halgorithm
import hmisc/types/colorstring
import ../src/hparse/[
  grammar_dsl,
  parse_tree,
  grammars,
  token,
  lexer,
  ll1_gen,
  ll1_table,
  parse_primitives,
  bnf_grammars
]

import hpprint, hpprint/objdiff

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

func `$`[C, L, I](tr: ParseTree[C, L, I]): string = tr.lispRepr()

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
    initGrammarConst[En, string](grammar):
      A ::= en1 & en2

    let parser = newLL1RecursiveParser[En, string, void](grammar)
    let tree = makeTokens[En, string](@[en1, en2]).makeStream().withResIt:
      parser.parse(it)

    # pprint tree
    assertNoDiff tree, ParseTree[En, string, void](
      kind: ptkNterm,
      nterm: "A",
      subnodes: @[
        ParseTree[En, string, void](
          kind: ptkToken, tok: Token[En, string, void](cat: en1)),
        ParseTree[En, string, void](
          kind: ptkToken, tok: Token[En, string, void](cat: en2))
      ]
    )

  test "LL(1) string tokens":
    initGrammarConst[NoCategory, string](grammar):
      A ::= "hello" & *(B) & "world"
      B ::= "!!"

    let parser = newLL1RecursiveParser[NoCategory, string, void](grammar)
    let tree = makeTokens(
      @["hello", "!!", "!!", "!!", "world"]).makeStream().withResIt:
        parser.parse(it)

    echo tree.treeRepr()

  test "LL(1) predictive string tokens":
    let grammar = initGrammar[NoCategory, string]:
      A ::= "hello" & *(B) & "world"
      B ::= "!!"

    let parser = newLL1TableParser[NoCategory, string](grammar.toGrammar())
    let tree = makeTokens(
      @["hello", "!!", "!!", "!!", "world"]).makeStream().withResIt:
        parser.parse(it)

    echo tree.treeRepr()
