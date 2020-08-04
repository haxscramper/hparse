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
  earley,
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

    # echo parser.getGrammar().exprRepr()

    echo tree.treeRepr()

  test "Earley parser":
    let grammar = initGrammar[NoCategory, string]:
      A ::= "1" & "2"

    let parser = newEarleyParser[NoCategory, string](grammar.toGrammar())
    let tree = makeTokens(@["1", "2"]).makeStream().withResIt:
      parser.parse(it)

    echo tree[0].treeRepr()

suite "Compare parsers table vs codegen LL(1)":
  template testparse(tokens, grammarBody: untyped): untyped =
    let grammarVal =
      block:
        initGrammarCalls(NoCategory, string)
        initGrammarImpl(grammarBody)
      # initGrammar[NoCategory, string]:
      # grammarBody

    const grammarConst =
      block:
        initGrammarCalls(NoCategory, string)
        initGrammarImpl(grammarBody)

    echo grammarVal.toGrammar().exprRepr()

    block:
      let recParser = newLL1RecursiveParser[NoCategory, string, void](
        grammarConst)
      let recTree = makeTokens(tokens).makeStream().withResIt:
        recParser.parse(it)

      echo "Recursive tree"
      echo recTree.treeRepr()

    block:
      let tableParser = newLL1TableParser[NoCategory, string](
        grammarVal.toGrammar())
      let tableTree = makeTokens(tokens).makeStream().withResIt:
        tableParser.parse(it)

      echo "Table tree"
      echo tableTree.treeRepr()

  # test "Primitive grammar":
  #   testparse(@["e", "E"]):
  #     A ::= "e" & "E"

  test "Double splice one-or-more":
    testparse(@[":", ";", ":", ";", ":", ";"]):
      A ::= *(":" & ";")

    testparse(@[":", ";", ":", ";", ":", ";"]):
      A ::= *(@(":" & ";"))

  # test "List DSL":
  #   testparse(@["[", "i", ",", "i", ",", "i", ",", "i", "]"]):
  #     List ::= !"[" & Elements & !"]"
  #     Elements ::= Element & @*(@(!"," & Element))
  #     Element ::= "i" | List
