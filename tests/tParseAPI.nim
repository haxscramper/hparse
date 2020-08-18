import sugar, strutils, sequtils, strformat, sets, macros, options
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
  llstar_gen,
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
  when true:
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
      const defaultCategory = en1
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
            kind: ptkToken, tok: Token[En, string, void](kind: etokRegular,
                                                         cat: en1)),
          ParseTree[En, string, void](
            kind: ptkToken, tok: Token[En, string, void](kind: etokRegular,
                                                         cat: en2))
        ]
      )

    test "LL(1) string tokens":
      const defaultCategory = catNoCategory
      initGrammarConst[NoCategory, string](grammar):
        A ::= "hello" & *(B) & "world"
        B ::= "!!"

      let parser = newLL1RecursiveParser[NoCategory, string, void](grammar)
      let tree = makeTokens(
        @["hello", "!!", "!!", "!!", "world"]).makeStream().withResIt:
          parser.parse(it)

      echo tree.treeRepr()

    test "LL(1) predictive string tokens":
      const defaultCategory = catNoCategory
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
      const defaultCategory = catNoCategory
      let grammar = initGrammar[NoCategory, string]:
        A ::= "1" & "2"

      let parser = newEarleyParser[NoCategory, string](grammar.toGrammar())
      let tree = makeTokens(@["1", "2"]).makeStream().withResIt:
        parser.parse(it)

      echo tree[0].treeRepr()

template testparse(tokens, grammarBody: untyped): untyped =
  let grammarVal =
    block:
      const defaultCategory {.inject.} = catNoCategory
      initGrammarCalls(NoCategory, string)
      initGrammarImpl(grammarBody)
    # initGrammar[NoCategory, string]:
    # grammarBody

  const grammarConst =
    block:
      const defaultCategory {.inject.} = catNoCategory
      initGrammarCalls(NoCategory, string)
      initGrammarImpl(grammarBody)

  echo "EBNF grammar"
  echo grammarVal.toGrammar().exprRepr()


  block:
    let recParser = newLL1RecursiveParser[NoCategory, string, void](
      grammarConst)
    let recTree = makeTokens(tokens).makeStream().withResIt:
      recParser.parse(it)

    echo "Recursive tree"
    echo recTree.treeRepr()

  echo "BNF grammar"
  echo grammarVal.toGrammar().toBNF().exprRepr()

  block:
    let tableParser = newLL1TableParser[NoCategory, string](
      grammarVal.toGrammar())
    let tableTree = makeTokens(tokens).makeStream().withResIt:
      tableParser.parse(it)

    echo "Table tree"
    echo tableTree.treeRepr()


suite "Compare parsers table vs codegen LL(1)":
  when true:

    # test "Primitive grammar":
    #   testparse(@["e", "E"]):
    #     A ::= "e" & "E"

    test "Double splice one-or-more":
      # testparse(@[":", ";", ":", ";", ":", ";"]):
      #   A ::= *(":" & ";")

      testparse(@["-", ":", ";", ":", ";", ":", ";"]):
        A ::= "-" & @*(@(":" & ";"))

    test "List DSL":
      testparse(@["[", "i", ",", "i", ",", "i", ",", "i", "]"]):
        List ::= !"[" & Elements & !"]"
        Elements ::= Element & @*(@(!"," & Element))
        Element ::= "i" | List

suite "Predicate token":
  func makeExpTokenPredUsr(
    cat: NoCategory, valset: set[char]
       ): ExpectedToken[NoCategory, string] =

    result = makeExpTokenPred[NoCategory, string](
      catNoCategory, &"[{valset}]",
      proc(str: string): bool =
        for ch in str:
          if ch notin valset:
            return false
        return true
     )

  test "Predicate token":
    const defaultCategory = catNoCategory
    testparse(@["90", "---", "**"]) do:
      A ::= Ints | Punct
      Ints ::= [[ {'0' .. '9'} ]]
      Punct ::= [[ {'-', '*', ','} ]]


  test "Elisp funcall":
    const defaultCategory = catNoCategory
    initGrammarConst[NoCategory, string](grammar):
      List ::= !"(" & [[ {'f', 'F'} ]] & @*(Element) & !")"
      Element ::= "i" | List

    let parser = newLLStarParser[NoCategory, string, void](grammar)

    proc testToks(tok: seq[string]): void =
      let tree = tok.makeTokens().makeStream().withResIt:
        parser.parse(it)

      echo tree.treeRepr()

    testToks @["(", "f", "i", "i", "i", ")"]
    testToks @["(", "F", "i", "(", "f", "i", ")", ")"]

  test "tExample standalone":
    const defaultCategory = catNoCategory
    initGrammarConst[NoCategory, string](grammar):
      A ::= B | C
      B ::= [[ it.startsWith("@") ]]
      C ::= [[ true ]]

    let parser = newLLStarParser[NoCategory, string, void](grammar)

    proc testToks(tok: seq[string]): void =
      let tree = tok.makeTokens().makeStream().withResIt:
        parser.parse(it)

      echo tree.treeRepr()

    testToks (@["@ident", "#comment", "@ident"]
