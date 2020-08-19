import sugar, strutils, sequtils, strformat, macros
import hparse/doc_example
import hmisc/helpers
import unittest
import hashes

suite "Grammar exampl":
  test "Tree fixup":
    const defaultCategory = catNoCategory
    initGrammarConst[NoCategory, string](grammar):
      A ::= "hello" & *(B) & "world"
      B ::= "!!"

    var toks = @[
      "hello", "!!", "!!", "!!", "world"].makeTokens().makeStream()

    let grammarVal =
      block:
        let tmp = grammar
        tmp.toGrammar()

    echo "Original grammar"
    echo grammarVal.exprRepr()
    echo "---\n\n"

    echo "Grammar converter to BNF"
    echo grammarVal.toBNF().exprRepr()
    echo "---\n\n"

    echo "Recursive descent tree"
    let parser1 = newLLStarParser[NoCategory, string, void](grammar)
    let tree1 = parser1.parse(toks)
    echo tree1.treeRepr()
    echo "---\n\n"

    toks.revertTo(0)

    echo "Table-driven parser tree without structure fixup"
    let parser2 = newLL1TableParser(
      grammarVal,
      dofixup = false,
      retainGenerated = true
    )
    let tree2 = parser2.parse(toks)
    echo tree2.treeRepr()
    echo "---\n\n"


    toks.revertTo(0)

    echo "Table-driven parser tree with fixup"
    let parser3 = newLL1TableParser(grammarVal, dofixup = true)
    let tree3 = parser3.parse(toks)
    echo tree3.treeRepr()
    echo "---\n\n"

suite "Tree actions examples":
  test "Drop tree action":
    echo ecompare(@["a", "b", "c"]) do:
      A ::= "a" & "b" & "c"
    do:
      A ::= "a" & !"b" & "c"


  test "Subrule tree action":
    echo ecompare(@["-", "z", "e"]) do:
      A ::= "-" & "z" & "e"
    do:
      A ::= "-" & { "z" & "e" }

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


  func makeExpTokenPredUsr(
    cat: NoCategory, value: bool): ExpectedToken[NoCategory, string] =

    result = makeExpTokenPred[NoCategory, string](
      catNoCategory, &"[{value}]",
      proc(str: string): bool = value
     )

  test "Predicates for tokens":
    when false:
      echo eparse(@["@ident", "#comment", "@ident"]) do:
        A ::= B | C
        B ::= [[ it.startsWith("@") ]]
        C ::= [[ true ]]

    when false: # THis causes **C** compilation error
      echo eparse(@["90", "---", "**"]) do:
        A ::= Ints | Punct
        Ints ::= [[ {'0' .. '9'} ]]
        Punct ::= [[ {'-', '*', ','} ]]

  test "Devnotes example 1":
    let nt = nterm[NoCategory, string]
    proc dslTok(lex: string): auto = tok[NoCategory, string](
      catNoCategory, lex)

    discard expandMacros:
      initGrammarImpl:
        List ::= "[" & Elements & "]"
        Elements ::= Element & @*(!"," & Element)
        Element ::= "," | List
