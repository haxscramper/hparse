import sugar, strutils, sequtils, strformat
import hparse/doc_example
import hmisc/helpers
import unittest

suite "Grammar exampl":
  test "Tree fixup":
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
