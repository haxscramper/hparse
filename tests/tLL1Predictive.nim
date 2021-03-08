import std/[sugar, strutils, sequtils, strformat,
            sets, random, colors, macros]
import hasts/[graphviz_ast, html_ast]


#===========================  implementation  ============================#

import hparse/[
  ll1_table, # Table-driven LL(1) parser
  ll1_gen,   # Codegen-based LL(1) parser
  earley,    # Earley parser
  grammars,
  bnf_grammars, # FIXME HACK not necessary but removeing it generates
                # `rule cannot be called` compilation error.
  token,
  lexer
]

import hmisc/algo/hseq_mapping # HACK fixes `undeclared field 'nthType1'`

include example_grammar

#================================  tests  ================================#

import unittest

suite "Predictive LL(1)":
  let nt = nterm[TokenKind, string]
  proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
  test "Simple grammar":
    let nte = nt("element")
    let grammar = {
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "["),
        nt("elements"),
        tok(tkPunct, "]")
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nte,
        zeroP(andP(tok(tkPunct, ","), nt("element")))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(tok(tkIdent), nt("list"))
    }.toGrammar()

    let tableParser = newLL1TableParser(grammar, retainGenerated = false)

    var stream = mapString("[a,b,c,d,e]").makeStream()
    let tree = tableParser.parse(stream)
    tree.topng("/tmp/tree.png")

import hparse/ll1_table


suite "Table-driven vs recursive descent":
  test "Image generation":
    const nt = nterm[TokenKind, string]
    proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
    const grammarConst = {
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "["),
        nt("elements"),
        tok(tkPunct, "]")
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element"),
        zeroP(andP(tok(tkPunct, ","), nt("element")))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(tok(tkIdent), nt("list"))
    }

    let grammarVal = grammarConst
    echo grammarVal.toGrammar().exprRepr()

    let
      recursiveParser = newLL1RecursiveParser[
        TokenKind, string, LexInfo](grammarConst)
      tableParser = newLL1TableParser[TokenKind, string](
        grammarVal.toGrammar(), retainGenerated = false,
        dofixup = false
      )

      testInput = "[a,b,e,e,z,e]"


    let
      recursiveTree = mapString(testInput).makeStream().withResIt:
        recursiveParser.parse(it)
      tableTree = mapString(testInput).makeStream().withResIt:
        tableParser.parse(it)


    let color = TokenStyleCb[TokenKind, string, LexInfo](
      cb: proc(tok: LTok): TokenStyle =
        case tok.cat:
          of tkPunct:
            result.color = colRed
          of tkIdent:
            result.color = colBlue
    )

    let
      tokens: seq[LTok] = mapString(testInput).makeStream().getBuffer()
      tokenNode = tokens.mapPairs(
        toHtmlTableVert(@[
          #[ IMPLEMENT correct token coloring ]#
          toHtmlText(
            ("  " & $rhs.cat & "  ").tokKindStr("tk"),
            color = color.highlight(rhs).color).toHtmlCell().withIt do:
              it["align"] = "center"
              it.dotPort = idx + 1
          ,
          toHtmlText(escapeHtmlGraphviz($rhs.lex), props = {htpBold})
        ])
      ).toHtmlTableHoriz().withIt do:
        it.border = 1

    "/tmp/page.html".writeFile(tokenNode.toHtmlDoc())

    var resultGraph: DotGraph
    block:
      var tree = recursiveTree.toDotGraph(
        colorCb = color, idshift = 1, bottomTokens = true)
      tree.isCluster = true
      tree.name = "recursive"
      tree.topNodes.add:
        withIt makeDotNode(
          toDotNodeId(rand(100000)), grammarVal.toGrammar().exprRepr()):
          it.width = 10
          it.labelAlign = nlaLeft
          it.labelLeftPad = " ".repeat(10)

      let tokNodeId = toDotNodeId(rand(100000))
      for it in tokens:
        let pos = it.getPosInfo() + 1
        tree.addEdge(makeDotEdge(
            pos.toDotNodeId(), tokNodeId.addRecord(pos)))

      let tokNode = makeDotNode(tokNodeId, tokenNode)
      tree.addNode tokNode
      resultGraph.addSubgraph(tree)

    block:
      var tree = tableTree.toDotGraph(colorCb = color, bottomTokens = true)
      tree.isCluster = true
      tree.name = "table"
      tree.topNodes.add:
        withIt makeDotNode(
          toDotNodeId(rand(100000)),
          tableParser.getGrammar().exprRepr(true)
        ):
          it.width = 10
          it.labelAlign = nlaLeft
          it.labelLeftPad = " ".repeat(10)


      let tokNodeId = toDotNodeId(rand(100000))
      for it in tokens:
        let pos = it.getPosInfo() + 1
        tree.addEdge(makeDotEdge(pos.toDotNodeId(),
                                 tokNodeId.addRecord(pos)))

      let tokNode = makeDotNode(tokNodeId, tokenNode)
      tree.addNode tokNode

      resultGraph.addSubgraph(tree)

    resultGraph.styleNode.fontname = "Consolas"
    resultGraph.toPng("/tmp/combined.png", tmpfile = "/tmp/combined-dot.dot")

    echo "Starting earley parser"
    let
      earleyParser = newEarleyParser[TokenKind, string](
        grammarVal.toGrammar())

      earleyTree = mapString("[a]").makeStream().withResIt:
        earleyParser.parse(it)

    earleyTree[0].toPng("/tmp/earley.png")
    earleyTree[0].printTreeRepr()
    echo "Finished earley parser"
