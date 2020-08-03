import unittest, sequtils, options, random, strutils
import typetraits
import hpprint
import hmisc/algo/[halgorithm, htree_mapping]
import hasts/graphviz_ast
import hparse/[
  ll1_gen, grammars, parse_primitives, lexer, token], macros
include example_grammar

import hparse/bnf_grammars # Unless I import this one explicitly I get
                           # error with `hashes`.
import hashes, sets, tables

#======================  grammar parser generation  ======================#

#======================  dummy value construction  =======================#

# proc pe(args: varargs[PTree]): PTree = newTree(toSeq(args))
# proc pt(tok: TokenKind): PTree = newTree(Token(kind: tok))
# proc pn(name: NTermSym, args: varargs[PTree]): PTree =
#   newTree(name, toSeq(args))

# proc `$`(a: PTree): string = pstring(a)

# proc tokensBFS(tree: PTree): seq[Token] =
#   tree.mapItBFStoSeq(
#     it.getSubnodes(),
#     if it.kind == ptkTerm: some(it.tok) else: none(Token)
#   )

# proc parseToplevel[Tok](
#   toks: seq[Tok],
#   parseCb: proc(
#     toks: var TokStream[Tok]): ParseTree[Tok]): ParseTree[Tok] =
#   var stream = makeStream(toks)
#   return parseCb(stream)

# proc toTokSeq(inseq: seq[TokenKind]): seq[Token] =
#   inseq.mapIt(Token(kind: it))

suite "LL(1) parser simple":
  const nt = nterm[TokenKind, string]
  proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
  let parser = newLL1RecursiveParser[Token, string, LexInfo]({
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "["),
        nt("elements"),
        tok(tkPunct, "]")
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element"),
        zeroP(andP(
          tok(tkPunct, "["),
          nt("element")
        ))
      ),
      # element ::= 'ident' | <list>
      "element" : orP(
        tok(tkIdent),
        nt("list")
      )
    })

suite "LL(1) parser tree actions":
  const nt = nterm[TokenKind, string]
  proc tok(k: TokenKind): auto = tok[TokenKind, string](k)
  newLL1RecursiveParser[Token, string, LexInfo]({
      # list ::= '[' <elements> ']'
      "list" : andP(
        tok(tkPunct, "[").addAction(taDrop),
        nt("elements").addAction(taSpliceDiscard),
        tok(tkPunct, "]").addAction(taDrop)
      ),
      # elements ::= <element> (',' <element>)*
      "elements" : andP(
        nt("element").addAction(taSpliceDiscard),
        zeroP(andP(
          tok(tkPunct, ",").addAction(taDrop),
          nt("element").addAction(taSpliceDiscard)
        ).addAction(taSpliceDiscard)).addAction(taSpliceDiscard)
      ),
      # element ::= 'ident' | <list>
      "element" : orP(
        tok(tkIdent).addAction(taPromote),
        nt("list").addAction(taPromote)
      )
    }, standalone = true)

  test "Drop rule":
    let text = "[[c,z,d,[e,d]],[e,d,f]]"
    # let text = "[a,b]"
    var toks = mapString(text).makeStream(
      # nextTokCb = (
      #   proc(tok: LTok, pos: int) = echo "Reading ", tok.exprRepr(), " @ ", pos
      # )
    )
    let tree = parser.parse(toks)

    echo "--- FINAL ---"
    echo tree.treeRepr("tk")
    tree.topng("/tmp/image.png", "tk", bottomTokens = true)
