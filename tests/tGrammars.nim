import sugar, strutils, sequtils, strformat
import hparse/[grammars, bnf_grammars, parse_primitives]

include example_grammar

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest
let nt = nterm[TokenKind, string]
proc tok(k: TokenKind): auto = tok[TokenKind, string](k)

suite "EBNF -> BNF convesion":
  proc conv[Tk, string](patt: Patt[Tk, string]): void =
    echo "\n\e[41mVVVVVVVVVVVVVVVVVVVV\e[0m"
    let ebnf = rule("X", patt)

    echo "\e[32mEBNF:\e[0m"
    echo ebnf.exprRepr()
    block:
      echo "\e[32mBNF not flattened:\e[0m"
      let newrules = ebnf.toBNF()
      for rule in newrules:
        echo rule.exprRepr()

    block:
      echo "\e[32mBNF flattened:\e[0m"
      let newrules = ebnf.toBNF(true)
      for rule in newrules:
        echo rule.exprRepr()

  test "wee":
    conv(andP(
      zeroP(andP(nt "E", nt "##")).addAction(taSpliceDiscard),
      tok(tkPunct).addAction(taDrop)
    ))

    # conv(andP(
    #   nt("element"),
    #   zeroP(andP(
    #     oneP(tok(tkPunct)),
    #     zeroP(nt("element"))
    #   )),
    #   optP(tok(tkPunct))))
