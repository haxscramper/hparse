import sugar, strutils, sequtils, strformat
import hmisc/helpers
import hparse/[grammars, grammar_dsl, parse_tree, parse_primitives]

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

# func `==`[S, C, L](l, r: array[S, (string, Patt[C, L])]): bool =
#   result = true
#   for (lhs, rhs) in zip(l, r):
#     if (lhs[0] != rhs[0]) or not (grammars.`==`(lhs[1], rhs[1])):
#       return false

type
  Tkind = enum
    tkA
    tkB

let nt = nterm[TKind, string]
proc tok(lex: string): auto = tok[TKind, string](tkA, lex)
proc tok(lex: TKind): auto = tok[TKind, string](tkA)

suite "Grammar DSL":
  test "Grammar literal construction":
    assertEq makeGrammarImpl(A ::= Q & B & C), {
      "A" : andP(nt("Q"), nt("B"), nt("C"))
    }

    assertEq makeGrammarImpl(A ::= "$" & Z), {
      "A" : andP(tok("$"), nt("Z"))
    }

    assertEq makeGrammarImpl(A ::= "$" & *(Z & A)), {
      "A" : andP(tok("$"), zeroP(andP(nt("Z"), nt("A"))))
    }

    assertEq makeGrammarImpl(A ::= (A | B) & C), {
      "A" : andP(orP(nt("A"), nt("B")), nt("C"))
    }


    assertEq do:
        {"A" : nt("B"), "C" : nt("D")}
    do:
      makeGrammarImpl:
        A ::= B
        C ::= D

    assertEq do:
      makeGrammarImpl:
        List ::= "[" & Elements & "]"
        Elements ::= Element & *("," & Element)
        Element ::= tkA | List
    do:
       {
         "List" : andP(tok("["), nt("Elements"), tok("]")),
         "Elements" : andP(
           nt("Element"), zeroP(andP(tok(","), nt("Element")))),
         "Element" : orP(tok(tkA), nt("List"))
       }

  test "Grammar tree actions":
    assertEq makeGrammarImpl(A ::= !B), {
      "A" : nt("B").addAction(taDrop)
    }

    assertEq makeGrammarImpl(A ::= !B & C), {
      "A" : andP(nt("B").addAction(taDrop), nt("C"))
    }

    assertEq makeGrammarImpl(A ::= (A | !B) & C), {
      "A" : andP(orP(nt("A"), nt("B").addAction(taDrop)), nt("C"))
    }
