import sugar, strutils, sequtils, strformat
import hmisc/helpers
import hparse/[grammars, grammar_dsl, parse_tree, parse_primitives]

import hpprint/objdiff
import hmisc/macros/obj_field_macros

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
  template grm(body: untyped): untyped =
    makeGrammarImpl(body)

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
    # TODO TEST subrule
    assertEq makeGrammarImpl(A ::= !B), {
      "A" : nt("B").addAction(taDrop)
    }

    assertEq makeGrammarImpl(A ::= !B & C), {
      "A" : andP(nt("B").addAction(taDrop), nt("C"))
    }

    assertEq makeGrammarImpl(A ::= (A | !B) & C), {
      "A" : andP(orP(nt("A"), nt("B").addAction(taDrop)), nt("C"))
    }

    assertEq makeGrammarImpl(A ::= !B & @C & ^D & ^@E), {
      "A" : andP(
        nt("B").addAction(taDrop),
        nt("C").addAction(taSpliceDiscard),
        nt("D").addAction(taPromote),
        nt("E").addAction(taSplicePromote)
      )
    }

    assertEq makeGrammarImpl(A ::= !*(C) & U), {
      "A" : andP(zeroP(nt("C")).addAction(taDrop), nt("U"))
    }

    assertEq grm(A ::= !*B), grm(A ::= !(*B))
    assertEq grm(A ::= !*B & !+C), grm(A ::= !(*B) & !(+C))

    assertNoDiff do:
      makeGrammarImpl:
        A ::= !*A & !+B
        B ::= ^*(E) & ^+(E) & @+(Z) & (@O | ^@*(E))
    do:
      {
        "A" : andP(
          zeroP(nt("A")).addAction(taDrop) ,
          oneP(nt("B")).addAction(taDrop)),
        "B" : andP(
          zeroP(nt("E")).addAction(taPromote),
          oneP(nt("E")).addAction(taPromote),
          oneP(nt("Z").addAction(taSpliceDiscard)),
          orP(
            nt("O").addAction(taSpliceDiscard),
            zeroP(nt("E")).addAction(taSplicePromote)
          )
        )
      }
