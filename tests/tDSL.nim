import sugar, strutils, sequtils, strformat

import hparse/[grammars, grammar_dsl]

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

const nt = nterm[TKind, string]
proc tok(lex: string): auto = tok[TKind, string](tkA, lex)

suite "Grammar DSL":
  test "test":
    # makeGrammar(A ::= B)
    # makeGrammar(A ::= *B)
    assert makeGrammarImpl(A ::= Q & B & C) == {
      "A" : andP(nt("Q"), nt("B"), nt("C"))
    }

    assert makeGrammarImpl(A ::= "$" & Z) == {
      "A" : andP(tok("$"), nt("Z"))
    }

    assert makeGrammarImpl(A ::= "$" & *(Z & A)) == {
      "A" : andP(tok("$"), zeroP(andP(nt("Z"), nt("A"))))
    }

    # makeGrammar(A ::= (A | B) & C)
    # block:
    #   let grammar = makeGrammar:
    #     A ::= B
    #     C ::= D

    #   assertEq {"A" : nt("B"), "C" : nt("D")}, grammar

    # block:
    #   let grammar = makeGrammar:
    #     List ::= "[" & Elements & "]"
    #     Elements ::= Element & *("," & Element)
    #     Element ::= ident | List

    #   # assertEq {
    #   #   "List" :
    #   # }
