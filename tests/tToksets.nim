import sugar, strutils, sequtils, strformat, macros

#===========================  implementation  ============================#
import hmisc/algo/hseq_mapping
import sets
import hmisc/types/colorstring
import ../src/hparse/[
  bnf_grammars,
  token,
  grammar_dsl,
  grammars,
  bnf_algo
]


#================================  tests  ================================#

import unittest

template bnfGrammar(body: untyped): untyped =
  let grammar =
    block:
      initGrammarCalls(NoCategory, string)
      initGrammarImpl(body)

  grammar.toGrammar().toBNF()

func `[]`*(first: FollowTable[NoCategory, string],
           nt: string): TokSet[NoCategory, string] =
  first[makeBnfNterm(nt)]


func `[]`*(first: FirstTable[NoCategory, string],
           nt: string): TokSet[NoCategory, string] =
  first[makeBnfNterm(nt)].mapPairs(rhs).union()

func contains*(nulls: NullableSet, str: string): bool =
  makeBnfNterm(str) in nulls

func getFollow[C, L](grm: BnfGrammar[C, L]): FollowTable[C, L] =
  let (_, follow, _) = getSets(grm)
  return follow

macro assertAll(body: untyped): untyped =
  assert body.kind == nnkStmtList
  result = newStmtList()
  for node in body:
    result.add newCall("assert", node)


suite "Token sets":
  test "Simple follow":
    block:
      let grm = bnfGrammar:
        A ::= B & "*"
        B ::= "-"

      assert grm.getFollow()["B"].sameset makeTokSet(@["*"])

    block:
      let grm = bnfGrammar:
        A ::= B & ("&" | "*")
        B ::= "()"

      assert grm.getFollow()["B"].sameset makeTokSet(@["&", "*"])

    block:
      let grm = bnfGrammar:
        A ::= B & C & "*"
        B ::= "-"
        C ::= null

      echo grm.exprRepr()
      let (first, follow, nulls) = grm.getSets()
      assertAll:
        # "C0" in nulls
        "C" in nulls
        follow["C"].sameset @["*"]
        follow["B"].sameset @["*"]
        follow["C"].sameset follow["B"]
