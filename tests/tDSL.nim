import sugar, strutils, sequtils, strformat, macros
import hmisc/helpers
import hparse/[
  grammars,
  grammar_dsl,
  parse_tree,
  parse_primitives,
  token,

  # grammar_dsl,
  # parse_tree,
  # grammars,
  # token,
  # lexer,
  # ll1_gen,
  # ll1_table,
  # parse_primitives,
  # bnf_grammars
]

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



suite "Grammar base":
  type
    Tkind = enum
      tkA
      tkB

  let nt = nterm[TKind, string]
  proc tok(lex: string): auto = tok[TKind, string](tkA, lex)
  proc tok(lex: TKind): auto = tok[TKind, string](tkA)


  template grm(body: untyped): untyped =
    initGrammarImpl(body)

  test "Grammar literal construction":
    discard initGrammarImpl(A ::= tkA & tkB)

    assertEq initGrammarImpl(A ::= Q & B & C), {
      "A" : andP(nt("Q"), nt("B"), nt("C"))
    }

    assertEq initGrammarImpl(A ::= B & C), {
      "A" : andP(nt("B"), nt("C"))
    }

    assertEq initGrammarImpl(A ::= tkA & tkB), {
      "A" : andP(tok(tkA), tok(tkB))
    }

    assertEq initGrammarImpl(A ::= "$" & Z), {
      "A" : andP(tok("$"), nt("Z"))
    }

    assertEq initGrammarImpl(A ::= "$" & *(Z & A)), {
      "A" : andP(tok("$"), zeroP(andP(nt("Z"), nt("A"))))
    }

    assertEq initGrammarImpl(A ::= (A | B) & C), {
      "A" : andP(orP(nt("A"), nt("B")), nt("C"))
    }


    assertEq do:
        {"A" : nt("B"), "C" : nt("D")}
    do:
      initGrammarImpl:
        A ::= B
        C ::= D

    assertEq do:
      initGrammarImpl:
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

    assertEq grm(A ::= [B] & C), {"A" : andP(optP(nt "B"), nt("C"))}
    assertEq grm(A ::= ?B & C), {"A" : andP(optP(nt "B"), nt("C"))}
    assertEq grm(A ::= [B & C]), {"A" : optP(andP(nt "B", nt "C"))}
    assertEq grm(A ::= [B | C]), grm(A ::= [B | C])
    assertEq grm(A ::= ?(A | B)), grm(A ::= [A | B])


  test "Grammar tree actions":
    assertEq initGrammarImpl(A ::= !B), {
      "A" : nt("B").addAction(taDrop)
    }

    assertEq initGrammarImpl(A ::= !B & C), {
      "A" : andP(nt("B").addAction(taDrop), nt("C"))
    }

    assertEq initGrammarImpl(A ::= (A | !B) & C), {
      "A" : andP(orP(nt("A"), nt("B").addAction(taDrop)), nt("C"))
    }

    assertEq initGrammarImpl(A ::= !B & @C & ^D & ^@E), {
      "A" : andP(
        nt("B").addAction(taDrop),
        nt("C").addAction(taSpliceDiscard),
        nt("D").addAction(taPromote),
        nt("E").addAction(taSplicePromote)
      )
    }

    assertEq initGrammarImpl(A ::= !*(C) & U), {
      "A" : andP(zeroP(nt("C")).addAction(taDrop), nt("U"))
    }

    assertEq grm(A ::= {B & C} & D), {
      "A" : andP(
        andP(nt "B", nt "C").addAction(taSubrule), nt("D")
      )
    }

    # NOTE expect compilation error message
    # discard grm(A ::= *{A & B})
    # discard grm(A ::= !{A & B})

    assertEq grm(A ::= !*B), grm(A ::= !(*B))
    assertEq grm(A ::= !*B & !+C), grm(A ::= !(*B) & !(+C))

    assertNoDiff do:
      initGrammarImpl:
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
          oneP(nt("Z")).addAction(taSpliceDiscard),
          orP(
            nt("O").addAction(taSpliceDiscard),
            zeroP(nt("E")).addAction(taSplicePromote)
          )
        )
      }


    assertEq do:
      initGrammarImpl:
        List ::= "[" & Elements & "]"
        Elements ::= Element & @*(!"," & Element)
        Element ::= tkA | List
    do:
       {
         "List" : andP(tok("["), nt("Elements"), tok("]")),
         "Elements" : andP(
           nt("Element"), zeroP(
             andP(tok(",").addAction(taDrop), nt("Element"))
           ).addAction(taSpliceDiscard)),
         "Element" : orP(tok(tkA), nt("List"))
       }

  test "Grammar template rules":
    proc csvList(str: string): Patt[TKind, string] =
      andP(tok(str), zeroP(andP(tok(","), tok(str))))

    assertEq grm(A ::= %csvList("cat")), {
      "A" : csvList("cat")
    }

    assertEq grm(A ::= %csvList("cat")), {
      "A" : andP(tok("cat"), zeroP(andP(tok(","), tok("cat"))))
    }

suite "Grammar DSL API":
  test "{initGrammar} template":
    # TODO test `void` category type with string lexemes
    type
      En = enum
        tkA
        tkB

    let ntUsr = nterm[En, string]
    proc tokUsr(lex: string): auto = tok[En, string](tkA, lex)
    proc tokUsr(lex: En): auto = tok[En, string](tkA)

    let grammar = initGrammar[En, string]:
      A ::= B

    block:
      discard initGrammar[En, string]:
        A ::= tkA & tkB

      assertEq do:
        initGrammar[En, string]:
          A ::= tkA & tkB
      do:
        {"A" : andP(tok[En, string](tkA), tok[En, string](tkB))}

      const grm0 = {"A" : andP(tok[En, string](tkA), tok[En, string](tkB))}

      let grm = initGrammar[En, string]:
        A ::= tkA & tkB

      # const grm2 = initGrammar[En, string]:
      #   A ::= tkA & tkB

    assertEq do:
      initGrammar[En, string]:
        A ::= B
    do:
      {"A" : ntUsr("B")}

  test "{initGrammar} with `void` category":
    let grammar = initGrammar[void, string]:
      A ::= "hello"

    assertEq do:
      initGrammar[void, string]:
        A ::= "hello111"
    do:
      {"A" : voidCatTok("hello111")}


  test "{initGrammarConst}":
    initGrammarConst[void, string](grammar):
      A ::= B

    assertEq grammar, {"A" : nterm[void, string]("B")}

  test "{initGrammar} with `NoCategory`":
    let grammar = initGrammar[NoCategory, string]:
      A ::= "hello" & *(B) & "world"
      B ::= "!!"

  test "{initGrammar} `prefCat.lex` for tokens":
    type
      Cat = enum
        ctOne
        ctTwo

    # dumpTree:
    #   {"A": andP(tok(ctOne, "hello"), tok(ctTwo, "world"))}

    block: # Automatically infer prefix for token category
      let grammar = initGrammar[Cat, string]:
        A ::= "hello".one & "world".two

      assertEq grammar, {
        "A" : andP(
          tok(ctOne, "hello"),
          tok(ctTwo, "world")
        )
      }

    block: # But don't just prepend it two all lowercase identifiers
      let grammar = initGrammar[Cat, string]:
        A ::= "hello".ctOne & "world".ctTwo

      assertEq grammar, {
        "A" : andP(
          tok(ctOne, "hello"),
          tok(ctTwo, "world")
        )
      }

    block:
      assertEq do:
        initGrammar[Cat, string]:
          A ::= one
      do:
        {
          "A" : tok makeExpToken[Cat, string](ctOne)
        }
