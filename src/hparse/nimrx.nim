import macros, sequtils, strutils, options
import ../hparse, llstar_gen, grammar_dsl

# func tokenize(())

type
  RxKind = enum
    rxkIdent
    rxkStrLit
    rxkIntLit
    rxkOp
    rxkCl

  RxTok = Token[RxKind, string, void] # IDEA helper function to
  # generate information from relative position in string literals
  # (split string literal but retain positioning) and raise
  # `CodeError` if necessary.

func tok(kind: RxKind, lex: string): RxTok =
  makeToken[RxKind, string, void](kind, lex)

func tokenize(str: string): seq[RxTok] =
  for word in str.split(" "):
    debugecho word
    if word[0] == '(':
      result.add tok(rxkOp, "(")
      result.add tok(rxkIdent, word[1..^1]) # TODO check for correct
                                            # word
    elif word[^1] == ')':
      if word[0] == '\"':
        result.add tok(rxkStrLit, word[1..^3])
      else:
        result.add tok(rxkIdent, word[0..^2])
        # result.add tok(rxkCl, word[0..^2])

      result.add tok(rxkCl, ")")
    elif word[0] == '\"' and word[^1] == '\"':
      result.add tok(rxkStrLit, word[1..^2])
    else:
      result.add tok(rxkIdent, word)

initGrammarConst[RxKind, string](grammar):
  List ::= op & ident & *(Element) & cl
  Element ::= List | ident | strlit | intlit

static:
  echo grammar.toGrammar().exprRepr()

let parser {.compiletime.} =
  newLLStarParser[RxKind, string, void](grammar)


macro rx*(body: string): untyped =
  echo body.treeRepr()
  var toks = tokenize(body.strVal()).makeStream()
  echo toks.exprRepr()
  let tree = parser.parse(toks)
  echo tree.treeRepr()


  result = quote: 2
