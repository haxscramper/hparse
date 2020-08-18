import macros, sequtils, strutils, options
import ../hparse, llstar_gen, grammar_dsl

# func tokenize(())

type
  RxTokKind = enum
    rxkIdent
    rxkStrLit
    rxkIntLit
    rxkOp
    rxkCl

  RxTok = Token[RxTokKind, string, void] # IDEA helper function to
  # generate information from relative position in string literals
  # (split string literal but retain positioning) and raise
  # `CodeError` if necessary.

  RxParse = ParseTree[RxTokKind, string, void]

  RxCharClass = enum
    rcsDigit
    rcsControl
    rcsHexDigit
    rcsBlank
    rcsGraph
    rcsPrint
    rcsAlnum
    rcsAlpha
    rcsAscii
    rcsNonAscii
    rcsLower
    rcsPunct
    rcsSpace
    rcsUpper
    rcsWord

  RxKind = enum
    rcsStrLit
    rcsChar ## Any character in set
    rcsNotChar ## Any character not in set
    rcsLineStart
    rcsLineEnd
    rcsStringStart
    rcsStringEnd
    rcsBufferStart
    rcsBufferEnd
    # rcsPoint
    rcsWordStart
    rcsWordEnd
    rcsBoundary

    rcsAnd
    rcsGroup
    rcsGroupIndexed
    rcsOr
    # rcsNonGreedy
    # rcsGreedy


    rcsZeroOrMore
    rcsZeroOrMoreNonGreedy

    rcsOneOrMore
    rcsOneOrMoreNonGreedy

    rcsOptional
    rcsOptionalNonGreedy

    rcsN_times
    rcsNOrMore_times
    rcsNtoM_times

    rcsBackrefN
    rcsEval
    rcsReLiteral

  RxSpec = object
    case kind*: RxKind
      of rcsChar, rcsNotChar:
        charclass*: RxCharClass
      else:
        subrx*: seq[RxSpec]

func mkCharClass(charclass: RxCharClass, negation: bool): RxSpec =
  if negation:
    RxSpec(kind: rcsNotChar, charclass: charclass)
  else:
    RxSpec(kind: rcsChar, charclass: charclass)

func toRxSpec(tree: RxParse): RxSpec =
  if tree.isToken:
    return case tree.tok.lex:
      of "word":
        mkCharClass(rcsWord, false)
      else:
        raiseAssert("#[ IMPLEMENT ]#")
  else:
    case tree.nterm:
      of "List":
        let kind = case tree[0].tok.lex:
          of "and":
            rcsAnd
          else:
            raiseAssert("#[ IMPLEMENT ]#")

        result = RxSpec(kind: kind)
        result.subrx = tree[1..^1].mapIt(it.toRxSpec())
      of "Element":
        discard

func tok(kind: RxTokKind, lex: string): RxTok =
  makeToken[RxTokKind, string, void](kind, lex)

func tokenize(str: string): seq[RxTok] =
  for word in str.split(" "):
    # debugecho word
    if word[0] == '(':
      result.add tok(rxkOp, "(")
      result.add tok(rxkIdent, word[1..^1]) # TODO check for correct
                                            # word
    elif word[^1] == ')':
      let braceEnd = word.rfind(AllChars - { ')' })
      # debugecho braceEnd
      if word[0] == '\"':
        result.add tok(rxkStrLit, word[1 .. braceEnd])
      else:
        result.add tok(rxkIdent, word[0..^2])

      for _ in (braceEnd + 1) .. (word.len - 1):
        result.add tok(rxkCl, ")")
    elif word[0] == '\"' and word[^1] == '\"':
      result.add tok(rxkStrLit, word[1..^2])
    else:
      result.add tok(rxkIdent, word)

const defaultCategory = rxkStrLit

initGrammarConst[RxTokKind, string](grammar):
  List ::= !op & ident & @*(Element) & !cl
  Element ::= List | ident | strlit | intlit

let parser {.compiletime.} =
  newLLStarParser[RxTokKind, string, void](grammar)


macro rx*(body: string): untyped =
  # echo body.treeRepr()
  var toks = tokenize(body.strVal()).makeStream()
  echo toks.exprRepr()
  let tree = parser.parse(toks)
  echo tree.treeRepr()

  let spec = tree.toRxSpec()

  result = quote: 2
