import unittest, sequtils

import hparse/lexer
import hmisc/helpers


type
  TokenKind = enum
    tkOpBrace
    tkCloseBrace
    tkIdent
    tkComma

  Token = object
    kind: TokenKind

suite "Token streams":
  template newtoks(): untyped =
    var ts {.inject.} = makeStream(@[
      Token(kind: tkOpBrace),
      Token(kind: tkIdent),
      Token(kind: tkComma),
      Token(kind: tkIdent),
      Token(kind: tkCloseBrace)
    ])


  test "Get all tokens":
    newtoks()
    assertEq toSeq(ts).mapIt(it.kind), @[
      tkOpBrace, tkIdent, tkComma, tkIdent, tkCloseBrace
    ]
