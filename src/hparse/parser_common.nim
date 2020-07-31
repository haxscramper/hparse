import sugar, strutils, sequtils
import macros

type
  ParserError* = ref object of CatchableError
    pos*: LineInfo

  SyntaxError* = ref object of ParserError

  LexerError* = ref object of ParserError
