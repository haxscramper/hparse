# To support lexing in both compiled and interpreted environments as
# well as at compile-time
import regex, token, options
import streams, strformat, sequtils

type
  Matcher[Tok] = object
    ## Token matcher. `Tok` is a type of returned Token.
    case cbMatch: bool
      of true:
        matchImpl: proc(inStream: Stream): tuple[
          ok: bool, tok: Tok, pos: int] ## Callback proc to match
        ## token. Starting position and other parts of lexer state are
        ## stored in the `inStream`. On failure proc must return
        ## starting position in `pos` and `ok = false`.
      of false:
        patt: Regex
        # classify: proc(match: string, ruleId: int): Token[C, ] ## Classify
        # ## matched token and convert it to string.


  Lexer[Tok] = object
    ## Data stream lexer. `Tok` is a type of returned tokens.
    stream*: Stream ## Input data stream
    # Lexer state is implicitly stored in the stream (position and
    # buffer)
    matchers*: seq[Matcher[Tok]]

  CbProc*[Tok] = proc(tok: Tok, currPos: int)

  TokStream*[Tok] = object
    ## Buffer for tokens. Modeled after `std/streams` implementation
    nextTokCb: CbProc[Tok]
    currPos: int ## Current position in buffer
    buffer: seq[Tok] ## Token buffer
    newTok: proc(): tuple[stop: bool, tok: Tok] ## Callback to get new
    ## tokens. To indicate final token return `stop = true`
    atEnd: bool

func absPos*[Tok](ts: TokStream[Tok]): int =
  ## Absolute current position in token stream
  ts.currPos + 1

func revertTo*[Tok](ts: var TokStream[Tok], pos: int): void =
  ## Revert to position `pos`
  ts.currPos = pos - 1

proc next*[Tok](ts: var TokStream[Tok]): Tok =
  ## Create single token by either parsing new data or returning from
  ## buffer
  # echo "Getting next token"
  if ts.currPos < ts.buffer.len - 1:
    inc ts.currPos
    if ts.nextTokCb != nil:
      when (not nimvm): # FIXME still triggers on compiletime
        when not defined(nimscript): # FIXME still triggers on compiletime
          # FIXME fails on compiletime
          # ts.nextTokCb(ts.buffer[ts.currPos], ts.currPos)
          discard

      # when not defined(nimscript):
      # else:
      #   discard
    #   echo "done ..."
    # echo "Returning from buffer"
    return ts.buffer[ts.currPos]
  else:
    if ts.atEnd:
      raiseAssert("Cannot read from finished token stream. " &
      & "Current position: {ts.currPos}, buffer size: {ts.buffer.len}")

    else:
      # Assuming _if_ token stream not atEnd _then_ it can read at
      # least one more token.
      let (stop, tok) = ts.newTok()
      if stop:
        ts.atEnd = true

      ts.buffer.add tok
      inc ts.currPos
      if ts.nextTokCb != nil: ts.nextTokCb(tok, ts.currPos)
      return tok

proc nextTry*[Tok](ts: var TokStream[Tok]): Option[Tok] =
  if not (ts.currPos < ts.buffer.len - 1) and ts.atEnd:
    none(Tok)
  else:
    some(ts.next())


func makeStream*[Tok](
  tokens: seq[Tok], nextTokCb: CbProc[Tok] = nil): TokStream[Tok] =
  var tokens = tokens
  for idx in 0 ..< tokens.len:
    setPosInfo(tokens[idx], idx)

  TokStream[Tok](
    buffer: tokens,
    newTok: proc(): tuple[stop: bool, tok: Tok] = (result.stop = true),
    atEnd: true,
    currPos: -1,
    nextTokCb: nextTokCb
  )

func getBuffer*[Tok](toks: TokStream[Tok]): seq[Tok] =
  toks.buffer

func finished*[Tok](toks: TokStream[Tok]): bool =
  toks.atEnd and toks.currPos == toks.buffer.len - 1

proc move*[Tok](ts: var TokStream[Tok], shift: int = -1): void =
  ts.currPos = ts.currPos + shift

proc peek*[Tok](ts: var TokStream[Tok]): Tok =
  ## Get next token from token stream without changing position
  let next = ts.next()
  ts.move(-1)
  return next

func `[]`*[Tok](ts: TokStream[Tok], pos: int | uint): Tok =
  ts.buffer[pos.int]

iterator items*[Tok](ts: var TokStream[Tok]): Tok =
  ## Iterate over tokems in tokens stream. New parsing is done only
  ## when buffer is reached.
  while not ts.finished():
    yield ts.next()

proc reset*[Tok](ts: var TokStream[Tok]): Tok =
  ## Reset token stream internal state - clear buffer, set position to
  ## 0 etc.
  # REVIEW why return anything?
  ts.buffer = @[]
  ts.atEnd = false
  ts.currPos = 0

func exprRepr*[Tok](ts: TokStream[Tok]): string =
  if ts.buffer.len < 4:
    "@" & $ts.currPos & " [" &
      ts.buffer.mapIt(it.exprRepr()).join(", ") & "]"
  else:
    ts.buffer.mapIt(it.exprRepr()).join("\n")
