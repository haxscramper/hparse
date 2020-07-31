## Parser combinator librariy

import options
import regex
import sugar

type
  ParseResult*[Val] = object
    mark: string
    case success*: bool
      of true:
        value*: Val
        endpos*: int
      of false:
        error*: string

  Callbacks* = object
    entry*: proc(pos: int)
    success*: proc(pos: int)
    failure*: proc(pos: int)

const noCallbacks* = Callbacks(
  entry: proc(p: int) = discard,
  success: proc(p: int) = discard,
  failure: proc(p: int) = discard
)

type
  Parser*[Val, Buf] = proc(buffer: Buf, position: int = 0): ParseResult[Val]

proc parseString*(str: string, cb: Callbacks = noCallbacks): Parser[string, string] =
  return proc(buffer: string, position: int = 0): ParseResult[string] =
    if position + str.len > buffer.len:
      ParseResult[string](
        success: false,
        error: "End position reached"
      )
    elif buffer[position ..< position + str.len] == str:
      ParseResult[string](
        success: true,
        value: str,
        endpos: position + str.len
      )
    else:
      ParseResult[string](
        success: false,
        error: "Cannot find string"
      )

proc parseRx*(rx: Regex, cb: Callbacks = noCallbacks): Parser[string, string] =
  return proc(buffer: string, startpos: int = 0): ParseResult[string] =
    if not cb.entry.isNil(): cb.entry(startpos)

    var m: RegexMatch
    if find(buffer, rx, m, startpos) and m.boundaries.a == startpos:
      ParseResult[string](
        success: true,
        endpos: m.boundaries.b + 1,
        value: buffer[m.boundaries.a .. m.boundaries.b],
        mark: "rx"
      )
    else:
      ParseResult[string](
        success: false
      )

proc parseValue*[Val, Buf](val: Val, cb: Callbacks = noCallbacks): Parser[Val, Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[Val] =
    if buffer[position] == val:
      return ParseResult(success: true, value: val, mark: "equal")

proc parseValue*[Val](val: seq[Val], cb: Callbacks = noCallbacks): Parser[Val, seq[Val]] =
  return proc(buffer: seq[Val], position: int = 0): ParseResult[Val] =
    for i in 0 ..< val.len:
      if buffer[position + i] != val[i]:
        return ParseResult(success: false)

    return ParseResult(success: true, value: buffer[position .. position + val.len], mark: "arr eql")

proc parseOr*[Val, Buf](args: seq[Parser[Val, Buf]], cb: Callbacks = noCallbacks): Parser[Val, Buf] =
  return proc(buffer: string, position: int = 0): ParseResult[Val] =
    for parser in args:
      let res = parser(buffer, position)
      if res.success:
        return res

    return ParseResult[Val](
      success: false,
      error: "None of the parsers match"
    )

proc parseAnyOf*[Val, Buf](args: seq[Val], cb: Callbacks = noCallbacks): Parser[Val, Buf] =
  let parsers = collect(newSeq):
    for arg in args:
      parseValue(arg)

  return parseOr(parsers, cb = cb)

proc parseAnyOf*(args: seq[string], cb: Callbacks = noCallbacks): Parser[string, string] =
  let parsers = collect(newSeq):
    for arg in args:
      parseString(arg)

  return parseOr(parsers, cb = cb)

proc parseAnd*[Val, Buf](args: seq[Parser[Val, Buf]], cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return proc(buffer: string, position: int = 0): ParseResult[seq[Val]] =
    var posNow = position
    var resVals: seq[Val]
    for parser in args:
      let res = parser(buffer, posNow)
      if res.success:
        resVals.add res.value
        posNow = res.endpos
      else:
        return ParseResult[seq[Val]](
          success: false,
          error: res.error
        )

    return ParseResult[seq[Val]](
      success: true,
      value: resVals,
      endpos: posNow
    )

proc flatten*[Val, Buf](parser: Parser[seq[Val], Buf], cb: Callbacks = noCallbacks): Parser[Val, Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[Val] =
    let res = parser(buffer, position)
    if res.success:
      var resSeq: Val
      for item in res.value:
        resSeq &= item

      return ParseResult[Val](success: true, value: resSeq, endpos: res.endpos)
    else:
      return ParseResult[Val](success: false)

proc sequentize*[Val, Buf](parser: Parser[Val, Buf], cb: Callbacks =  noCallbacks): Parser[seq[Val], Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[seq[Val]] =
    let res = parser(buffer, position)
    if res.success:
      return ParseResult[seq[Val]](
        success: true,
        value: @[res.value],
        endpos: res.endpos
      )
    else:
      return ParseResult[seq[Val]](
        success: false
      )

proc flatten*[Val, Buf](parsers: seq[Parser[Val, Buf]], cb: Callbacks = noCallbacks): Parser[Val, Buf] =
  let parser = parseAnd(parsers, cb = cb)
  return proc(buffer: Buf, position: int = 0): ParseResult[Val] =
    let res = parser(buffer, position)
    if res.success:
      var resSeq: Val
      for item in res.value:
        resSeq &= item

      return ParseResult[Val](success: true, value: resSeq, endpos: res.endpos)
    else:
      return ParseResult[Val](success: false)

proc parseOpt*[Val, Buf](parser: Parser[Val, Buf], cb: Callbacks = noCallbacks): Parser[Option[Val], Buf] =
  return proc(buffer: string, position: int = 0): ParseResult[Option[Val]] =
    let res = parser(buffer, position)
    if res.success:
      return ParseResult(
        success: true,
        value: some(res.value),
        endpos: res.endpos
      )
    else:
      return ParseResult(
        success: true,
        value: none(Val),
        endpos: position
      )

proc makeParseFromOpt*[Val, Buf](
  parser: proc(buf: Buf, pos: int = 0): Option[tuple[val: Val, pos: int]],
  cb: Callbacks = noCallbacks): Parser[Val, Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[Val] =
    let res = parser(buffer, position)
    if res[0].isSome():
      return ParseResult[Val](
        endpos: res[1],
        value: res[0].get(),
        success: true,
      )
    else:
      return ParseResult[Val](
        succes: false
      )

proc parseNTimes*[Val, Buf](
  parser: Parser[Val, Buf],
  mintimes: int = 0,
  maxtimes: int = high(int),
  cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[seq[Val]] =
    var resSeq: seq[Val]
    var posNow = position
    for i in 0 .. maxtimes:
      let res = parser(buffer, posNow)
      if res.success:
        resSeq.add res.value
        posNow = res.endpos
      else:
        if i < mintimes:
          return ParseResult[seq[Val]](success: false, error: res.error)
        elif mintimes <= i:
          return ParseResult[seq[Val]](success: true, value: resSeq, endpos: posNow)

proc parseOneOrMore*[Val, Buf](parser: Parser[Val, Buf], cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return parseNTimes(parser, mintimes = 1, cb = cb)

proc parseZeroOrMore*[Val, Buf](parser: Parser[Val, Buf], cb: Callbacks = noCallbacks): Parser[seq[Val], Buf] =
  return parseNTimes(parser, cb = cb)

proc parseSkipUntil*[Val, Buf](value: Val, cb: Callbacks = noCallbacks): Parser[Buf, Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[Buf] =
    var i = position
    while true:
      if (i == buffer.len) or (buffer[i] == value):
        if i == position:
          return ParseResult[Buf](success: false, error: "No tokens matched", mark: "skip")
        else:
          return ParseResult[Buf](
            success: true,
            endpos: i,
            value: buffer[position ..< i],
            mark: "skip"
          )

      inc i

proc parseMap*[Val, Buf, Res](
  parser: Parser[Val, Buf],
  convert: proc(arg: Val): Res,
  cb: Callbacks = noCallbacks): Parser[Res, Buf] =
  return proc(buffer: Buf, position: int = 0): ParseResult[Res] =
    let res = parser(buffer, position)
    if res.success:
      return ParseResult[Res](
        success: true,
        value: convert(res.value),
        endpos: res.endpos,
      )

    else:
      return ParseResult[Res](
        success: false
      )
