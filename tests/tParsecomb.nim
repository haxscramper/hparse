import hparse/parsecomb
import macros
import hmisc/helpers
import sugar
import options
import sequtils
import strutils
import regex



if false:
  let cat = parseString("cat")
  let dog = parseString("dog")

  echo parseOr(@[cat, dog])("cat", 0)
  echo parseOr(@[cat, dog])("dog", 0)
  echo parseAnd(@[cat, dog])("catdog", 0)

static:
  if false:
    let nl = parseRx(re(r"~(\d?)%"))
    echo nl("~%")
    echo nl("~8%")

static:
  if false:
    var m: RegexMatch
    discard find("**~90%", re r"~(\d+)?%", m, 1)
    echo m

if false:
  let nhashes = parseNTimes(parseString("#"), 2, 8)
  echo nhashes("#####")

  let nnewlines = parseZeroOrMore(parseOr(@[
    parseAnd(@[parseString("~"), parseString("%")]),
    parseAnd(@[parseString("$$")])
  ]))

  echo nnewlines("~%~%$$~%")

if false:
  let anyof = parseAnyOf(@["4", "ekk"])
  echo anyof("412")

if false:
  let flt = parseOr(@[
    parseString("&&&"),
    flatten(parseZeroOrMore(parseString("(")))
   ])

  echo flt("&&&")
  echo flt("((((()))))")

type

  FormatCommand = enum
    fcRadixCardinal
    fcRadixOrdinal
    fcRoman
    fcRomanOld

  PrefixArgs = enum
    paInteger
    paString

  FormatSpecifier = object
    kind: FormatCommand
    args: seq[(PrefixArgs, string)]

  FormatTok = object
    case command: bool
      of true:
        spec: string
        args: seq[string]
      of false:
        content: string

  FormatAst = object
    args: seq[string]
    children: seq[FormatAst]

proc toFormatTok(s: seq[string]): FormatTok =
  if s.len == 1:
    return FormatTok(command: false, content: s[0])
  elif s.len == 3 and s[0] == "~":
    return FormatTok(
      command: true,
      spec: s[2]
    )

proc mks(
  kind: seq[FormatCommand],
  args: varargs[(PrefixArgs, string)]
     ): seq[FormatSpecifier] =
  for k in kind:
    result &= FormatSpecifier(kind: k, args: toSeq(args))


let controls = @[(
    endstr: @[
      (fcRadixOrdinal, "R"),
      (fcRadixCardinal, ":R"),
      (fcRoman, "@R"),
      (fcRomanOld, ":@R")
    ],
   args: @[
     (paInteger, "radix"),
     (paInteger, "mincol"),
     (paString, "padchar"),
     (paString, "commachar"),
     (paInteger, "comma-interval")
])]

proc parseFormatSpec(
  buffer: string, position: int): Option[(FormatSpecifier, int)] =
  if buffer[position] == '~':
    for ctl in controls:
      for ends in ctl.endstr:
        let pos = buffer.find(ends[1], start = position)
        if pos != 0:
          let args = buffer[position + 1 ..< pos - 1]
          return some((FormatSpecifier(
            kind: ends[0],
            args: args.split(",").enumerate().mapIt(
              (ctl.args[it[0]][0], it[1]))
          ), pos + len(ends[1])))

echo parseFormatSpec("~8,12@R", 0)


let skipUntilFormat = parseSkipUntil[char, string]('~').sequentize()
let matchFormatCtrl = parseAnd(@[
  parseString("~"),
  parseAnyOf(@[
    "C", "%", "&", "|", "~", "R", "D", "B", "O", "X",
    "F", "E", "G", "$", "A", "S", "W", "_", "<", ";",
    "@", "^", ":>", "I", "/", "T", "<", ";", ":;", "^",
    ">", "*", "[", ";", ":;", ":[", "]", "@["
  ])
])

let matchTokens = parseNTimes(parseOr(@[
  skipUntilFormat, matchFormatCtrl
]).parseMap(toFormatTok), maxtimes = 10)

echo matchTokens("Heloo ~% world ~%")
