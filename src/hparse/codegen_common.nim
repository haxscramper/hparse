import parse_primitives
import strutils, strformat, macros

proc makeParserName*(nterm: NTermSym): string =
  ## Converter nonterminal name into parsing proc name
  "parse" & nterm.capitalizeAscii()

proc makeSetLiteral*[T](s: set[T]): NimNode =
  ## Create new set literal
  result = nnkCurly.newTree()
  for elem in s:
    result.add ident($elem)

func makeIds*[C, L](): tuple[cId, lId, iId: NimNode] =
  (
    cId: ident($(typeof C)),
    lId: ident($(typeof L)),
    iId: ident("I")
  )
