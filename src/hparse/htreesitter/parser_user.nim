{.passl: "-lstdc++".}
{.passl: "-ltree-sitter".}
{.passl: "test_parser.o".}

import test_wrapper
import std/[unicode]
import htreesitter

type
  TestScanner = object

proc scan(scan: var TestScanner, tslex: var TsLexer): Option[TestExternalTok] =
  tslex.advance(false)
  tslex.markEnd()
  if not tslex.finished():
    return some(testExternNimExternal)

func initTestScanner(): TestScanner = discard

var scanner = tsInitScanner(test, TestScanner)

var parser = newTestParser()

var str = """
hello
"""

let tree = parser.parseString(str)

echo tree.treeRepr(str)
