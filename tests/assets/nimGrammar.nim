{.passl: "-ltree-sitter".}
{.passl: "otherTest_parser.o".}

import otherTest_wrapper
import std/[unicode]
import hparse/htreesitter/htreesitter

type
  TestScanner = object

proc scan(scan: var TestScanner, tslex: var TsLexer): Option[OtherTestExternalTok] =
  tslex.advance(false)
  tslex.markEnd()
  if not tslex.finished():
    return some(otherTestExternExternalToken)

func initTestScanner(): TestScanner = discard

var
  scanner = tsInitScanner(otherTest, TestScanner)
  parser = newOtherTestParser()
  str = "(123)"

echo parser.parseString(str).treeRepr(str)
