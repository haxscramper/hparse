import test_wrapper, macros
# import hmisc/macros/matching
import htreesitter

{.experimental: "caseStmtMacros".}

{.passl: "-lstdc++".}
{.passl: "-ltree-sitter".}
# {.passl: "cpp_scanner.o".}
# {.passl: "cpp_parser.o".}

var parser = newTestParser()

var str = """
hello
"""

let tree = parser.parseString(str)

let currStr: ptr string = addr str

echo tree.treeRepr(str)

proc strVal(node: TestNode): string =
  currStr[][node.slice()]
