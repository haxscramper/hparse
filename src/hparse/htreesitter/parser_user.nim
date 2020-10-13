import cppwrapper, macros
import hmisc/macros/matching

{.experimental: "caseStmtMacros".}

{.passl: "-lstdc++".}
{.passl: "-ltree-sitter".}
{.passl: "cppscanner.o".}
{.passl: "cppparser.o".}

var parser = newCppParser()

let str = """
int a = 12;
"""

let tree = parser.parseString(str)

echo tree.treeRepr(str)

proc strVal(node: CppNode, str: string): string =
  str[node.slice()]

case tree[0]:
  of Declaration[@dtype, .._]:
    echo "first is declaration with type ", dtype.strVal(str)
