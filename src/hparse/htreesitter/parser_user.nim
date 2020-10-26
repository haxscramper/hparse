import cppwrapper, macros
import hmisc/macros/matching
import htreesitter

{.experimental: "caseStmtMacros".}

{.passl: "-lstdc++".}
{.passl: "-ltree-sitter".}
{.passl: "cppscanner.o".}
{.passl: "cppparser.o".}

var parser = newCppParser()

var str = """
int a = 12;
"""

var currCppStr: ptr string

let tree = parser.parseString(str)

currCppStr = addr str

echo tree.treeRepr(str)

proc strVal(node: CppNode): string =
  currCppStr[][node.slice()]

case tree[0]:
  of Declaration[@dtype, .._]:
    echo "first is declaration with type ", dtype.strVal()
    echo cast[int](TsNode(dtype).id)
