import cppwrapper, macros

var parser = newCppParser()

let str = """
int a = 12;
"""

let tree = parser.parseString(str)

echo tree.treeRepr(str)
