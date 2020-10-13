import toml_parser, macros

var parser = newTomlParser()

let str = """
# This is a TOML document.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00-08:00 # First class dates
"""

let tree = parser.parseString(str)

echo tree.treeRepr(str)
