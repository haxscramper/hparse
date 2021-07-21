import
  hparse/htreesitter/hts_wrapgen,
  hmisc/other/[oswrap, hargparse]

import
  std/[options, unittest, strformat]

let
  dir = getAppTempDir()
  inDir = currentSourceDir() / "assets"
  user = dir /. "user.nim"

mkDir dir

user.writeFile("""
import ./wrapper
{.compile: "parser.c".}
{.passl: "-ltree-sitter".}

import
  hmisc/wrappers/treesitter,
  hmisc/hdebug_misc

startHax()

proc treeRepr(node: Simple1Node, base: string): string =
  treeRepr[Simple1Node, Simple1NodeKind](node, base, 8)

proc test(str: string) =
  let node = str.parseSimple1String()
  echov node.treeRepr(str)
  echov node[0].treeRepr(str)
  echov node[1].treeRepr(str)
  if "optional_field1" in node:
    echov node["optional_field1"].treeRepr(str)

  if "optional_field2" in node:
    echov node["optional_field2"].treeRepr(str)

  echov node["non_optional_field"].treeRepr(str)

echov "----"
test("ident11 ident12 ident2 ident3")
echov "----"
test("ident12 ident2 ident3")
echov "----"
test("ident11 ident2 ident3")
echov "----"
test("ident2 ident3")
""")

suite "Using proc API":
  test "Using proc API":
    grammarFromFile(
      inDir /. "optional_pos.js",
      parserOut  = some(dir /. "parser.c"),
      wrapperOut = some(dir /. "wrapper.nim"),
      parseruser = some(user),
      forceBuild = true)

suite "Using shell API":
  test "Help":
    echo newApp().helpStr(cliNoDefaultOpts)

  test "Using shell API":
    main(@[
      "grammarFromFile",
      $(inDir /. "optional_pos.js"),
      &"--parserOut='{dir /. \"parser.c\"}'",
      &"--wrapperOut='{dir /. \"wrapper.nim\"}'",
      &"--parserUser='{user}'",
      "--forceBuild=yes"])
