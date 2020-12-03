import test_wrapper
import std/[macros, unicode]
import hnimast
# import hmisc/macros/matching
import htreesitter


macro tsInitScanner*(
  langname: untyped, scannerType, kindType: typed): untyped =
  result = newStmtList()


  let
    initCall = ident("init" & scannerType.repr)

  result.add quote do:
    var scanner {.inject.} = `initCall`()

  result.add newNProcDecl(
    name = "tree_sitter_" & langname.strVal() & "_external_scanner_create",
    rtyp = some newNType("pointer"),
    exported = false,
    pragma = newNPragma("exportc"),
    impl = (
      quote do:
        echo "called external scanner creation"
        result = addr scanner
    )
  ).toNNode()

  result.add newNProcDecl(
    name = "tree_sitter_" & langname.strVal() & "_external_scanner_destroy",
    args = { "inScanner" : newNtype(scannerType.repr) },
    exported = false,
    pragma = newNPragma("exportc"),
    impl = (
      quote do:
        discard
    )
  ).toNNode()
  
  result.add newNProcDecl(
    name = "tree_sitter_" & langname.strVal() & "_external_scanner_scan",
    args = {
      "payload" : newNtype("pointer"),
      "lexer" : newNType("ptr", @["TSLexer"]),
      "valid_symbols" : newNType("ptr", @["bool"])
    },
    exported = false,
    pragma = newNPragma("exportc"),
    impl = (
      quote do:
        scan(cast[ptr `scannerType`](payload)[], lexer[])
    )
  ).toNNode()


  result.add newNProcDecl(
    name = "tree_sitter_" & langname.strVal() & "_external_scanner_serialize",
    args = {
      "payload" : newNtype("pointer"),
      "buffer" : newNType("cstring"),
    },
    exported = false,
    pragma = newNPragma("exportc"),
    impl = (
      quote do:
        discard
    )
  ).toNNode()

  result.add newNProcDecl(
    name = "tree_sitter_" & langname.strVal() & "_external_scanner_deserialize",
    args = {
      "payload" : newNtype("pointer"),
      "buffer" : newNType("cstring"),
      "length" : newNType("cuint")
    },
    exported = false,
    pragma = newNPragma("exportc"),
    impl = (
      quote do:
        discard
    )
  ).toNNode()


  result = quote do:
    block:
      `result`
      scanner

  echo result.repr


type
  TestScanner = object

  TestToks = enum
    tk1

proc scan(scan: var TestScanner, tslex: var TsLexer) =
  echo "Called scan on ", $tslex.nextRune()
  discard

func initTestScanner(): TestScanner = discard

var scanner = tsInitScanner(test, TestScanner, TestToks)


# {.experimental: "caseStmtMacros".}

{.passl: "-lstdc++".}
{.passl: "-ltree-sitter".}
{.passl: "test_parser.o".}

var parser = newTestParser()

var str = """
hello
"""

let tree = parser.parseString(str)

let currStr: ptr string = addr str

echo tree.treeRepr(str)

proc strVal(node: TestNode): string =
  currStr[][node.slice()]
