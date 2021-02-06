
import
  hparse / htreesitter / htreesitter, sequtils, strutils

type
  OtherTestNodeKind* = enum
    otherTestSourceFile,    ## source_file
    otherTestSingleLParTok, ## (
    otherTestSingleRParTok, ## )
    otherTestExternalToken, ## externalToken
    otherTestSyntaxError     ## Tree-sitter parser syntax error
type
  OtherTestExternalTok* = enum
    otherTestExternExternalToken ## externalToken
type
  OtherTestNode* = distinct TSNode
type
  OtherTestParser* = distinct PtsParser
proc tsNodeType*(node: OtherTestNode): string
proc kind*(node: OtherTestNode): OtherTestNodeKind =
  case node.tsNodeType
  of "source_file":
    otherTestSourceFile
  of "(":
    otherTestSingleLParTok
  of ")":
    otherTestSingleRParTok
  of "externalToken":
    otherTestExternalToken
  of "ERROR":
    otherTestSyntaxError
  else:
    raiseAssert("Invalid element name \'" & node.tsNodeType & "\'")

proc tree_sitter_otherTest(): PtsLanguage {.importc, cdecl.}
proc tsNodeType*(node: OtherTestNode): string =
  $ts_node_type(TSNode(node))

proc newOtherTestParser*(): OtherTestParser =
  result = OtherTestParser(ts_parser_new())
  discard ts_parser_set_language(PtsParser(result), tree_sitter_otherTest())

proc parseString*(parser: OtherTestParser; str: string): OtherTestNode =
  OtherTestNode(ts_tree_root_node(ts_parser_parse_string(PtsParser(parser), nil,
      str.cstring, uint32(len(str)))))

func `[]`*(node: OtherTestNode; idx: int; withUnnamed: bool = false): OtherTestNode =
  if withUnnamed:
    OtherTestNode(ts_node_child(TSNode(node), uint32(idx)))
  else:
    OtherTestNode(ts_node_named_child(TSNode(node), uint32(idx)))

func len*(node: OtherTestNode; withUnnamed: bool = false): int =
  if withUnnamed:
    int(ts_node_child_count(TSNode(node)))
  else:
    int(ts_node_named_child_count(TSNode(node)))

proc isNil*(node: OtherTestNode): bool =
  ts_node_is_null(TsNode(node))

iterator items*(node: OtherTestNode; withUnnamed: bool = false): OtherTestNode =
  for i in 0 .. node.len(withUnnamed):
    yield node[i, withUnnamed]

proc slice*(node: OtherTestNode): Slice[int] =
  ts_node_start_byte(TsNode(node)).int ..< ts_node_end_byte(TsNode(node)).int

proc treeRepr*(mainNode: OtherTestNode; instr: string; withUnnamed: bool = false): string =
  proc aux(node: OtherTestNode; level: int): seq[string] =
    if not(node.isNil()):
      result = @["  ".repeat(level) & ($node.kind())[9 ..^ 1]]
      if node.len(withUnnamed) == 0:
        result[0] &= " " & instr[node.slice()]
      for subn in items(node, withUnnamed):
        result.add subn.aux(level + 1)

  return aux(mainNode, 0).join("\n")
