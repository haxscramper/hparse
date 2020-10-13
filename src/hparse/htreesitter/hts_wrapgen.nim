import macros, options, sequtils, strutils, tables, logging, sets
import compiler/ast

import hmisc/[hexceptions, hdebug_misc]
import hmisc/other/[hjson, hshell, oswrap, colorlogger]
import hmisc/algo/[clformat, halgorithm, hstring_algo]
import hmisc/macros/matching

import hpprint, hnimast

import htreesitter


template mapItSome*[T](opt: Option[T], expr: untyped): untyped =
  type ResT = typeof((var it {.inject.}: typeof(opt.get()); expr))
  var res: Option[ResT]
  if opt.isSome():
    let it {.inject.} = opt.get()
    res = some(expr)

  res


{.experimental: "caseStmtMacros".}

type
  TreeChildren = object
    multiple: bool
    required: bool
    types: seq[tuple[ttype: string, named: bool]]

  Tree = object
    ttype: string
    named: bool
    children: Option[TreeChildren]

  NodeSpec = seq[Tree]



func id(str: string): PNode = newPident(str)
proc lit(arg: string | int): PNode = newPLit(arg)



func toTree(js: JsonNode): Tree =
  result = Tree(
    ttype: js["type"].asStr(),
    named: js["named"].asBool()
  )

  if js.matches({"children": @ch}):
    result.children = some TreeChildren(
      multiple: ch["multiple"].asBool(),
      required: ch["required"].asBool(),
      types: ch["types"].mapIt((
        ttype: it["type"].asStr(),
        named: it["named"].asBool())))


func toNtermName(str: string): string =
  if str.validIdentifier():
    str.splitCamel().joinCamel()
  else:
    str.toNamedMulticharJoin()

func ntermName(elem: Tree, lang: string): string =
  result = lang & elem.ttype.toNtermName().capitalizeAscii()
  if not elem.named:
    result &= "Tok"

func makeNodeName(lang: string): string = lang.capitalizeAscii() & "Node"
func makeNodeKindName(lang: string): string = lang.makeNodeName() & "Kind"



func makeKindEnum(spec: NodeSpec, lang: string): PEnum =
  result = PEnum(name: lang.makeNodeKindName(), exported: true)
  var used: HashSet[string]
  for elem in spec:
    let name = elem.ntermName(lang)
    if name notin used: # WARNING
      used.incl name
      result.values.add makeEnumField[PNode](name, comment = elem.ttype)


func makeGetKind(spec: NodeSpec, lang: string): ProcDecl[PNode] =
  result = newPProcDecl(
    "kind",
    {"node" : newPType(lang.makeNodeName())},
    some newPType(lang.makeNodeKindName())
  )

  let nameGet = makeTree[PNode](
    DotExpr[== id("node"), == id("tsNodeType")])
  var impl = makeTree[PNode](CaseStmt[== nameGet])

  var used: HashSet[string]
  for elem in spec:
    let name = elem.ttype
    if name notin used:
      used.incl name
      impl.add makeTree[PNode](OfBranch[
          == newPLit(name),
          == newPIdent(elem.ntermName(lang))
      ])

  let assrt = pquote do:
    raiseAssert("Invalid element name '" & `nameGet` & "'")

  impl.add makeTree[PNode](Else[== assrt])

  result.impl = impl

func camelCase(str: varargs[string, `$`]): string =
  str.joinCamel()

func pascalCase(str: varargs[string, `$`]): string =
  result = str.joinCamel()
  result[0] = result[0].toUpperAscii()

func makeLangParserName(lang: string): string =
  pascalCase(lang, "parser")


func makeImplTsFor(lang: string): PNode =
  result = nnkStmtList.newPTree()
  let
    parser: PNode = lang.makeLangParserName().newPType().toNNode()
    nodeType: PNode = lang.makeNodeName().newPType().toNNode()
    langLen: PNode = newPLit(lang.len)
    langImpl: PNode = newPIdent("tree_sitter_" & lang)
    newParserID: PNode = newPIdent("new" & lang.makeLangParserName())

  result.add pquote do:
    proc `langImpl`(): PtsLanguage {.importc, cdecl.}
    proc tsNodeType*(node: `nodeType`): string =
      $ts_node_type(TSNode(node))

    proc `newParserID`*(): `parser` =
      result = `parser`(ts_parser_new())
      discard ts_parser_set_language(PtsParser(result), `langImpl`())

    proc parseString*(parser: `parser`; str: string): `nodeType` =
      `nodeType`(
        ts_tree_root_node(
          ts_parser_parse_string(
            PtsParser(parser), nil, str.cstring, uint32(len(str)))))

  startHaxComp()
  result.add pquote do:
    func `[]`*(node: `nodeType`,
               idx: int, withUnnamed: bool = false): `nodeType` =
      if withUnnamed:
        `nodeType`(ts_node_child(TSNode(node), uint32(idx)))
      else:
        `nodeType`(ts_node_named_child(TSNode(node), uint32(idx)))

    func len*(node: `nodeType`, withUnnamed: bool = false): int =
      if withUnnamed:
        int(ts_node_child_count(TSNode(node)))
      else:
        int(ts_node_named_child_count(TSNode(node)))

    proc isNil*(node: `nodeType`): bool =
      ts_node_is_null(TsNode(node))

    iterator items*(node: `nodeType`,
                   withUnnamed: bool = false): `nodeType` =
      for i in 0 .. node.len(withUnnamed):
        yield node[i, withUnnamed]

    proc slice*(node: `nodeType`): Slice[int] =
      ts_node_start_byte(TsNode(node)).int ..<
      ts_node_end_byte(TsNode(node)).int

    proc treeRepr*(mainNode: `nodeType`,
                   instr: string,
                   withUnnamed: bool = false): string =
      proc aux(node: `nodeType`, level: int): seq[string] =
        if not(node.isNil()):
          result = @["  ".repeat(level) & ($node.kind())[`langLen`..^1]]
          if node.len(withUnnamed) == 0:
            result[0] &= " " & instr[node.slice()]

          for subn in items(node, withUnnamed):
            result.add subn.aux(level + 1)

      return aux(mainNode, 0).join("\n")



func makeDistinctType*(baseType, aliasType: NType[PNode]): PNode =
  let
    aliasType = aliasType.toNNode()
    baseType = baseType.toNNode()

  pquote do:
    type `aliasType`* = distinct `baseType`


proc createProcDefinitions(spec: NodeSpec, inputLang: string): PNode =
  result = nnkStmtList.newPTree()

  result.add pquote do:
    import hparse/htreesitter/htreesitter, sequtils, strutils

  result.add makeKindEnum(spec, inputLang).toNNode(standalone = true)
  result.add makeDistinctType(
    newPType("TSNode"),
    newPType(makeNodeName(inputLang)))

  result.add makeDistinctType(
    newPType("PtsParser"),
    newPType(makeLangParserName(inputLang))
  )

  let langId = newPident makeNodeName(inputLang)

  result.add pquote do:
    proc tsNodeType*(node: `langId`): string

  result.add makeGetKind(spec, inputLang).toNNode()
  result.add makeImplTsFor(inputLang)

proc compileGrammar(
  grammarJs: AbsFile,
  scannerFile: Option[AbsFile] = none(AbsFile),
  parserOut: AbsFile,
  wrapperOut: AbsFile,
  scannerOut: Option[AbsFile] = none(AbsFile),
  junkDir: AbsDir) =

  mkDir junkDir
  withDir junkDir:
    cpFile grammarJs, "grammar.js", lvlInfo

    execShell("npm link regexp-util")
    info "Linked regexp-util BS for node.js"

    info "Generating tree-sitter files"
    execShell makeGnuCmd("tree-sitter").withIt do:
      it.cmd "generate"

    debug "Done"

    if Some(@file) ?= scannerFile:
      cpFile file, "src/scanner.c", lvlInfo

    var spec = "src/node-types.json".
      parseFile().getElems().mapIt(it.toTree())

    let grammar = "src/grammar.json".parseFile()

    for extra in grammar["extras"]:
      if extra.matches({"name": @name}):
        spec.add Tree(ttype: name.asStr(), named: true)


    let inputLang: string = grammar["name"].asStr()
    wrapperOut.writeFile($createProcDefinitions(spec, inputLang))
    info "Wrote generated wrappers to", wrapperOut

    debug "Linking parser.c"
    execShell makeGnuCmd("clang").withIt do:
      it.arg "src/parser.c"
      it - "c"
      it - ("o", "", "parser.o")

      cpFile "parser.o", parserOut, lvlNotice

    if Some(@file) ?= scannerFile:
      debug "Linking", file
      execShell makeGnuCmd("clang").withIt do:
        it.arg $file
        it - "c"
        it - ("o", "", "scanner.o")

      cpFile "scanner.o", scannerOut.get(), lvlNotice





    info "tree-sitter object files generation ok"

let (args, opts) = paramStrs().splitCmdLine()

args.assertMatch([
  opt @grammarJs or "grammar.js",
  opt @scannerFile,
  opt @parserUser,
  .._])

scannerFile = some("scanner.cc")
parserUser = some("parser_user.nim")

if scannerFile.isNone():
  warn "No input scanner file"

let lang = "cpp"

startColorLogger()

compileGrammar(
  grammarJs = grammarJs.toAbsFile(true),
  scannerFile = scannerFile.mapItSome(it.toAbsFile(true)),
  parserOut = cwd() /. (lang & "parser.o"),
  wrapperOut = cwd() /. (lang & "wrapper.nim"),
  scannerOut = some(cwd() /. (lang & "scanner.o")),
  junkDir = (AbsDir("/tmp") / lang)
)


rmDir "cache.d"
let user = "parser_user.nim"
try:
  let (stdout, stderr, code) = runShell makeNimCmd("nim").withIt do:
    it.subCmd "c"
    it - "r"
    it - ("nimcache", "cache.d")
    it - ("forceBuild", "on")
    for file in ["parser", "scanner"]:
      # Link parser and external scanners
      it - ("passL", lang & file & ".o")

    it - ("passL", "-lstdc++")

    # Link tree-sitter
    it - ("passL", "-ltree-sitter")

    it.arg user

  echo stdout
  echo stderr
except ShellError:
  let ex = getCEx(ShellError)
  echo ex.outstr
  echo ex.msg
  for line in ex.errstr.split("\n"):
    if line.contains(["undefined reference"]):
      if line.contains("external"):
        once: err "Missing linking with external scanners"
        info line.split(" ")[^1][1..^2]
      elif line.contains("ts_"):
        once: err "Missing linking with tree-sitter library"
        info line
      elif line.contains("std::"):
        once: err "Missing linking with C++ stdlib"
        info line
      else:
        once: err "Missing linking with other library"
        info line

    elif line.contains(["/bin/ld", "ld returned"]):
      discard
    else:
      echo line
