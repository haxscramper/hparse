import std/[macros, options, sequtils, strutils,
            tables, sets, sha1, uri]
import compiler/ast

import hmisc/[hexceptions, hdebug_misc]
import hmisc/other/[hjson, hshell, oswrap, colorlogger, hcligen]
import hmisc/algo/[clformat, halgorithm, hstring_algo]
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

  NodeSpec = object
    nodes: seq[Tree]
    externals: seq[string]



func id(str: string): PNode = newPident(str)
proc lit(arg: string | int): PNode = newPLit(arg)



func toTree(js: JsonNode): Tree =
  result = Tree(
    ttype: js["type"].asStr(),
    named: js["named"].asBool()
  )

  if "children" in js:
    let ch = js["children"]
    result.children = some TreeChildren(
      multiple: ch["multiple"].asBool(),
      required: ch["required"].asBool(),
      types: ch["types"].mapIt((
        ttype: it["type"].asStr(),
        named: it["named"].asBool())))


func toNtermName(str: string): string =
  if str.validIdentifier() and str != "_":
    str.splitCamel().joinCamel()
  else:
    str.toNamedMulticharJoin()

func getName(
    names: CountTable[string],
    name: string,
    minCount: int = 0
  ): string =

  if names[name.normalize()] > minCount:
    return name & $(names[name.normalize()] + 1 - minCount)

  else:
    return name

func ntermName(elem: Tree, lang: string): string =
  result = lang & elem.ttype.toNtermName().capitalizeAscii()
  if not elem.named:
    result &= "Tok"

func makeNodeName(lang: string): string = lang.capitalizeAscii() & "Node"
func makeNodeKindName(lang: string): string = lang.makeNodeName() & "Kind"
func langErrorName(lang: string): string =
  lang & "SyntaxError"


proc makeKindEnum(
    spec: NodeSpec,
    lang: string,
    names: var CountTable[string]
  ): PEnumDecl =

  result = PEnumDecl(name: lang.makeNodeKindName(), exported: true)
  for elem in spec.nodes:
    let name = elem.ntermName(lang)
    let newName = names.getName(name)
    if name.normalize() in names:
      warn "Name clash for", name
      debug "Normalized form:", name.normalize(),
       "has count of", names[name.normalize()]

    result.values.add makeEnumField[PNode](
      newName, comment = elem.ttype)

    # info name.normalize()
    names.inc name.normalize()

  result.values.add makeEnumField[PNode](
    lang.langErrorName(),
    comment = "Tree-sitter parser syntax error"
  )


func makeGetKind(
    spec: NodeSpec,
    lang: string,
    names: CountTable[string]
  ): ProcDecl[PNode] =

  result = newPProcDecl(
    "kind",
    {"node" : newPType(lang.makeNodeName())},
    some newPType(lang.makeNodeKindName()),
    pragma = newPPragma("noSideEffect")
  )

  let nameGet = nnkDotExpr.newPTree("node".id, "tsNodeType".id)
  var impl = nnkCaseStmt.newPTree(nameGet)

  var used: HashSet[string]
  for elem in spec.nodes:
    let name = elem.ttype
    if name notin used:
      used.incl name
      impl.add nnkOfBranch.newPTree(
        newPLit(name),
        newPIdent(names.getName(
        elem.ntermName(lang), 1))
      )

  impl.add nnkOfBranch.newPTree(
    newPLit("ERROR"),
    newPIdent(lang & "SyntaxError")
  )

  let assrt = pquote do:
    raiseAssert("Invalid element name '" & `nameGet` & "'")

  impl.add nnkElse.newPTree(assrt)

  result.impl = pquote do:
    {.cast(noSideEffect).}:
      `impl`

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
    parseStringID: PNode = newPIdent("parse" & lang.capitalizeAscii() & "String")

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

    proc `parseStringID`*(str: string): `nodeType` =
      let parser = `newParserId`()
      return parseString(parser, str)

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
      ## Iterate over subnodes. `withUnnamed` - also iterate over unnamed
      ## nodes (usually things like punctuation, braces and so on).
      for i in 0 .. node.len(withUnnamed):
        yield node[i, withUnnamed]

    func slice*(node: `nodeType`): Slice[int] =
      {.cast(noSideEffect).}:
        ## Get range of source code **bytes** for the node
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


proc createProcDefinitions(
    spec: NodeSpec,
    inputLang: string,
    names: var CountTable[string]
  ): PNode =

  result = nnkStmtList.newPTree()

  result.add pquote do:
    import hparse/htreesitter/htreesitter, sequtils, strutils

  result.add makeKindEnum(spec, inputLang, names).toNNode(standalone = true)

  if spec.externals.len > 0:
    var externEnum = PEnumDecl(
      name: inputLang.capitalizeAscii() & "ExternalTok",
      exported: true)

    var externSet: CountTable[string]
    for extern in spec.externals:
      if extern.normalize() notin externSet:
        externEnum.values.add makeEnumField[PNode](
          inputLang & "Extern" & extern.capitalizeAscii(),
          comment = extern
        )


      else:
        let name = inputLang & "Extern" & extern.capitalizeAscii() &
          "_" & $(externSet[extern.normalize()] + 1)

        warn "External enum name clash"
        debug extern, "will be wrapped as", name

        externEnum.values.add makeEnumField[PNode](
          name,
          comment = extern
        )

      externSet.inc extern.normalize()

    result.add externEnum.toNNode(standalone = true)

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

  result.add makeGetKind(spec, inputLang, names).toNNode()
  result.add makeImplTsFor(inputLang)


proc getAppCachedHashes*(): seq[string] =
  let cacheDir = getAppCacheDir()
  let cachedFiles = (cacheDir /. "cachedFiles.txt")
  if cachedFiles.fileExists():
    return cachedFiles.readFile().split("\n")

proc setAppCachedHashes*(files: seq[string]) =
  (getAppCacheDir() /. "cachedFiles.txt").writeFile(files.join("\n"))

proc noChangesForFile*(file: AnyFile): bool =
  if $secureHashFile(file.getStr()) in getAppCachedHashes():
    true
  else:
    false

proc compileGrammar(
    grammarJs: AbsFile,
    langPrefix: string,
    scannerFile: Option[AbsFile] = none(AbsFile),
    parserSourceOut: Option[AbsFile] = none(AbsFile),
    junkDir: AbsDir,
    forceBuild: bool = false,
    extraFiles: seq[tuple[src: AbsFile, target: RelFile]] = @[]
  ): string =

  let junkDir = junkDir / langPrefix

  info "Lang prefix", langPrefix
  debug junkDir

  info "Using cache dir", junkDir

  let startDir = cwd()

  rmDir junkDir
  mkDir junkDir
  withDir junkDir:
    info "Started temporary directory"
    debug cwd()
    cpFile grammarJs, RelFile("grammar.js")
    if extraFiles.len > 0:
      logIndented:
        info "Copying extra files"
        for (src, target) in extraFiles:
          mkDir target.dir
          cpFile src, target
          debug src, "->\n", target

    execShell(shCmd(npm, --silent, link, "regexp-util"))
    execShell(shCmd(npm, --silent, link, "tree-sitter-c"))
    execShell(shCmd(npm, --silent, link, "readdir-enhanced"))
    execShell(shCmd(npm, --silent, link, "nan"))
    info "Linked regexp-util BS for node.js"

    info "Generating tree-sitter files"
    execShell shCmd("tree-sitter", "generate")


    # if true:
    #   execShell shCmd("tree-sitter", "build-wasm")
    #   execShell shCmd("tree-sitter", "web-ui")


    debug "Done"

    if scannerFile.isSome():
      let file = scannerFile.get()
      cpFile file, RelFile("src/scanner.c")

    var spec = NodeSpec(
      nodes: "src/node-types.json".parseFile(
      ).getElems().mapIt(it.toTree())
    )

    let grammar = "src/grammar.json".parseFile()

    for extra in grammar["extras"]:
      if "name" in extra:
        let name = extra["name"]
        spec.nodes.add Tree(ttype: name.asStr(), named: true)

    for extern in grammar["externals"]:
      if "name" in extern:
        spec.externals.add extern["name"].asStr()


    let lang: string = grammar["name"].asStr()

    let
      parserOut = startDir /. (lang & "_parser.o")
      wrapperOut = startDir /. (lang & "_wrapper.nim")
      scannerOut = if scannerFile.isSome():
                     some(startDir /. (lang & "_scanner.o"))
                   else:
                     none(AbsFile)

    var names: CountTable[string]
    wrapperOut.writeFile($createProcDefinitions(spec, lang, names))
    info "Wrote generated wrappers to", wrapperOut

    debug "Linking parser.c"
    execShell makeGnuShellCmd("gcc").withIt do:
      it.arg "src/parser.c"
      it - "c"
      it - ("o", "", "parser.o")

    cpFile RelFile("parser.o"), parserOut
    if parserSourceOut.getSome(src):
      cpFile RelFile("src/parser.c"), src

    debug "Copied parser file to", parserOut

    if scannerFile.isSome():
      let file = scannerFile.get()
      debug "Linking", file
      execShell makeGnuShellCmd("gcc").withIt do:
        it.arg $file
        it - "c"
        it - ("o", "", "scanner.o")

      cpFile RelFile("scanner.o"), scannerOut.get()
      debug "Compied compiled scanner to", scannerOut.get()


    if extraFiles.len > 0:
      logIndented:
        info "Copying extra files to target directory"
        for (src, target) in extraFiles:
          if target.ext != "js":
            cpFile target, parserOut.dir / target
            debug target, "->\n", parserOut.dir / target

    info "tree-sitter object files generation ok"
    return lang

proc argParse(
  dst: var tuple[src: AbsFile, target: RelFile],
  dfl: tuple[src: AbsFile, target: RelFile],
  a: var ArgcvtParams): bool =

  return false


proc grammarFromFile*(
    grammarJs:   FsFile         = RelFile("grammar.js"),
    scannerFile: Option[FsFile] = none(FsFile),
    parserUser:  Option[FsFile] = none(FsFile),
    parserOut:   Option[FsFile] = none(FsFile),
    cacheDir:    AbsDir         = getAppCacheDir(),
    nimcacheDir: Option[FsDir]  = none(FsDir),
    forceBuild:  bool           = false,
    langPrefix:  string         = "",
    extraFiles: seq[tuple[src: AbsFile, target: RelFile]] = @[]
  ) =

  info "Working directory is", cwd()
  if scannerFile.isNone():
    warn "No input scanner file"

  else:
    info "Scanner file is", scannerFile.get()
    info "Absolute scanner file position", scannerFile.get().toAbsFile()

  let lang = compileGrammar(
    grammarJs   = grammarJs.toAbsFile(true),
    langPrefix  = langPrefix,
    scannerFile = scannerFile.mapItSome(it.toAbsFile(true)),
    junkDir     = cacheDir,
    forceBuild  = forceBuild,
    parserSourceOut   = parserOut.mapItSome(it.toAbsFile()),
    extraFiles = extraFiles
  )


  if parserUser.isSome():
    info "Test parser user file is some. Compiling ..."
    let user = parserUser.get()
    try:
      let (stdout, stderr, code) = runShell makeNimShellCmd("nim").withIt do:
        it.cmd "c"
        it - "r"
        if nimcacheDir.isSome():
          it - ("nimcache", nimcacheDir.get().getStr())

        it - ("warnings", "off")

        it - ("forceBuild", "on")
        # if scannerFile.isSome():
        #   it - ("passL", lang & "_scanner.o")

        # # Link parser
        # it - ("passL", lang & "_parser.o")

        # TODO make linking with C++ stdlib optional
        it - ("passL", "-lstdc++")

        # Link tree-sitter
        it - ("passL", "-ltree-sitter")

        it.arg user

      echo stdout
      echo stderr
    except ShellError:
      let ex = getCEx(ShellError)
      echo ex.outstr
      # echo ex.msg
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

          elif line.contains("tree_sitter_"):
            once: err "Missing linking with compiled parser"

          else:
            once: err "Missing linking with other library"
            info line

        elif line.contains(["/bin/ld", "ld returned"]):
          discard
        else:
          echo line

import std/httpclient

proc grammarFromUrl*(
    grammarUrl: Url,
    grammarFile: FsFile     = RelFile("grammar.js"),
    scannerUrl: Option[Url] = none(Url),
    scannerFile: FsFile     = RelFile("scanner.c"),
    parserOut: FsFile       = RelFile("parser.c"),
    extraFiles: seq[tuple[src: AbsFile, target: RelFile]] = @[]
  ) =

  let client = newHttpClient()
  client.downloadFile(grammarUrl.string, grammarFile.getStr())
  if scannerUrl.getSome(scanner):
    client.downloadFile(scanner.string, scannerFile.getStr())

  grammarFromFile(
    grammarJs   = grammarFile,
    scannerFile = if scannerUrl.isNone(): none(FsFile) else: some(scannerFile),
    parserOut   = some(parserOut),
    extraFiles = extraFiles
  )

when isMainModule:
  startColorLogger()
  if paramCount() == 0:
    grammarFromFile(
      forceBuild = true,
      parserUser = some RelFile("parser_user.nim")
    )
  else:
    dispatchMulti(
      [grammarFromFile],
      [grammarFromUrl]
    )
