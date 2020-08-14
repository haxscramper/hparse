import sugar, strutils, sequtils, strformat, macros, tables, hashes
import options

import hmisc/helpers
import hmisc/types/initcalls

import parse_primitives, token, lexer, codegen_common, parse_tree,
       grammars, bnf_grammars

export codegen_common

type
  ParseCache*[C, L, I] = object
    table*: Table[(Hash, int), Option[ParseTree[C, L, I]]] ## Pattern
    ## hash + current position -> parse tree value. If `(Hash, int)`
    ## is not present - parsing required. If value is `none` - parsing
    ## has been attempted and failed. `some` - parsing succeded,
    ## return immediately

func contains*[C, L, I](cache: ParseCache[C, L, I],
                        idpos: (Hash, int)): bool =
  idpos in cache.table

func `[]`*[C, L, I](cache: ParseCache[C, L, I],
                    idpos: (Hash, int)): Option[ParseTree[C, L, I]] =
  cache.table[idpos]

func `[]=`*[C, L, I](cache: var ParseCache[C, L, I],
                     idpos: (Hash, int),
                     item: Option[ParseTree[C, L, I]]): void =
  cache.table[idpos] = item

func makeParseProcDef[C, L](name: string): NimNode =
  nnkProcDef.newTree(
    newIdentNode(name),
    newEmptyNode(),
    nnkGenericParams.newTree(
      nnkIdentDefs.newTree(
        newIdentNode("I"),
        newEmptyNode(),
        newEmptyNode()
      )
    ),
    nnkFormalParams.newTree(
      mkNTypeNode(
        "Option",
        mkNType("ParseTree", @[$(typeof C), $(typeof L), "I"])),
      mkVarDeclNode(
        "toks",
        mkNType("TokStream",
                @[mkNType("Token", @[$(typeof C), $(typeof L), "I"])]),
        nvdVar
      ),
      mkVarDeclNode(
        "cache",
        mkNType("ParseCache", @[$(typeof C), $(typeof L), "I"]),
        nvdVar
      )
    ),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode()
  )


proc makeParseBlock[C, L](patt: Patt[C, L], resName: string = "res"): NimNode


proc makeTermBlock[C, L](term: Patt[C, L]): NimNode =
  assert term.kind == pkTerm
  let
    tokIdent = newLit(term.tok)
    tokType = ident "Tok"
    toksIdent = ident "toks"

  let (c, l, i) = makeIds[C, L]()

  return quote do:
    let expected = `tokIdent`
    let tok = next(`toksIdent`)
    if expected.matches(tok):
      some(newTree(tok))
    else:
      none(ParseTree[`c`, `l`, `i`])


proc makeNtoMTimesBlock[C, L](nterm: Patt[C, L],
                              mintimes, maxtimes: int): NimNode =
  assert nterm.kind in {pkZeroOrMore, pkOneOrMore, pkOptional}
  let
    bodyParse = makeParseBlock(nterm.item[0], "itemRes")
    minLit = newLit(mintimes)
    maxLit = newLit(maxtimes)
    cnt = ident("cnt")
    tokType = ident("Tok")
    itemIdent = ident("itemRes")
    kindLiteral = ident($nterm.kind)
    subItems = ident "subItems"
    toksIdent = ident "toks"


  let countConstraints =
    if maxtimes > 0:
      nnkPar.newTree(nnkInfix.newTree(
        ident "<",
        ident "cnt",
        maxLit))
    else:
      ident("true")

  let minNumAssert =
    if mintimes > 0:
      quote do:
        if `cnt` < `minLit`:
          raiseAssert(
            "Expected at least " & $(`minLit`) & " elements but found only " & $`cnt`
          )
    else:
      newEmptyNode()

  let (c, l, i) = makeIds[C, L]()
  let finalValue =
    if nterm.kind == pkOptional:
      quote do:
        if subItems.len == 1:
          ParseTree[`c`, `l`, `i`](kind: pkOptional, optValue: some(`subItems`[0]))
        else:
          ParseTree[`c`, `l`, `i`](kind: pkOptional)
    else:
      quote do:
        newTree[`c`, `l`, `i`](`subItems`)
        # ParseTree[`c`, `l`, `i`](kind: `kindLiteral`, values: `subItems`)


  return quote do:
    # TEST WARNING possible variable shadowing if parsing rule
    # contains nested `{N,M}` rules.
    var `cnt` = 0
    var `subItems`: seq[ParseTree[`c`, `l`, `i`]]
    # echo "entering loop"
    # echo `toksIdent`.peek().exprRepr(), " ", `toksIdent`.peek() in laSet
    # echo laSet.exprRepr()
    # while `countConstraints` and (not `toksIdent`.finished()) and (`toksIdent`.peek() in laSet):
    #   # echo "found item"
    #   `bodyParse`
    #   inc `cnt`
    #   `subItems`.add `itemIdent`

    # `minNumAssert`
    # `finalValue`


proc makeNTermBlock[C, L](nterm: Patt[C, L]): NimNode =
  assert nterm.kind == pkNTerm
  # let
  #   ntermIdent = ident(makeParserName(nterm.nterm))
  #   toksIdent = ident "toks"
  #   cacheId = ident "cache"
  newCall(makeParserName(nterm.nterm), ident("toks"), ident("cache"))
  # quote do:
  #   `ntermIdent`(`toksIdent`, `cacheId`)


proc makeAltBlock[C, L](alt: Patt[C, L]): NimNode =
  ## Create code block for parsing alternative pattern
  assert alt.kind == pkAlternative
  let
    foundOk = ident "foundOk"
    finalId = ident "finalRes"
    altStr = newLit alt.exprRepr(
      defaultGrammarPrintConf.withIt do: it.colored = false)

  let (c, l, i) = makeIds[C, L]()

  let branches: NimNode = newStmtList: collect(newSeq):
    for idx, patt in alt.patts:
      let
        idxLit = newLit(idx)
        resname = "altTmp"
        resid = ident resname
        parseBlock = makeParseBlock(patt, resname)
      quote:
        if not `foundOk`:
          # echo "Trying alternative ", `altStr`, " ", `idxLit`
          `finalId` = block:
            `parseblock`
            `resid`

          if `finalId`.isSome():
            `foundOk` = true

  quote do:
    var `finalId`: Option[ParseTree[`c`, `l`, `i`]]
    var `foundOk`: bool = false
    `branches`
    `finalId`




proc makeConcatBlock[C, L](nterm: Patt[C, L]): NimNode =
  assert nterm.kind == pkConcat
  let allOk = ident "parseOk"
  let (c, l, i) = makeIds[C, L]()

  let parseStmts = collect(newSeq):
    for idx, patt in nterm.patts:
      let
        resname = &"patt{idx}res"
        resid = ident resname
        parseblock = makeParseBlock(patt, resname)

      quote:
        let `resid` = block:
          if `allOk`:
            `parseblock`
            `resid`
          else:
            none(ParseTree[`c`, `l`, `i`])

        if `resid`.isNone():
          `allOk` = false

          # `resid`

  let valueVars = nnkBracket.newTree(
    nterm.patts
      .enumerate()
      .mapIt(ident &"patt{it[0]}res")
  )

  return (
    @[ newVarStmt(allOk, newLit(true)) ] & parseStmts & @[
    quote do:
      if `allOk`:
        var buf: seq[ParseTree[`c`, `l`, `i`]]
        for idx, item in `valueVars`:
          # echo idx
          # echo item.get().treeRepr()
          buf.add item.get()

        some(newTree[`c`, `l`](buf))
      else:
        none(ParseTree[`c`, `l`, `i`])
  ]).newStmtList()

proc makeParseBlock[C, L](patt: Patt[C, L], resName: string = "res"): NimNode =
  ## Generate code block to parse pattern `patt`.
  result = case patt.kind:
    of pkTerm:
      makeTermBlock(patt)
    of pkOptional:
      makeNtoMTimesBlock(patt, 0, 1)
    of pkNterm:
      makeNTermBlock(patt)
    of pkAlternative:
      makeAltBlock(patt)
    of pkConcat:
      makeConcatBlock(patt)
    of pkZeroOrMore:
      makeNtoMTimesBlock(patt, 0, -1)
    of pkOneOrMore:
      makeNtoMTimesBlock(patt, 1, -1)

  let
    resId = ident resName
    toksId = ident "toks"
    pattHashId = ident "pattHash"
    pattHashLit = newLit(hash(patt))
    cacheId = ident "cache"

  let (cId, lId, iId) = makeIds[C, L]()


  let actAssgn =
    if patt.action != taDefault:
      let actLit = ident($patt.action)
      quote do:
        `resId`.get().action = `actLit`
    else:
      newEmptyNode()

  let
    comment = patt.exprRepr(
      defaultGrammarPrintConf.withIt do: it.colored = false)
    commentLit = newLit(comment)

  return newStmtList(
    newCommentStmtNode(comment),
    quote do:
      let currpos = `toksId`.absPos()
      var `resId`: Option[ParseTree[`cId`, `lId`, `iId`]] = block:
        let idpos: (Hash, int) = (`pattHashLit`, `toksId`.absPos())
        # echo `toksId`.currpos()
        if idpos in `cacheId`:
          # echo "Position ", idpos,
          #   " known in cache as ", `cacheId`[idpos].isSome()
          if `cacheId`[idpos].isSome():
            let endpos = `cacheId`[idpos].get().finish
            # echo "Succeded earlier, moving stream to ", endpos
            `toksId`.revertTo(endpos)

          `cacheId`[idpos]
        else:
          let tmp = block:
            `result`

          `cacheId`[idpos] = tmp
          tmp

      if `resId`.isNone():
        `toksId`.revertTo(currpos)
        # echo "Parse of ", `commentLit`, " from ", currpos, " failed"
      else:
        `resId`.get().start = currpos
        `resId`.get().finish = `toksId`.absPos()
        runTreeActions(`resId`.get())
        `actAssgn`
    )

proc makeRuleParser[C, L](rule: Rule[C, L]): tuple[
  decl, impl: NimNode] =
  ## Generate implementation for proc to parse rule
  let
    procName = ident(rule.nterm.makeParserName())
    resIdent = ident "res"
    ntermNterm = newLit(rule.nterm)
    parseBody = rule.patts.makeParseBlock()

  let decl = makeParseProcDef[C, L](rule.nterm.makeParserName())
  var impl = makeParseProcDef[C, L](rule.nterm.makeParserName())
  impl[6] = quote do:
    `parseBody`
    if `resIdent`.isSome():
      case `resIdent`.get().kind:
        of ptkToken, ptkNTerm:
          return some(
            newTree(name = `ntermNterm`, subnodes = @[`resIdent`.get()]))
        of ptkList:
          return some(
            newTree(name = `ntermNterm`,
                    subnodes = `resIdent`.get().getSubnodes()))

  return (decl: decl, impl: impl)

proc makeLLStarParser*[C, L](gram: Grammar[C, L]): NimNode =
  ## Generate code for parsing grammar `gram`
  var
    decls: seq[NimNode]
    impls: seq[NimNode]
  for rule in gram.rules:
    let (decl, impl) = makeRuleParser(rule)
    decls.add decl
    impls.add impl

  result = newStmtList(
    decls.newStmtList(),
    impls.newStmtList()
  )


type
  LLStarParser*[C, L, I] = object
    startCb: proc(
      toks: var TokStream[Token[C, L, I]],
      cache: var ParseCache[C, L, I]
    ): Option[ParseTree[C, L, I]]

func initLLStarParser*[C, L, I](
      cb: proc(
        toks: var TokStream[Token[C, L, I]],
        cache: var ParseCache[C, L, I]
      ): Option[ParseTree[C, L, I]]
    ): LLStarParser[C, L, I] =
  result.startCb = cb


proc parse*[C, L, I](
  parser: LLStarParser[C, L, I],
  toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I] =
  var cache: ParseCache[C, L, I]
  parser.startCb(toks, cache).get()

template newLLStarParser*[C, L, I](
  body: typed,
  standalone: bool = false): untyped =
  # Trillion IQ hack
  macro buildParser(): untyped =
    let grammar = toGrammar(body)
    let cbName = grammar.start.makeParserName()
    result = newStmtList(
      makeLLStarParser(grammar),
      nnkLetSection.newTree(
        nnkIdentDefs.newTree(
          newIdentNode("cb"),
          newEmptyNode(),
          nnkBracketExpr.newTree(
            newIdentNode(cbName),
            newIdentNode($(typeof I))
          )
        )
      ),
      if standalone:
        nnkLetSection.newTree(
          nnkIdentDefs.newTree(
            newIdentNode("parser"),
            newEmptyNode(),
            nnkCall.newTree(
              newIdentNode("initLLStarParser"),
              newIdentNode("cb"))))
      else:
        newCall("initLLStarParser", ident "cb")
    )

    colorPrint(result, doPrint = false)

  buildParser()
