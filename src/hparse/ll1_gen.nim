import macros, options, sugar
import strformat, strutils, sequtils, algorithm
import hmisc/helpers
import hmisc/types/initcalls
import sets
import grammars

export helpers

import hashes, tables, sets

import lexer
import parse_primitives, parser_common, parse_tree, parse_helpers, token,
       codegen_common

export codegen_common


## LL1 parser generator code


func makeInitCalls*[C, L](tok: TokSet[C, L]): NimNode =
  mixin makeInitCalls
  result = newCall(
    "initTokSet",
    nnkExprEqExpr.newTree(ident "tokens", tok.getTokens().makeInitCalls()),
    nnkExprEqExpr.newTree(ident "hasEof", tok.getHasEof().makeInitCalls()))

func makeInitCalls*[L](lex: LexSet[L]): NimNode =
  mixin makeInitCalls
  result = newCall(
    "initLexSet",
    nnkExprEqExpr.newTree(ident "hasAll", lex.getHasAll().makeInitCalls()),
    nnkExprEqExpr.newTree(ident "lexemes", lex.getLexemes().makeInitCalls()))


template doIt(s, action: untyped): untyped =
  ## Execute action for each element in sequence, return original
  ## action.
  type Item = type((s[0]))
  for it {.inject.} in s:
    action

  s

type
  FirstSet*[C, L] = TokSet[C, L]
  NTermSets*[C, L] = object
    first*: Table[NTermSym, FirstSet[C, L]]

  CompPatt*[C, L] = object
    action*: TreeAct
    first: FirstSet[C, L]
    case kind*: PattKind
      of pkNterm:
        nterm*: NTermSym ## Nonterminal to parse
      of pkTerm:
        tok*: ExpectedToken[C, L] ## Single token to match literally
      of pkAlternative, pkConcat:
        patts*: seq[CompPatt[C, L]]
      of pkOptional, pkZeroOrMore, pkOneOrMore:
        opt*: seq[CompPatt[C, L]] ## Single instance that will be repeated
        # I could've used `Option[]` but decided to go with `seq`
        # since I should not have a situation where `opt` field is
        # `none` - it is just a workaround to allow recursive field


  CompRule*[C, L] = object
    nterm*: NTermSym
    patts*: CompPatt[C, L]

  CompGrammar*[C, L] = object
    sets*: NTermSets[C, L]
    rules*: seq[CompRule[C, L]]

#=========================  FIRST set computat  ==========================#

proc computeFirst*[C, L](
  patt: Patt[C, L], other: NTermSets[C, L]): FirstSet[C, L] =
  ## Generate FIRST set for `patt`
  case patt.kind:
    of pkTerm:
      result.incl patt.tok
    of pkConcat:
      result.incl computeFirst(patt.patts[0], other)
    of pkAlternative:
      for p in patt.patts:
        result.incl computeFirst(p, other)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      result.incl computeFirst(patt.opt, other)
    of pkNterm:
      result.incl other.first[patt.nterm]

proc computePatt*[C, L](
  patt: Patt[C, L], sets: NTermSets[C, L]): CompPatt[C, L] =
  ## Generate FIRST set for pattern `patt`
  let kind = patt.kind
  case kind:
    of pkTerm:
      result = CompPatt[C, L](
        kind: pkTerm,
        tok: patt.tok,
        first: makeTokSet[C, L]())
      result.first.incl patt.tok
    of pkConcat:
      result = CompPatt[C, L](
        kind: pkConcat,
        patts: patt.patts.mapIt(computePatt(it, sets)),
        first: makeTokSet[C, L]()
      )
      result.first.incl computeFirst(patt.patts[0], sets)
    of pkAlternative:
      result = CompPatt[C, L](
        kind: pkAlternative,
        patts: patt.patts.mapIt(computePatt(it, sets)),
        first: makeTokSet[C, L]()
      )
      for p in patt.patts:
        result.first.incl computeFirst(p, sets)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      result = CompPatt[C, L](
        kind: kind,
        opt: @[computePatt(patt.opt, sets)],
        first: makeTokSet[C, L]())
      result.first.incl computeFirst(patt.opt, sets)
    of pkNterm:
      # FIRST sets for nonterminals are stored in `sets`
      result = CompPatt[C, L](
        kind: pkNterm,
        nterm: patt.nterm,
        first: makeTokSet[C, L]()
      )

  result.action = patt.action

func first*[C, L](
  patt: CompPatt[C, L], sets: NTermSets[C, L]): FirstSet[C, L] =
  case patt.kind:
    of pkNTerm: sets.first[patt.nterm]
    else: patt.first


proc necessaryTerms[C, L](rhs: Patt[C, L]): seq[NTermSym] =
  ## Generate list of nonterminals that might appear at rhs of production
  case rhs.kind:
    of pkAlternative:
      return rhs.patts.filterIt(it.kind == pkNTerm).mapIt(it.nterm)
    of pkNTerm:
      return @[rhs.nterm]
    of pkConcat:
      return necessaryTerms(rhs.patts[0])
    of pkZeroOrMore, pkOptional, pkOneOrMore:
      return necessaryTerms(rhs.item[0])
    else:
      return @[]

proc computeGrammar*[C, L](g: Grammar[C, L]
  ): CompGrammar[C, L] =
  ## Generate first/follow sets for all rules in grammar. Rules in
  ## resulting grammar are ordered based on topological sorting.
  var sets: NTermSets[C, L]
  # Just because I can sqeeze it into <= 4 lines does not mean that it
  # is a good idea. But code above performs topological sort of the
  # whole grammar based on which terms depend on which. If there is a
  # cycle in grammar exception is thrown - it means grammar is
  # left-recursive. (REVIEW: check if left recursion cannot occur from
  # some other type of grammar)

  let sortedRules = g.rules.topoSort(
    deps = ((r) => (r.patts.necessaryTerms().mapIt(it.hash))),
    idgen = ((r) => hash(r.nterm))
  )

  # echo "---"
  # for rule in g.rules:
  #   echo rule.exprRepr()

  # for rule in sortedRules:
  #   echo rule.exprRepr()


  for rule in sortedRules:
    let compPatt = computePatt(rule.patts, sets)
    sets.first[rule.nterm] = first(compPatt, sets)
    result.rules.add CompRule[C, L](nterm: rule.nterm, patts: compPatt)

  result.sets = sets

proc makeSetLiteral*[C, L](s: TokSet[C, L]): NimNode =
  makeInitCalls(s)

proc makeParseBlock[C, L](
  patt: CompPatt[C, L],
  sets: NTermSets[C, L],
  resName: string = "res"): NimNode

proc makeAltBlock[C, L](alt: CompPatt[C, L], sets: NTermSets[C, L]): NimNode =
  ## Create code block for parsing alternative pattern
  assert alt.kind == pkAlternative
  let (sets, branches) = unzip: collect(newSeq):
    for idx, patt in alt.patts:
      let
        resName = &"patt{idx}res"
        resIdent = ident resName
        parseBlock = makeParseBlock(patt, sets, resName)

      (
        first(patt, sets),
        nnkOfBranch.newTree(
          newLit(idx),
          quote do:
            `parseBlock`
            `resIdent`
        )
      )

  let
    selector: TokLookup[C, L] = makeTokLookup(sets)
    selectorLit = makeInitCalls(selector)
    toksIdent = ident "toks"
    altId = ident "altId"
    elseBody = nnkElse.newTree(
      quote do:
        # TODO IMPLEMENT generated more informative error messages
        raiseAssert("Unexpected token")
    )


  result = quote do:
    let selector = `selectorLit`
    let peek = `toksIdent`.peek()
    let `altId` = selector.getAlt(peek)
    # echo "selected id ", `altId`, " for token ", peek.exprRepr()

  result.add: withIt(nnkCaseStmt.newTree(altId)):
    for branch in branches & @[elseBody]:
      it.add branch

# proc makeParserName*(nterm: NTermSym): string =
#   ## Converter nonterminal name into parsing proc name
#   "parse" & nterm.capitalizeAscii()

proc makeTermBlock[C, L](term: CompPatt[C, L]): NimNode =
  assert term.kind == pkTerm
  let
    tokIdent = toInitCalls(term.tok)
    tokType = ident "Tok"
    toksIdent = ident "toks"
  return quote do:
    # echo "token block"
    let expected = `tokIdent`
    # echo "expected token, retrieving next ..."
    let tok = next(`toksIdent`)
    # echo "asserting token ..."
    assertToken(expected, tok)
    newTree(tok)

proc makeNTermBlock[C, L](nterm: CompPatt[C, L]): NimNode =
  assert nterm.kind == pkNTerm
  let
    ntermIdent = ident(makeParserName(nterm.nterm))
    toksIdent = ident "toks"
  quote do:
    `ntermIdent`(`toksIdent`)

proc makeConcatBlock[C, L](nterm: CompPatt[C, L], sets: NTermSets[C, L]): NimNode =
  assert nterm.kind == pkConcat
  let parseStmts = collect(newSeq):
    for idx, patt in nterm.patts:
      makeParseBlock(patt, sets, &"patt{idx}res")

  let valueVars = nnkBracket.newTree(
    nterm.patts
      .enumerate()
      .mapIt(ident &"patt{it[0]}res")
  )

  # let tokIdent = ident "Tok"
  let
    cId = ident($(typeof C))
    lId = ident($(typeof L))
  return (parseStmts & @[
    quote do:
      newTree[`cId`, `lId`](@`valueVars`)
      # ParseTree[`tokIdent`](
      #   kind: pkConcat,
      #   values: @`valueVars`
      # )
  ]).newStmtList()

proc makeNtoMTimesBlock[C, L](
  nterm: CompPatt[C, L], sets: NtermSets[C, L],
  mintimes, maxtimes: int): NimNode =
  assert nterm.kind in {pkZeroOrMore, pkOneOrMore, pkOptional}
  let
    laLiteral = makeSetLiteral(first(nterm, sets))
    bodyParse = makeParseBlock(nterm.opt[0], sets, "itemRes")
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
          raise SyntaxError(
            msg: "Expected at least " & $(`minLit`) & " elements but found only " & $`cnt`
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
    let laSet = `laLiteral`
    # echo "entering loop"
    # echo `toksIdent`.peek().exprRepr(), " ", `toksIdent`.peek() in laSet
    # echo laSet.exprRepr()
    while `countConstraints` and (not `toksIdent`.finished()) and (`toksIdent`.peek() in laSet):
      # echo "found item"
      `bodyParse`
      inc `cnt`
      `subItems`.add `itemIdent`

    `minNumAssert`
    `finalValue`



proc makeParseBlock[C, L](
  patt: CompPatt[C, L],
  sets: NTermSets[C, L],
  resName: string = "res"): NimNode =
  ## Generate code block to parse pattern `patt`.
  result = case patt.kind:
    of pkTerm:
      makeTermBlock(patt)
    of pkOptional:
      makeNtoMTimesBlock(patt, sets, 0, 1)
    of pkNterm:
      makeNTermBlock(patt)
    of pkAlternative:
      makeAltBlock(patt, sets)
    of pkConcat:
      makeConcatBlock(patt, sets)
    of pkZeroOrMore:
      makeNtoMTimesBlock(patt, sets, 0, -1)
    of pkOneOrMore:
      makeNtoMTimesBlock(patt, sets, 1, -1)

  let
    resIdent = ident resName
    toksIdent = ident "toks"
    cId = ident($(typeof C))
    lId = ident($(typeof L))
    iId = ident("I")


  let actAssgn =
    if patt.action != taDefault:
      let actLit = ident($patt.action)
      quote do:
        `resIdent`.action = `actLit`
    else:
      newEmptyNode()

  let
    comment = $patt & " " & $patt.kind
    commentLit = newLit(comment)

  return newStmtList(
    newCommentStmtNode(comment),
    quote do:
      # echo "parsing ", `comment`, " ... "
      var `resIdent`: ParseTree[`cId`, `lId`, `iId`] = block:
        `result`

      runTreeActions(`resIdent`)
      `actAssgn`
    )

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
      nnkBracketExpr.newTree(
        newIdentNode("ParseTree"),
        newIdentNode($(typeof C)),
        newIdentNode($(typeof L)),
        newIdentNode("I")
      ),
      nnkIdentDefs.newTree(
        newIdentNode("toks"),
        nnkVarTy.newTree(
          nnkBracketExpr.newTree(
            newIdentNode("TokStream"),
            nnkBracketExpr.newTree(
              newIdentNode("Token"),
              newIdentNode($(typeof C)),
              newIdentNode($(typeof L)),
              newIdentNode("I")
            )
          )
        ),
        newEmptyNode()
      )
    ),
    newEmptyNode(),
    newEmptyNode(),
    newEmptyNode()
  )



proc makeRuleParser[C, L](
  rule: CompRule[C, L],
  sets: NTermSets[C, L]): tuple[decl, impl: NimNode] =
  ## Generate implementation for proc to parse rule
  let
    procName = ident(rule.nterm.makeParserName())
    resIdent = ident "res"
    ntermNterm = newLit(rule.nterm)
    parseBody = rule.patts.makeParseBlock(sets)

  let decl = makeParseProcDef[C, L](rule.nterm.makeParserName())
  var impl = makeParseProcDef[C, L](rule.nterm.makeParserName())
  impl[6] = quote do:
    `parseBody`
    case `resIdent`.kind:
      of ptkToken, ptkNTerm:
        return newTree(
          name = `ntermNterm`, subnodes = @[`resIdent`])
      of ptkList:
        return newTree(
          name = `ntermNterm`, subnodes = `resIdent`.getSubnodes())

  return (decl: decl, impl: impl)


proc makeGrammarParser*[C, L](gram: CompGrammar[C, L]): NimNode =
  ## Generate code for parsing grammar `gram`
  var
    decls: seq[NimNode]
    impls: seq[NimNode]
  for rule in gram.rules:
    let (decl, impl) = makeRuleParser(rule, gram.sets)
    decls.add decl
    impls.add impl

  result = newStmtList(
    decls.newStmtList(),
    impls.newStmtList()
  )

proc `$`*[C, L](patt: CompPatt[C, L]): string =
  case patt.kind:
    of pkNterm:
       &"<{patt.nterm}>"
    of pkTerm:
       &"'{patt.tok.exprRepr()}'"
    of pkAlternative:
      patt.patts.mapIt($it).join(" | ")
    of pkConcat:
      patt.patts.mapIt($it).join(" , ")
    of pkOptional:
      &"({patt.opt})?"
    of pkZeroOrMore:
      &"({patt.opt})*"
    of pkOneOrMore:
      &"({patt.opt})+"


#=======================  Parser type definition  ========================#

type
  LL1RecursiveDescentParser*[C, L, I] = object
    startCb: proc(toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I]

func newLL1RecursiveDescent*[C, L, I](
  cb: proc(toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I]
                                    ): LL1RecursiveDescentParser[C, L, I] =
  result.startCb = cb

proc parse*[C, L, I](
  parser: LL1RecursiveDescentParser[C, L, I],
  toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I] =
  # echo "parsing ... "
  parser.startCb(toks)

template newLL1RecursiveParser*[C, L, I](
  body: typed,
  standalone: bool = false): untyped =
  # Trillion IQ hack
  macro buildParser(): untyped =
    let grammar = toGrammar(body)
    let compGrammar = computeGrammar(grammar)
    let cbName = grammar.start.makeParserName()
    result = newStmtList(
      makeGrammarParser(compGrammar),
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
              newIdentNode("newLL1RecursiveDescent"),
              newIdentNode("cb"))))
      else:
        newCall("newLL1RecursiveDescent", ident "cb")
    )

    # echo result.toStrLit()
    # colorPrint(result, doPrint = false)

  buildParser()
