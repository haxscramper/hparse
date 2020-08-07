import grammars, lexer
import hmisc/[helpers]
import hmisc/algo/hseq_mapping
import hdrawing, hdrawing/term_buf
import hmisc/types/[seq2d]
import sugar, sequtils, hashes, tables, strutils, strformat, deques, sets

import bnf_grammars, grammars, parse_helpers, parse_tree, token,
       parse_primitives, bnf_algo

#*************************************************************************#
#******************************  LL1Table  *******************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

type
  LL1Table*[C, L] = Table[BnfNTerm, RuleLookup[C, L]]

#==============================  Accessors  ==============================#

func getRule*[C, L, I](
  tbl: LL1Table[C, L], nterm: BnfNterm, tok: Token[C, L, I]): RuleId =
  try:
    return tbl[nterm].getRule(tok)
  except AssertionError:
    {.noSideEffect.}:
      var e = getCurrentException()
      e.msg = msgjoin(
        "Failure to get rule for nterm `", nterm.exprRepr(), "`: ",
        getCurrentExceptionMsg())

      raise e


func `[]`*[A, B, C](
  table: Table[A, Table[B, C]], aKey: A, bKey: B): C =
  table[aKey][bKey]

func contains*[A, B, C](table: Table[A, Table[B, C]], pair: (A, B)): bool =
  (pair[0] in table) and (pair[1] in table[pair[0]])

func contains*[C, L, I](
  table: Table[BnfNterm, TokLookup[C, L]],
  pair: (BnfNterm, Token[C, L, I])): bool =
  (pair[0] in table) and (pair[1] in table[pair[0]])


proc necessaryTerms*[C, L](
  id: RuleId, grammar: BnfGrammar[C, L]): seq[BnfNTerm] =
  ## Generate list of nonterminals that might appear at rhs of production
  let patt: BnfPatt[C, L] = grammar.rules[id.head][id.alt]
  if not patt.elems[0].isTerm:
    result.add patt.elems[0].nterm

func toGrid[A, B, C](
  table: Table[A, Table[B, C]],
  aConvCb: proc(a: A): string {.noSideEffect.} = nil,
  bConvCb: proc(b: B): string {.noSideEffect.} = nil,
  cConvCb: proc(a: C): string {.noSideEffect.} = nil): Seq2D[string] =
  let aConvCb = (aConvCb != nil).tern(aConvCb, proc(a: A): string = $a)
  let bConvCb = (bConvCb != nil).tern(bConvCb, proc(b: B): string = $b)
  let cConvCb = (cConvCb != nil).tern(cConvCb, proc(c: C): string = $c)

  let aIdx: Table[string, int] = collect(initTable(2)):
    for rowIdx, key in toSeq(table.keys).mapIt(aConvCb(it)).sorted():
      {key : rowIdx + 1}

  var bIdx: Table[string, int] = block:
    let bKeys: seq[string] = collect(newSeq):
      for _, subtable in table:
        for bKey, _ in subtable:
          bConvCb(bKey)

    collect(initTable):
      for colIdx, key in bKeys.deduplicate():
        {key : colIdx + 1}

  result.fillToSize(rows = aIdx.len + 1, cols = bIdx.len + 1, default = "")
  for aKey, subtable in table:
    for bKey, cVal in subtable:
      result[aIdx[aConvCb(aKey)], bIdx[bConvCb(bKey)]] = cConvCb(cVal)

  for key, rowIdx in aIdx:
    result[rowIdx, 0] = $key

  for key, colIdx in bIdx:
    result[0, colIdx] = $key


const pconf* = GrammarPrintConf(
  prodArrow: "->",
  emptyProd: "''",
  ntermWrap: ("", ""),
  concatSep: " ",
  normalizeNterms: true
)

proc makeLL1TableParser*[C, L](grammar: BnfGrammar[C, L]): tuple[
  table: LL1Table[C, L], nullable: Table[BnfNterm, seq[AltId]]] =
  # let firstTable = getFirst(grammar)
  # let followTable = getFollow(grammar, firstTable)
  # mixin items
  let (firstTable, followTable, nullable) = getSets(grammar)
  result.nullable = nullable
  # echo "233"
  for ruleId, alt in grammar.iterrules():
    if ruleId.head notin firstTable:
      #[ IMPLEMENT REVIEW what has to be done ]#
      discard
    else:
      let first = firstTable[ruleId.head][ruleId.alt]
      if ruleId.head notin result.table:
        result.table[ruleId.head] = initRuleLookup(first, ruleId, canConflict = false)
      else:
        result.table[ruleId.head].addRule(first, ruleId, canConflict = false)

  for nterm, nullAlts in nullable:
    let first = followTable[nterm]
    for alt in nullAlts.deduplicate():
      let ruleId = ruleId(nterm, alt)

      if nterm notin result.table:
        result.table[nterm] = initRuleLookup(first, ruleId)
      else:
        result.table[nterm].addRule(first, ruleId, canConflict = false)



  plog:
    debugecho "\e[35mNULLABLE\e[39m set"
    for nterm, nullAlts in nullable:
      for alt in nullAlts:
        debugecho fmt("{nterm.exprRepr()}[{alt}]")

    debugecho "\e[35mFIRST\e[39m set"
    for head, alts in firstTable:
      for id, alt in alts:
        debugecho fmt("{alt.exprRepr():>40} -> [{id}]{head.exprRepr()}")

    debugecho "\e[35mFOLLOW\e[39m set"
    for head, alts in followTable:
      dechofmt "{alts.exprRepr():>40} -> {head.exprRepr()}"

    # debugecho "Parse table:\n", newTermGrid(
    #   (0,0),
    #   toGrid(
    #     result,
    #     # aConvCb = matchCurry2(BnfNterm, true, exprRepr),
    #     # # bConvCb = matchCurry2(C, L, pconf, exprRepr),
    #     # cConvCb = matchCurry2(RuleId, true, exprRepr)
    #   ).toTermBufGrid(),
    #   makeAsciiGridBorders()
    # ).toTermBuf().toString()

#============================  Parser object  ============================#

type
  LL1TableParser*[C, L] = object
    start: BnfNterm
    nullable: Table[BnfNterm, seq[AltId]]
    grammar: BnfGrammar[C, L]
    parseTable: LL1Table[C, L]
    retainGenerated: bool

func getGrammar*[C, L](
  parser: LL1TableParser[C, L]): BnfGrammar[C, L] =
  parser.grammar

proc newLL1TableParser*[C, L](
  grammar: Grammar[C, L],
  retainGenerated: bool = false): LL1TableParser[C, L] =
  let bnfg = grammar.toBNF()
  let tmp = makeLL1TableParser(bnfg)
  result.parseTable = tmp.table
  result.nullable = tmp.nullable
  result.start = bnfg.start
  result.grammar = bnfg
  result.retainGenerated = retainGenerated

type
  TermProgress[C, L, I] = object
    nterm: BnfNterm
    expected: seq[FlatBnf[C, L]]
    acts: ActLookup
    elems: seq[ParseTree[C, L, I]]

func canFold[C, L, I](lst: TermProgress[C, L, I],
                      forcefold: bool,
                      nullable: Table[BnfNterm, seq[AltId]]): bool =
  if forcefold:
    lst.expected[lst.elems.len .. ^1].allOfIt(it.isNullable(nullable))
  else:
    lst.elems.len == lst.expected.len

proc parse*[C, L, I](
  parser: LL1TableParser[C, L],
  toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I] =
  var
    stack: seq[FlatBnf[C, L]] # Symbols stack
    curr: Token[C, L, I] = toks.next() # Current input token
    ppr: seq[TermProgress[C, L, I]] = @[] # Parse progres for all rules

  stack.add FlatBnf[C, L](isTerm: false, nterm: parser.start)

  template foldstack(): untyped =
    let forcefold = toks.finished()
    if forcefold and not canFold(ppr.last(), forcefold, parser.nullable):
      echo "Unexpected EOF"

    while (ppr.len > 0) and (ppr.last().canFold(forcefold, parser.nullable)):
      let last = ppr.pop()
      if ppr.len > 0:
        ppr.last().elems.add(
          if last.nterm.generated:
            if parser.retainGenerated:
              newTree(last.nterm.exprRepr(), last.elems, last.acts)
            else:
              newTree(last.elems, last.acts)
          else:
            newTree(last.nterm.name, last.elems, last.acts))
      else:
        return newTree(last.nterm.name, last.elems)


  while true:
    let top: FlatBnf[C, L] = stack.pop()
    if top.isTerm:
      assertToken(top.tok, curr)
      ppr.last().elems.add newTree(curr)
      if toks.finished():
        foldstack()
      else:
        curr = toks.next()
    else:
      let rule: RuleId = parser.parseTable.getRule(top.nterm, curr)
      let stackadd = parser.grammar.getProductions(rule)
      ppr.add TermProgress[C, L, I](
        nterm: rule.head,
        expected: stackadd.symbols,
        acts: stackadd.getActions()
      )

      stack &= stackadd.reversed()

    foldstack()
