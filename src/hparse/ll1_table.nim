import grammars, lexer
import hmisc/[helpers]
import hmisc/algo/hseq_mapping
import hdrawing, hdrawing/term_buf
import hmisc/types/[seq2d]
import sugar, sequtils, hashes, tables, strutils, strformat, deques, sets

import bnf_grammars, grammars, parse_helpers, parse_tree, token,
       parse_primitives

type
  AltId* = int
  FirstTable*[C, L] = Table[BnfNterm, Table[AltId, TokSet[C, L]]]
  FollowTable*[C, L] = Table[BnfNterm, TokSet[C, L]]

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

func `|=`*(a: var bool, b: bool): void = a = a or b

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

func isNullable[C, L](
  fbnf: FlatBnf[C, L],
  nulls: Table[BnfNterm, seq[AltId]]): bool =
  if fbnf.isTerm:
    false
  else:
    fbnf.nterm in nulls
    # of fbkEmpty: true
    # of fbkTerm: false
    # of fbkNterm:

func getSets*[C, L](grammar: BnfGrammar[C, L]): tuple[
  first: FirstTable[C, L],
  follow: FollowTable[C, L],
  nullable: Table[BnfNterm, seq[AltId]]] =

  for head, alts in grammar.rules:
    for altId, altBody in alts:
      # Generate empty `FIRST/FOLLOW` sets - each rule+alternative pair
      # corresponds to empty set.
      if head notin result.first:
        result.first[head] = {altId : makeTokSet[C, L]()}.toTable()
      else:
        result.first[head][altId] = makeTokSet[C, L]()
      # result.follow[rule.head] = {AltId(rule.alt) : makeTokSet[C, L]()}.toTable()
      result.follow[head] = makeTokSet[C, L]()

  block: # Add end token to `FOLLOW`
    var endNterms = initDeque[BnfNterm]()
    endNterms.addLast grammar.start
    while endNterms.len > 0:
      let nterm = endNterms.popFirst()
      result.follow[nterm] = makeTokSet[C, L](eofTok)
      for altId, alt in grammar.rules[nterm]:
        if not alt.elems.last.isTerm:
          endNterms.addLast alt.elems.last.nterm

  while true:
    var updated: bool = false
    for rule, body in grammar.iterrules():
      # showLog fmt "Processing {rule.exprRepr()} {body.exprRepr()}"
      # runIndentedLog:
      block: # `FIRST` set construction
        # Iterate over all rules in grammar
        if body.isEmpty():
          if rule.head notin result.nullable:
            result.nullable[rule.head] = @[ rule.alt ] # Remember index of nullable alternative
            updated |= true
          else:
            updated |= rule.alt notin result.nullable[rule.head]
            result.nullable[rule.head].add rule.alt
        else:
          for idx, elem in body.elems: # Iterate over all elements in `X ->
            # Y1 Y2` Store `FIRST` sets separately for each
            # alternative. Finish execution after first non-nullable
            # element is found.
            let first =
              if elem.isTerm:
                # Add token to `FIRST[X]` directly
                # showLog fmt "Found {elem.tok} @ {body.exprRepr()}[{idx}]"
                makeTokSet(elem.tok)
              else:
                # Add elements from `FIRST[Yi]` to `FIRST[X, <alt>]`.
                # Since `Yi` might have more than one alternative in
                # grammar we have to merge all possible `FIRST` sets.
                result.first[elem.nterm].mapPairs(rhs).union()


            # showLog fmt "Adding {first:>30} to FIRST of
            # {rule.head}[{rule.alt}]"
            updated |= result.first[rule.head][rule.alt].containsOrIncl(first)


            if not elem.isNullable(result.nullable):
              # showInfo fmt "Found non-nullable element
              # {elem.exprRepr()}"
              break # Found non-nullable element, finishing FIRST computation
            # else:
              # showInfo fmt "Element {elem.exprRepr()} of kind {elem.kind} is nullable"

          # showInfo fmt "Finished processing {body.exprRepr()}"

      block: # `FOLLOW` set construction
        var tailFollow: TokSet[C, L] = result.follow[rule.head] # `FOLLOW`
        # for the nonterminal we are working with - need to add this
        # as `FOLLOW` for element at the end
        for elem in body.elems.reversed(): # Iterate over all elements
          # in production in reverse order: `Y1 Y2 Y3 <- X`
          if not elem.isTerm:
            updated |= result.follow[elem.nterm].containsOrIncl(tailFollow)

          if elem.isNullable(result.nullable):
            # Continue snowballing `FOLLOW` tail - current element is
            # nullable => whatever we have accumulated in tail can
            # possibly appear in production.
            tailFollow.incl(result.first[elem.nterm].mapPairs(rhs).union())
          else:
            # Current elemen is not nullable => current tail is no
            # longer needed anc can be replaced with whatever
            # `FIRST[Yi]` contains.
            if elem.isTerm:
              tailFollow = makeTokSet(elem.tok)
            else:
              tailFollow = result.first[elem.nterm].mapPairs(rhs).union()


    if not updated:
      break



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

proc makeLL1TableParser*[C, L](grammar: BnfGrammar[C, L]): LL1Table[C, L] =
  # let firstTable = getFirst(grammar)
  # let followTable = getFollow(grammar, firstTable)
  # mixin items
  let (firstTable, followTable, nullable) = getSets(grammar)
  for ruleId, alt in grammar.iterrules():
    if ruleId.head notin firstTable:
      #[ IMPLEMENT REVIEW what has to be done ]#
      discard
    else:
      let first = firstTable[ruleId.head][ruleId.alt]
      if ruleId.head notin result:
        result[ruleId.head] = initRuleLookup(first, ruleId, canConflict = false)
      else:
        result[ruleId.head].addRule(first, ruleId, canConflict = false)

  for nterm, nullAlts in nullable:
    let first = followTable[nterm]
    for alt in nullAlts.deduplicate():
      let ruleId = ruleId(nterm, alt)

      if nterm notin result:
        result[nterm] = initRuleLookup(first, ruleId)
      else:
        result[nterm].addRule(first, ruleId, canConflict = false)



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
  plog:
    debugecho "\e[41mInput grammar\e[49m:\n", grammar.exprRepr()
    debugecho "\e[41mBNF grammar\e[49m:\n", bnfg.exprRepr(true, conf = pconf),
      "\n"
  result.parseTable = makeLL1TableParser(bnfg)
  result.start = bnfg.start
  result.grammar = bnfg
  result.retainGenerated = retainGenerated

type
  TermProgress[C, L, I] = object
    nterm: BnfNterm
    expected: int
    acts: ActLookup
    elems: seq[ParseTree[C, L, I]]

proc parse*[C, L, I](
  parser: LL1TableParser[C, L],
  toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I] =
  var stack: seq[FlatBnf[C, L]]
  stack.add FlatBnf[C, L](isTerm: false, nterm: parser.start)
  var
    curr: Token[C, L, I] = toks.next()
    ntermStack: seq[TermProgress[C, L, I]] = @[]
    done = false
    parseDone: bool = false

  while not done:
    let top: FlatBnf[C, L] = stack.pop()

    if top.isTerm:
      assertToken(top.tok, curr)
      ntermStack.last().elems.add newTree(curr)

      if toks.finished():
        done = true
      else:
        curr = toks.next()
    else:
      let rule: RuleId = parser.parseTable.getRule(top.nterm, curr)
      let stackadd = parser.grammar.getProductions(rule)
      ntermStack.add TermProgress[C, L, I](
        nterm: rule.head, expected: stackadd.len,
        acts: parser.grammar.getActions(rule))

      stack &= stackadd.reversed()

    while (ntermStack.len > 0) and
          (ntermStack.last().elems.len == ntermStack.last().expected):
      let last = ntermStack.pop()
      if ntermStack.len > 0:
        ntermStack.last().elems.add(
          if last.nterm.generated:
            if parser.retainGenerated:
              newTree(last.nterm.exprRepr(), last.elems)
            else:
              newTree(last.elems)
          else:
            newTree(last.nterm.name, last.elems)
        )
      else:
        result = newTree(last.nterm.name, last.elems)
  #       parseDone = true

  # if parseDone:
  #   return result
