import strutils, strformat, sequtils, sugar, sets, options
import hmisc/helpers
import hmisc/types/seq2d
import hdrawing, hdrawing/term_buf

import parse_tree, token, lexer, bnf_grammars, grammars, bnf_algo,
       parse_primitives


type
  StateId = distinct int
  LRActionKind* = enum
    laShift
    laReduce
    laError
    laAccept

  LRAction* = object
    case kind*: LRActionKind
      of laShift:
        state*: StateId
      of laReduce:
        reduce*: RuleId
      of laError:
        nil
      of laAccept:
        nil

  LRActionTable*[C, L] = object
    table: Table[StateId, ItemLookup[C, L, LRAction]]

  LRSets* = seq[GItemSet]

  LRGotoTable*[C, L] = object
    statecount: int
    # IDEA REVIEW add '2d table' data type for handing things like that
    table: Table[StateId, Table[FlatBnf[C, L], StateId]]

  LRStack*[C, L, I] = seq[tuple[state: StateId, tree: ParseTree[C, L, I]]]


#===========================  Pretty-printing  ===========================#

func exprRepr(lra: LRAction): string =
  case lra.kind:
    of laShift: &"s{lra.state.int}"
    of laReduce: &"r{lra.reduce.exprRepr()}"
    of laError: "err"
    of laAccept: "acc"


#=========================  Misc implementation  =========================#

func `==`(l, r: StateId): bool = (l.int == r.int)

func `[]`[C, L, I](action: LRActionTable[C, L],
                   state: StateId, tok: Token[C, L, I]): LRAction =
  action.table[state][tok]

func `[]`[C, L](goto: LRGotoTable[C, L],
                state: StateId, nterm: BnfNterm): StateId =
  goto.table[state][FlatBnf[C, L](isTerm: false, nterm: nterm)]

func `[]`[C, L](goto: LRGotoTable[C, L],
                state: StateId, tok: ExpectedToken[C, L]): StateId =
  let key = FlatBnf[C, L](isTerm: true, tok: tok)
  try:
    return goto.table[state][key]
  except KeyError:
    if state notin goto.table:
      raise newException(KeyError, &"No values for state {state.int}")
    else:
      raise newException(KeyError,
        &"No transitions for token {tok.exprRepr()} in " &
        &"state {state.int} (key: {key})")


func `[]`[C, L](goto: LRGotoTable[C, L],
                state: StateId,
                sym: FlatBnf[C, L]): StateId =
  goto.table[state][sym.withIt do: it.action = taDefault]
#                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#                   |
#                   Cursed code detected

func contains[C, L](goto: LRGotoTable[C, L],
                    pair: (StateId, ExpectedToken[C, L])): bool =
  (pair[0] in goto.table) and (
    FlatBnf[C, L](isTerm: true, tok: pair[1]) in goto.table[pair[0]])

func `[]=`[C, L](goto: var LRGotoTable[C, L],
                 accs: (StateId, FlatBnf[C, L]),
                 state: StateId): void =
  let key = accs[1].withIt do:
    it.action = taDefault

  # debugecho key
  if accs[0] notin goto.table:
    goto.table[accs[0]] = {key : state}.toTable()
  else:
    goto.table[accs[0]][key] = state

func `[]=`[C, L](actionTbl: var LRActionTable[C, L],
                 state: StateId,
                 tok: ExpectedToken[C, L],
                 action: LRAction): void =
  if state notin actionTbl.table:
    actionTbl.table[state] = initItemLookup[C, L, LRAction]()

  actionTbl.table[state].addItem(makeTokSet(tok), action)


#===============================  Parser  ================================#

type
  SLRParser*[C, L] = object
    grammar: BnfGrammar[C, L]
    action: LRActionTable[C, L]
    goto: LRGotoTable[C, L]

func addMain*[C, L](grammar: BnfGrammar[C, L]): BnfGrammar[C, L] =
  result = grammar
  let start = makeBnfNterm("main_" & grammar.start.name)
  result.rules[start] = @[patt(
    initRuleProd(@[ FlatBnf[C, L](isTerm: false, nterm: grammar.start) ])
  )]

  result.start = start

func makeClosure*[C, L](grammar: BnfGrammar[C, L],
                        initial: GItemSet): GItemSet =
  result = initial
  while true:
    let size = result.len

    var idx = 0
    while idx < result.len:
      let item  = result[idx]
      inc idx
      let next = grammar.nextSymbol(item)
      if next.isNone() or next.get().isTerm:
        discard
      else:
        let sym = next.get()
        for rule in grammar.iterrules(sym.nterm):
          result.append rule

    if size == result.len:
      break

func getGoto*[C, L](grammar: BnfGrammar[C, L],
                    itemset: GItemSet,
                    sym: FlatBnf[C, L]): GItemSet =

  for item in itemset:
    let next = grammar.nextSymbol(item)
    if next.isSome() and next.get() == sym:
      result.add item.withIt do:
        inc it.nextPos
        # result.add next.get().withIt do:
        #   inc it.nextPos

  return grammar.makeClosure(result)


func makeItemsets*[C, L](grammar: BnfGrammar[C, L]): tuple[
  goto: LRGotoTable[C, L], gitems: GItemSets] =
  var
    gitems = @[ makeClosure(grammar, @[ ruleId(grammar.start, 0) ]) ]
    goto: LRGotoTable[C, L]

  let grSymbols = toHashSet: collect(newSeq):
    for ruleid, prod in grammar.iterprods():
      for sym in prod:
          sym

  while true:
    let size = gitems.len

    var idx = 0
    while idx < gitems.len:
      let
        itemset = gitems[idx]
        setid = idx

      inc idx

      for sym in grSymbols:
        let gset = grammar.getGoto(itemset, sym)
        if gset.len > 0 and gset notin gitems:
          gitems.add gset
          goto[(StateId(setid), sym)] = StateId(gitems.len)
          # debugecho sym
          debugecho &"Goto {setid}, {sym.exprRepr()} -> {goto[StateId(setid), sym].int}"
          debugecho gset.exprRepr(grammar)
          # debugecho "\e[41m*=========\e[49m  eee  \e[41m==========*\e[49m"

    if size == gitems.len:
      break

  result.gitems = gitems
  result.goto = goto

func makeAction*[C, L](grammar: BnfGrammar[C, L],
                       goto: LRGotoTable[C, L],
                       gitems: GItemSets): LRActionTable[C, L] =
  let (_, follow, _) = grammar.getSets()
  for gsetid, gset in gitems:
    for item in gset:
      let next = grammar.nextSymbol(item)
      if next.isNone(): # `A -> a•`
        for tok in follow[item.ruleId.head]:
          result[StateId(gsetid), tok] = LRAction(
            kind: laReduce,
            reduce: item.ruleId
          )
      else: # `A -> αa•β`
        let sym = next.get()
        if sym.isTerm:
          # debugecho gsetid, " ", sym.tok.exprRepr()
          # debugecho sym
          if (StateId(gsetid), sym.tok) in goto:
            result[StateId(gsetid), sym.tok] = LRAction(
              kind: laShift,
              state: goto[StateId(gsetid), sym.tok]
            )

func newSLRParser*[C, L](grammar: Grammar[C, L]): SLRParser[C, L] =
  result.grammar = grammar.toBNF().addMain()
  let (goto, gitems) = result.grammar.makeItemsets()
  # {.noSideEffect.}:
  #   printItems(result.grammar, gitems)

  result.goto = goto
  result.action = result.grammar.makeAction(goto, gitems)


func gridRepr*[C, L](action: LRActionTable[C, L]): Seq2D[string] =
  # let states: seq[StateId] = collect(newSeq):
  #   for state, _ in action.table:
  #     state

  let headermap: Table[ExpectedToken[C, L], int] = block:
    let keys = collect(newSeq):
      for _, tokmap in action.table:
        for tok, _ in tokmap:
          tok

    collect(initTable):
      for colIdx, key in deduplicate(keys):
        # debugecho colIdx, " ", key.exprRepr()
        {key : colIdx + 1}

  result.fillToSize(rows = action.table.len + 4,
                    cols = headermap.len + 1, default = "")
  for state, tokmap in action.table:
    result[state.int + 1, 0] = $state.int
    for tok, action in tokmap:
      # debugecho state.int + 1, ", ", headermap[tok], " -> ", tok.exprRepr()
      result[state.int + 1, headermap[tok]] = action.exprRepr()

  for key, colIdx in headermap:
    result[0, colIdx] = key.exprRepr()

template top*[T](s: var seq[T]): var T = s[^1]

proc parse*[C, L, I](
  parser: SLRParser[C, L],
  toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I] =
  echo newTermGrid(
    (0,0),
    parser.action.gridRepr().toTermBufGrid(),
    makeThinLineGridBorders()
  ).toTermBuf().toString()

  var
    curr: Token[C, L, I] = toks.next()
    stack: LRStack[C, L, I] = @[(StateId(0), newTree[C, L, I](@[]))]

  while true:
    let
      s = stack.top().state
      action = parser.action[s, curr]

    case action.kind:
      of laShift:
        stack.add (action.state, newTree(curr))
        curr = toks.next()
      of laReduce:
        let
          rule = action.reduce
          reduction = parser.grammar.getProductions(rule)
          popped = collect(newSeq):
            for _ in 0 ..< (reduction.len - 2):
              stack.pop()

        stack.add (
          parser.goto[s, rule.head],
          newTree(rule.head.exprRepr(), popped.mapIt(it.tree))
        )
      of laAccept:
        # TODO return tree
        break
      of laError:
        discard

import grammar_dsl

when isMainModule:
  let grammar = initGrammar[NoCategory, string]():
    E ::= (E & "+" & T) | (T)
    T ::= (T & "*" & F) | (F)
    F ::= "(" & E & ")"
    F ::= "id"


  let parser = newSLRParser[NoCategory, string](grammar.toGrammar())
  let tree = makeTokens(
    @["id", "*", "id", "+", "id"]).makeStream().withResIt:
      parser.parse(it)

  echo "done"
