import strutils, strformat, sequtils, sugar, sets, options
import hmisc/helpers

import parse_tree, token, lexer, bnf_grammars, grammars


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

  LRGotoTable* = object
    # IDEA REVIEW add '2d table' data type for handing things like that
    table: Table[StateId, Table[BnfNterm, StateId]]

  LRStack*[C, L, I] = seq[tuple[state: StateId, tree: ParseTree[C, L, I]]]


#=========================  Misc implementation  =========================#

func `==`(l, r: StateId): bool = (l.int == r.int)

func `[]`[C, L, I](action: LRActionTable[C, L],
                   state: StateId, tok: Token[C, L, I]): LRAction =
  action.table[state][tok]

func `[]`(goto: LRGotoTable, state: StateId, nterm: BnfNterm): StateId =
  goto.table[state][nterm]

#===============================  Parser  ================================#

type
  SLRParser*[C, L] = object
    grammar: BnfGrammar[C, L]
    action: LRActionTable[C, L]
    goto: LRGotoTable

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

func getGoto*[C, L](gotoNow: var LRGotoTable,
                    itemset: GItemSet,
                    sym: FlatBnf[C, L]): tuple[
                      isNew: bool, gset: GItemSet] =
  discard

func makeItemsets*[C, L](grammar: BnfGrammar[C, L]): tuple[
  goto: LRGotoTable, gitems: GItemSets] =
  var
    gitems = @[ makeClosure(grammar, @[ ruleId(grammar.start, 0) ]) ]
    goto: LRGotoTable

  let grSymbols = toHashSet: collect(newSeq):
    for ruleid, prod in grammar.iterprods():
      for sym in prod:
          sym

  while true:
    let size = gitems.len

    for itemset in gitems:
      for sym in grSymbols:
        let (isNew, gset) = goto.getGoto(itemset, sym)
        if isNew and gset.len > 0:
          gitems.add gset

    if size == gitems.len:
      break

  result.gitems = gitems

func makeAction*[C, L](grammar: BnfGrammar[C, L],
                       goto: LRGotoTable,
                       gitems: GItemSets): LRActionTable[C, L] =
  discard

func newSLRParser*[C, L](grammar: Grammar[C, L]): SLRParser[C, L] =
  result.grammar = grammar.toBNF().addMain()
  let (goto, gitems) = result.grammar.makeItemsets()
  {.noSideEffect.}:
    printItems(result.grammar, gitems)

  result.goto = goto
  result.action = result.grammar.makeAction(goto, gitems)



template top*[T](s: var seq[T]): var T = s[^1]

proc parse*[C, L, I](
  parser: SLRParser[C, L],
  toks: var TokStream[Token[C, L, I]]): ParseTree[C, L, I] =
  var
    curr: Token[C, L, I] = toks.next()
    stack: LRStack[C, L, I]

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
