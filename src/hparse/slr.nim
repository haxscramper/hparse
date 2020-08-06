import strutils, strformat, sequtils, sugar
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
    actions: seq[LRAction]
    table: Table[StateId, TokLookup[C, L]]

  LRGotoTable* = object
    # IDEA REVIEW add '2d table' data type for handing things like that
    table: Table[StateId, Table[BnfNterm, StateId]]

  LRStack*[C, L, I] = seq[tuple[state: StateId, tree: ParseTree[C, L, I]]]


#===============================  Parser  ================================#

type
  SLRParser*[C, L] = object
    grammar: BnfGrammar[C, L]
    action: LRActionTable[C, L]
    goto: LRGotoTable

func addMain*[C, L](grammar: BnfGrammar[C, L]): BnfGrammar[C, L] =
  result = grammar
  result.rules[makeBnfNterm("main_" & grammar.start.name)] = @[
    initRuleProd(FlatBnf(isTerm: false, nterm: grammar.start))
  ]

func makeClosure*[C, L](grammar: BnfGrammar[C, L]): GItemSet =
  discard

func makeGoto*[C, L](grammar: BnfGrammar[C, L]): LRGotoTable =
  discard

func newSLRParser*[C, L](grammar: Grammar[C, L]): SLRParser[C, L] =
  result.grammar = grammar.toBNF().addMain()
  result.goto = result.gammar.makeGoto()
  result.action = result.grammar.makeAction(result.goto)



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
          newTree(rule.head, popped)
        )
      of laAccept:
        # TODO return tree
        break
      of laError:
        discard
