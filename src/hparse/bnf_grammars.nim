## Types/functions for BNF grammars

import grammars, token
import parse_primitives
import sets, hashes, sequtils, strformat, strutils, options
import hmisc/helpers
import hmisc/algo/clformat
import hmisc/types/colorstring
import hasts/graphviz_ast

#*************************************************************************#
#**************************  Type declarations  **************************#
#*************************************************************************#

#=============================  Primitives  ==============================#

type
  BnfPattKind* = enum
    ## Types of patters in BNF grammar
    bnfEmpty
    bnfTerm
    bnfNTerm
    bnfConcat
    bnfAlternative

  FixupFlag* = distinct bool

  BnfNTerm* = object
    case generated*: bool
      of true:
        idx*: seq[int]
        parent*: NtermSym
      of false:
        name*: NtermSym

  GSym*[C, L] = object
    action*: TreeAct
    case isTerm*: bool # REFACTOR rename to `isToken`
      of false:
        nterm*: BnfNterm
      of true:
        tok*: ExpectedToken[C, L] ## Token kind

let fixupAllow: FixupFlag = FixupFlag(true)
let fixupDisallow: FixupFlag = FixupFlag(false)
# func `==`(l, r: FixupFlag): bool = (l.bool == r.bool)

#*************************************************************************#
#***********************  Rule production symbols  ***********************#
#*************************************************************************#
type
  RuleProd*[C, L] = object
    # actions*: ActLookup
    symbols*: seq[GSym[C, L]]

func initRuleProd*[C, L](symbols: seq[GSym[C, L]]): RuleProd[C, L] =
  result.symbols = symbols

func concat*[C, L](lhs, rhs: RuleProd[C, L]): RuleProd[C, L] =
  result.symbols = lhs.symbols & rhs.symbols
  # result.actions = lhs.actions

  # for pos, action in rhs.actions:
  #   result.actions[pos + lhs.symbols.len] = action

func getActions*[C, L](prod: RuleProd[C, L]): ActLookup =
  for idx, sym in prod.symbols:
    if sym.action != taDefault:
      result[idx] = sym.action

iterator items*[C, L](prod: RuleProd[C, L]): GSym[C, L] =
  for sym in prod.symbols:
    yield sym

iterator pairs*[C, L](prod: RuleProd[C, L]): (int, GSym[C, L]) =
  for idx, sym in prod.symbols:
    yield (idx, sym)


func last*[C, L](prod: RuleProd[C, L]): GSym[C, L] = prod.symbols[^1]
func len*[C, L](prod: RuleProd[C, L]): int = prod.symbols.len
func reversed*[C, L](prod: RuleProd[C, L]): seq[GSym[C, L]] =
  prod.symbols.reversed()

func `[]`*[C, L](prod: RuleProd[C, L], idx: int): GSym[C, L] =
  prod.symbols[idx]

#=====================  Grammar & grammar elements  ======================#

type
  BnfPatt*[C, L] = ref object # REVIEW is it necessary to use `ref`?
    ## Recursive of flat bnf pattern
    case flat*: bool
      of false:
        case kind*: BnfPattKind
          of bnfEmpty:
            nil
          of bnfNterm:
            action*: TreeAct
            nterm*: BnfNTerm ## Nonterminal to parse
          of bnfTerm:
            tokAction*: TreeAct
            tok*: ExpectedToken[C, L] ## Single token to match literally
          of bnfAlternative, bnfConcat:
            actions*: ActLookup
            patts*: seq[BnfPatt[C, L]] ## Concatenation/list of alternatives
      of true:
        elems*: RuleProd[C, L] ## Flat bnf - concatenation of
                               ## (non)terminals

  BnfRule*[C, L] = object
    ## Single rule with one production
    nterm*: BnfNterm ## Nonterminal name
    patt*: BnfPatt[C, L] ## Elements

  RuleId* = object
    ## Nonterminal head and alternative index
    head*: BnfNterm
    alt*: int ## Index of alternative in `rules` field in grammar

  BnfGrammar*[C, L] = object
    start*: BnfNterm ## Start element in grammar
    rules*: Table[BnfNterm, seq[BnfPatt[C, L]]] ## Bnf rule and sequence
    ## of alternatives. Each item in sequence is expected to be
    ## `BnfPatt.flat == true`.


#============================  Aux functions  ============================#

func hash*(nterm: BnfNTerm): Hash =
  var h: Hash = 0
  h = h !& hash(nterm.generated)
  case nterm.generated:
    of true:
      h = h !& hash(nterm.parent)
      h = h !& hash(nterm.idx)
    of false:
      h = h !& hash(nterm.name)

  result = !$h

func hash*(id: RuleId): Hash =
  var h: Hash = 0
  h = h !& hash(id.head) !& hash(id.alt)
  result = !$h

func hash*[C, L](key: GSym[C, L]): Hash =
  var h: Hash = 0
  h = h !& hash(key.action) !& hash(key.isTerm)
  if key.isTerm:
    h = h !& hash(key.tok)
  else:
    h = h !& hash(key.nterm)

  result = !$h


func `==`*[C, L](l, r: GSym[C, L]): bool =
  (l.action == r.action) and (l.isTerm == r.isTerm) and (
    block:
      if l.isTerm:
        l.tok == r.tok
      else:
        l.nterm == r.nterm
  )

func `==`*(lhs, rhs: BnfNterm): bool =
  lhs.generated == rhs.generated and (
    block:
      if lhs.generated:
        (lhs.parent == rhs.parent) and
        (lhs.idx == rhs.idx)
      else:
        (lhs.name == rhs.name)
  )

#============================  Constructors  =============================#

func makeGrammar*[C, L](rules: seq[BnfRule[C, L]]): BnfGrammar[C, L] =
  ## Construction grammar from sequence of rules
  mixin contains, hash
  for rule in rules:
    if rule.nterm notin result.rules:
      result.rules[rule.nterm] = @[rule.patt]
    else:
      result.rules[rule.nterm].add rule.patt


# TODO rename into `makeRuleId`
func ruleId*(nterm: BnfNterm, alt: int): RuleId =
  ## Make new rule id
  RuleId(head: nterm, alt: alt)

func makeBnfNterm(parent: string, idx: seq[int]): BnfNTerm =
  BnfNterm(generated: true, idx: idx, parent: parent)

func makeBnfNterm*(name: string): BnfNTerm =
  BnfNterm(generated: false, name: name)

func rule*[C, L](nterm: BnfNterm, patt: BnfPatt[C, L]): BnfRule[C, L] =
  ## Construct new BNF rule using `nterm` as head and `patt` as production
  BnfRule[C, L](nterm: nterm, patt: patt)

func patt*[C, L](elems: RuleProd[C, L]): BnfPatt[C, L] =
  BnfPatt[C, L](flat: true, elems: elems)

func patt*[C, L](): BnfPatt[C, L] = BnfPatt[C, L](flat: true)

#==============================  Accessors  ==============================#

iterator iterrules*[C, L](grammar: BnfGrammar[C, L]): tuple[
  id: RuleId, alt: BnfPatt[C, L]] =
  for head, patts in grammar.rules:
    for altId, alt in patts:
      yield (id: ruleId(head, altId), alt: alt)

iterator iterprods*[C, L](grammar: BnfGrammar[C, L]): tuple[
  id: RuleId, prod: RuleProd[C, L]] =
  for head, patts in grammar.rules:
    for altId, alt in patts:
      yield (id: ruleId(head, altId), prod: alt.elems)


iterator iterrules*[C, L](grammar: BnfGrammar[C, L], head: BnfNterm): RuleId =
  for altId, _ in grammar.rules[head]:
    yield ruleId(head, altId)

func ruleBody*[C, L](grammar: BnfGrammar[C, L],
                     ruleId: RuleId): RuleProd[C, L] =
  return grammar.rules[ruleId.head][ruleId.alt].elems

#===================  Conversion from regular grammar  ===================#


func subrules*[C, L](patt: Patt[C, L]): seq[Patt[C, L]] =
  case patt.kind:
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      @[patt.opt]
    of pkAlternative, pkConcat:
      patt.patts
    else:
      raiseAssert(msgjoin("Invalid patt: {patt.kind} does not have subrules"))

func isNested*[C, L](patt: Patt[C, L]): bool =
  patt.kind in {pkAlternative .. pkOneOrMore}

func toBNF*[C, L](
  patt: Patt[C, L],
  parent: string,
  idx: seq[int],
  dofixup: FixupFlag): tuple[
    toprule: BnfPatt[C, L],
    newrules: seq[BnfRule[C, L]]] =

  case patt.kind:
    of pkTerm:
      # debugecho patt.exprRepr()
      result.toprule = BnfPatt[C, L](
        flat: false,
        kind: bnfTerm,
        tok: patt.tok,
        tokAction: patt.action
      )
    of pkNterm:
      result.toprule = BnfPatt[C, L](
        flat: false,
        kind: bnfNTerm,
        nterm: makeBnfNterm(patt.nterm),
        action: patt.action
      )
    of pkAlternative, pkConcat:
      var newsubp: seq[BnfPatt[C, L]]

      for pos, subp in patt.patts:
        let (bnfPatt, bnfRules) = subp.toBNF(
          parent, idx = idx & @[pos], dofixup = dofixup)
        newsubp.add bnfPatt
        result.newrules &= bnfRules

      if patt.kind == pkAlternative:
        result.toprule = BnfPatt[C, L](
          flat: false, kind: bnfAlternative, patts: newsubp)
      else:
        result.toprule = BnfPatt[C, L](
          flat: false, kind: bnfConcat, patts: newsubp)

      for idx, sym in patt.patts:
        result.toprule.actions[idx] = sym.action
    of pkZeroOrMore:
      # debugecho patt.exprRepr()
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[C, L](
        flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(
        patt.opt, parent, idx & @[0], dofixup = dofixup)
      result.newrules = @[
        BnfRule[C, L](
          nterm: newsym,
          patt: BnfPatt[C, L](
            flat: false,
            kind: bnfAlternative,
            patts: @[
              BnfPatt[C, L](flat: false, kind: bnfEmpty),
              BnfPatt[C, L](flat: false, kind: bnfConcat,
                            actions:
                              block:
                                if dofixup.bool:
                                  {
                                    0 : patt.action,
                                    1 : taSpliceDiscard
                                  }.toTable()
                                else:
                                  { 0 : patt.action }.toTable()
                            ,
                            patts: @[
                body,
                BnfPatt[C, L](flat: false, kind: bnfNterm, nterm: newsym)
        ])]))
      ] & subnewrules
      # debugecho " @@@"
      # debugecho patt.exprRepr()
      # debugecho result.toprule.exprRepr()
      # for new in result.newrules:
      #   debugecho "new:", new.exprRepr()
      # debugecho " ###"
    of pkOneOrMore:
      # IMPLEMENT
      # NOTE I'm not 100% sure if this is correct way to convert
      # one-or-more to bnf
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[C, L](flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(
        patt.opt, parent, idx & @[0], dofixup)
      result.newrules = @[
        BnfRule[C, L](
          nterm: newsym,
          patt: BnfPatt[C, L](
            flat: false,
            kind: bnfConcat,
            patts: @[
              body,
              BnfPatt[C, L](flat: false, kind: bnfAlternative, patts: @[
                BnfPatt[C, L](flat: false, kind: bnfEmpty),
                BnfPatt[C, L](flat: false, kind: bnfNterm, nterm: newsym)
        ])]))
      ] & subnewrules
    of pkOptional:
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[C, L](flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(
        patt.opt, parent, idx & @[0], dofixup = dofixup)
      result.newrules = @[
        BnfRule[C, L](
          nterm: newsym,
          patt: BnfPatt[C, L](
            flat: false,
            kind: bnfAlternative,
            patts: @[BnfPatt[C, L](flat: false, kind: bnfEmpty), body]))
      ] & subnewrules


func flatten[C, L](patt: BnfPatt[C, L]): seq[RuleProd[C, L]] =
 if patt.flat:
   return @[ patt.elems ]
 else:
   case patt.kind:
     of bnfEmpty:
       return @[ initRuleProd[C, L](@[]) ]
     of bnfTerm:
       return @[ initRuleProd(@[
         GSym[C, L](
           isTerm: true,
           tok: patt.tok,
           action: patt.tokAction
       )])]
     of bnfNterm:
       return @[ initRuleProd(@[
         GSym[C, L](
           isTerm: false,
           nterm: patt.nterm,
           action: patt.action
       )])]
     of bnfConcat:
       # debugecho patt.exprRepr()
       for idx, sub in patt.patts:
         var newpatts: seq[RuleProd[C, L]]
         for flat in sub.flatten():
           let flat = flat.withIt:
             if idx in patt.actions:
               it.symbols[0].action = patt.actions[idx]

           if result.len == 0:
             newpatts.add flat
           else:
             for val in result: # Concatenate head with alternative
               newpatts.add concat(val, flat)

         result = newpatts
       # for it in result:
       #   debugecho it.exprRepr()
     of bnfAlternative:
       for alt in patt.patts:
         result &= alt.flatten()


func toBNF*[C, L](
  rule: Rule[C, L],
  noAltFlatten: bool = false,
  renumerate: bool = false,
  dofixup: FixupFlag = fixupAllow): seq[BnfRule[C, L]] =
  # debugecho rule.exprRepr()
  let (top, newrules) = rule.patts.toBnf(
    rule.nterm, @[0], dofixup = dofixup)
  if noAltFlatten:
    block:
      let topflat = top.flatten()
      if topflat.len == 0:
        result.add rule(makeBNFNterm(rule.nterm), patt[C, L]())
      else:
        for elems in topflat:
          mixin makeBnfNterm, patt
          #[ IMPLEMENT expression cannot be called error ]#
          # FIXME XXXX
          result.add rule(makeBnfNterm(rule.nterm), patt(elems))

    for rule in newrules:
      let newpatts = rule.patt.flatten()
      for idx, elems in newpatts:
        let nterm = makeBnfNterm(rule.nterm.parent, rule.nterm.idx)
        # result.add(rule(nterm, elems)) # ERROR expression
        # `rule(nterm, elems)` cannot be called
        result.add(rule(nterm, patt(elems))) # FIXME XXXX WTF
  else:
    result.add rule(makeBnfNterm(rule.nterm), top) # FIXME XXXX
    result &= newrules

  if renumerate:
    #[ IMPLEMENT replace nonterminal names inside rules too ]#
    #[ IMPLEMENT do not change numbering of non-generated terms ]#
    for idx, rule in result:
      if rule.nterm.generated:
        var tmp = rule
        tmp.nterm.idx = @[idx]
        result[idx] = tmp

func toBNF*[C, L](
  grammar: Grammar[C, L], dofixup: FixupFlag = fixupAllow): BnfGrammar[C, L] =
  mixin toBNF
  result = makeGrammar(
    grammar.rules.mapIt(it.toBNF(
      noAltFlatten = true,
      renumerate = false,
      dofixup = dofixup
    )).concat())

  result.start = BnfNterm(generated: false, name: grammar.start)
  # debugecho result.exprRepr()



#========================  Predicates/accessors  =========================#

func isEmpty*[C, L](patt: BnfPatt[C, L]): bool =
  ## Check if pattern describes empty production
  (patt.elems.len == 0) # and (patt.elems[0].kind == fbkEmpty)

func `[]`*[C, L](grammar: BnfGrammar[C, L], rule: RuleId): BnfPatt[C, L] =
  ## Get BNF pattern for rule
  grammar.rules[rule.head][rule.alt]

func getProductions*[C, L](
  grammar: BnfGrammar[C, L], id: RuleId): RuleProd[C, L] =
  ## Get list of productions from flat bnf pattern at `id`
  # debugecho id.exprRepr()
  grammar.rules[id.head][id.alt].elems

func getActions*[C, L](grammar: BnfGrammar[C, L],
                       id: RuleId): ActLookup =
  grammar.rules[id.head][id.alt].actions

func first*[C, L](patt: BnfPatt[C, L]): GSym[C, L] =
  assert patt.flat
  return patt.elems[0]


#===========================  Pretty-printing  ===========================#


func exprRepr*(nterm: BnfNTerm, normalize: bool = false): string =
  if nterm.generated:
    if normalize:
      fmt("{nterm.parent.toUpperAscii()}{nterm.idx.join(\"\")}")
    else:
      fmt("{nterm.parent}{nterm.idx.join(\"_\")}")
  else:
    if normalize:
      nterm.name.toUpperAscii()
    else:
      nterm.name

func exprRepr*[C, L](
  fbnf: GSym[C, L],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  if fbnf.isTerm:
    fbnf.action.exprRepr(conf) & fbnf.tok.exprRepr(conf)
  else:
    mixin toYellow
    (
      (fbnf.action != taDefault).tern(fbnf.action.exprRepr(conf), "") &
      toYellow(
        fbnf.nterm.exprRepr(conf.normalizeNterms),
        conf.colored
      )
    ).wrap(conf.ntermWrap)

func exprRepr*[C, L](
  fbnf: seq[GSym[C, L]],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  fbnf.mapIt(it.exprRepr(conf)).join(conf.concatSep)

func exprRepr*[C, L](
  bnf: BnfPatt[C, L],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  if bnf.flat:
    if bnf.isEmpty:
      conf.emptyProd
    else:
      bnf.elems.mapIt(exprRepr(it, conf)).join(conf.concatSep)
  else:
    case bnf.kind:
      of bnfEmpty:
        conf.emptyProd
      of bnfTerm:
        bnf.tok.exprRepr(conf)
      of bnfNTerm:
        toYellow(bnf.nterm.exprRepr(), conf.colored).wrap(conf.ntermWrap)
      of bnfAlternative, bnfConcat:
        var buf: seq[string]
        for idx, subp in bnf.patts:
          if idx in bnf.actions:
            buf.add &"{bnf.actions[idx].exprRepr(conf)}{subp.exprRepr(conf)}"
          else:
            buf.add &"{subp.exprRepr(conf)}"


        buf.join(
          (bnf.kind == bnfConcat).tern(conf.concatSep, conf.alternSep)
        ).wrap("{}")


func exprRepr*[C, L](
  rule: BnfRule[C, L],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  let head = rule.nterm.exprRepr(conf.normalizeNterms)
  return fmt("{head:<12} {conf.prodArrow} {rule.patt.exprRepr(conf)}")


func exprRepr*[C, L](
  grammar: BnfGrammar[C, L],
  nojoin: bool = false,
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  mixin toRomanNumeral
  var buf: seq[string]
  if nojoin:
    for head, patts in grammar.rules:
      for idx, alt in patts:
        let head =
          if conf.normalizeNterms:
            fmt("{head.exprRepr(true):<12}")
          else:
            if conf.enumerateAlts:
              let idx = toRomanNumeral(idx + 1)
              fmt("{head.exprRepr():<10} ({idx})")
            else:
              fmt("{head.exprRepr():<10}")

        buf.add fmt("{head:<16} {conf.prodArrow} {alt.exprRepr(conf)}")

  else:
    for head, patts in grammar.rules:
      let head = head.exprRepr(conf.normalizeNterms)
      buf.add fmt("{head}  {conf.prodArrow} ")
      for idx, alt in patts:
        buf.add fmt(".{idx}{conf.alternSep}{alt.exprRepr(conf)}")
      buf.add ""

  return buf.join("\n")

func exprRepr*(id: RuleId, normalize: bool = false): string =
  if normalize:
    fmt("{id.head.exprRepr(true)}{id.alt}")
  else:
    fmt("{id.head.exprRepr(false)}.{id.alt}")

func exprRepr*[C, L](prod: RuleProd[C, L],
                     conf: GrammarPrintConf = defaultGrammarPrintConf
                    ): string =
  # var buf: seq[string]
  # for idx, sym in prod:
  #   buf.add fmt("{it.exprRepr(conf)}")
  prod.mapIt(it.exprRepr(conf)).join(" ")


func `$`*(id: RuleId): string = id.exprRepr()
func `$`*(nterm: BnfNterm): string = nterm.exprRepr()

#*************************************************************************#
#*****************************  Rule lookup  *****************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

type
  ItemLookup*[C, L, Item] = object
    ## Lookup table for rules. Mapping `Token -> RuleId`
    rules: seq[Item]
    tokMap: TokLookup[C, L]

#=============================  Predicates  ==============================#

#==============================  Accessors  ==============================#


func getItem*[C, L, I, Item](
  rlookup: ItemLookup[C, L, Item], tok: Token[C, L, I]): Item =
  ## Get single rule from lookup table. Raise exception on rule
  ## conflict.
  let idx: int = rlookup.tokMap.getAlt(tok)
  # func getElemByIdx*[T](s: seq[T], i: int): T = s[i]
  static:
    # let s: seq[Item] = rlookup.rules
    assert (idx is int) and (rlookup.rules is seq[Item])
    assert system.`[]`(rlookup.rules, idx) is Item
      # $typeof(rlookup.rules) & " " & $typeof(rlookup.rules[idx])

  return system.`[]`(rlookup.rules, idx)
  # static:
  #   echo typeof(rlookup.rules)
  #   echo typeof(rlookup.tokMap.getAlt(tok))
  #   echo typeof(rlookup.rules[idx])

  # rlookup.rules[rlookup.tokMap.getAlt(tok)]

func `[]`*[C, L, I, Item](
  lookup: ItemLookup[C, L, Item],
  tok: Token[C, L, I]): Item = lookup.getItem(tok)

func addItem*[C, L, Item](
  rl: var ItemLookup[C, L, Item], first: TokSet[C, L],
  ruleId: Item, canconflict: bool = false): void =
  ## Add new rule to lookup table
  let idx = rl.rules.len
  rl.rules.add ruleId
  for tok in items(first):
    # debugecho tok.exprRepr(), " -> ", ruleId.exprRepr(), " id: ", idx
    rl.tokMap.addAlt(tok, idx, canconflict = canconflict)

iterator pairs*[C, L, Item](il: ItemLookup[C, L, Item]
                           ): (ExpectedToken[C, L], Item) =
  for key, idx in il.tokMap:
    yield (key, il.rules[idx])

#============================  Constructors  =============================#
func initItemLookup*[C, L, Item](): ItemLookup[C, L, Item] =
  ItemLookup[C, L, Item](tokMap: initTokLookup[C, L]())


#========================  Other implementation  =========================#

#===========================  Pretty-printing  ===========================#


#*************************************************************************#
#*************************  Items and item sets  *************************#
#*************************************************************************#

type
  GItem* = object
    ruleId*: RuleId
    startPos*: int
    nextPos*: int

  GItemSet* = object
    id*: int
    gitems*: seq[GItem]

  GItemSets* = seq[GItemSet]

converter toGItemSet*(rules: seq[RuleId]): GItemSet =
  for it in rules:
    result.gitems.add GItem(ruleId: it)

func nextSymbol*[C, L](gr: BnfGrammar[C, L],
                       item: GItem): Option[GSym[C, L]] =
  if gr.ruleBody(item.ruleId).len > item.nextPos:
    some(gr.ruleBody(item.ruleId)[item.nextPos])
  else:
    none(GSym[C, L])

func len*(itemset: GItemSet): int = itemset.gitems.len
func `[]`*(itemset: GItemSet, idx: int): GItem = itemset.gitems[idx]
iterator items*(itemset: GItemSet): GItem =
  for it in itemset.gitems:
    yield it

func append*[A](a: var seq[A], b: A): void =
  for it in a:
    if it == b:
      return

  a.add b

func append*(itemset: var GItemSet, item: GItem): void =
  itemset.gitems.append item

func append*(itemset: var GItemSet, rule: RuleId): void =
  itemset.gitems.append GItem(ruleId: rule)

func add*(itemset: var GItemSet, item: GItem): void =
  # debugecho itemset.gitems.len
  itemset.gitems.add item
  # debugecho itemset.gitems.len
  # debugecho "\e[41m*=========\e[49m  sdfa  \e[41m=========*\e[49m"

func exprRepr*[C, L](stateset: GItemSet,
                     gr: BnfGrammar[C, L],
                     conf: GrammarPrintConf = defaultGrammarPrintConf,
                     onlyFull: bool = false,
                     asEarley: bool = false): string =

  var resbuf: seq[string]
  let maxw = stateset.maxIt(it.ruleId.exprRepr().termLen())
  for item in stateset:
    if (item.nextPos == gr.ruleBody(item.ruleId).len) or (not onlyFull):
      var buf = item.ruleId.exprRepr(conf.normalizeNterms
          ).termAlignLeft(maxw) & " ->"
      for idx, sym in gr.ruleBody(item.ruleId):
        if idx == item.nextPos:
          buf &= " ⦿".toMagenta(conf.colored) & sym.exprRepr(conf)
        else:
          buf &= " " & sym.exprRepr(conf)

        # buf &= " " &

      if asEarley:
        buf = termAlignLeft(buf, 60)
        if item.nextPos == gr.ruleBody(item.ruleId).len:
          buf = fmt("{buf} \e[4m#\e[24m ({item.startPos})")
        else:
          buf = fmt("{buf}   ({item.startPos})")
      else:
        if item.nextPos == gr.ruleBody(item.ruleId).len:
          buf = buf & " ⦿".toMagenta(conf.colored)

      resbuf.add buf

  return resbuf.join("\n")

func dotNodeRepr*[C, L](stateset: GItemSet,
                        gr: BnfGrammar[C, L],
                        conf: GrammarPrintConf = defaultGrammarPrintConf):
                          Node =
  result = makeNode(
    stateset.id.toNodeId(),
    label =
      "      " & $stateset.id & "      \n" &
      stateset.exprRepr(gr, conf.withIt do: it.colored = false)
  )

  result.labelAlign = nlaLeft

proc printItems*[C, L](gr: BnfGrammar[C, L],
                       state: GItemSets, onlyFull: bool = false): void =
  echo "\e[31mSTATE :\e[39m"
  for idx, stateset in state:
    echo fmt("   === {idx:^3} ===   ")
    echo stateset.exprRepr(gr)
