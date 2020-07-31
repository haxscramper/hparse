## Types/functions for BNF grammars

import grammars, token
import parse_primitives
import sets, hashes, sequtils, strformat, strutils
import hmisc/helpers
import hmisc/algo/clformat

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

  BnfNTerm* = object
    case generated*: bool
      of true:
        idx*: seq[int]
        parent*: NtermSym
      of false:
        name*: NtermSym

  FlatBnf*[C, L] = object
    action*: TreeAct
    case isTerm*: bool
      of false:
        nterm*: BnfNterm
      of true:
        tok*: ExpectedToken[C, L] ## Token kind

#=====================  Grammar & grammar elements  ======================#

type
  BnfPatt*[C, L] = ref object # REVIEW is it necessary to use `ref`?
    ## Recursive of flat bnf pattern
    action*: TreeAct
    case flat*: bool
      of false:
        case kind*: BnfPattKind
          of bnfEmpty:
            nil
          of bnfNterm:
            nterm*: BnfNTerm ## Nonterminal to parse
          of bnfTerm:
            tok*: ExpectedToken[C, L] ## Single token to match literally
          of bnfAlternative, bnfConcat:
            patts*: seq[BnfPatt[C, L]] ## Concatenation/list of alternatives
      of true:
        elems*: seq[FlatBnf[C, L]] ## Flat bnf - concatenation of (non)terminals

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

func makeBnfNterm(name: string): BnfNTerm =
  BnfNterm(generated: false, name: name)

func rule*[C, L](nterm: BnfNterm, patt: BnfPatt[C, L]): BnfRule[C, L] =
  ## Construct new BNF rule using `nterm` as head and `patt` as production
  BnfRule[C, L](nterm: nterm, patt: patt)

func patt*[C, L](elems: seq[FlatBnf[C, L]]): BnfPatt[C, L] =
  BnfPatt[C, L](flat: true, elems: elems)

#==============================  Accessors  ==============================#

iterator iterrules*[C, L](grammar: BnfGrammar[C, L]): tuple[
  id: RuleId, alt: BnfPatt[C, L]] =
  for head, patts in grammar.rules:
    for altId, alt in patts:
      yield (id: ruleId(head, altId), alt: alt)

iterator iterrules*[C, L](grammar: BnfGrammar[C, L], head: BnfNterm): RuleId =
  for altId, _ in grammar.rules[head]:
    yield ruleId(head, altId)

func ruleBody*[C, L](grammar: BnfGrammar[C, L],
                     ruleId: RuleId): seq[FlatBnf[C, L]] =
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
  idx: seq[int] = @[]): tuple[
    toprule: BnfPatt[C, L],
    newrules: seq[BnfRule[C, L]]] =

  case patt.kind:
    of pkTerm:
      result.toprule = BnfPatt[C, L](flat: false, kind: bnfTerm, tok: patt.tok)
    of pkNterm:
      result.toprule = BnfPatt[C, L](
        flat: false,
        kind: bnfNTerm,
        nterm: makeBnfNterm(patt.nterm)
      )
    of pkAlternative, pkConcat:
      var newsubp: seq[BnfPatt[C, L]]

      for pos, subp in patt.patts:
        let (bnfPatt, bnfRules) = subp.toBNF(parent, idx = idx & @[pos])
        newsubp.add bnfPatt
        result.newrules &= bnfRules

      if patt.kind == pkAlternative:
        result.toprule = BnfPatt[C, L](flat: false, kind: bnfAlternative, patts: newsubp)
      else:
        result.toprule = BnfPatt[C, L](flat: false, kind: bnfConcat, patts: newsubp)
    of pkZeroOrMore:
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[C, L](flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[C, L](
          nterm: newsym,
          patt: BnfPatt[C, L](
            flat: false,
            kind: bnfAlternative,
            patts: @[
              BnfPatt[C, L](flat: false, kind: bnfEmpty),
              BnfPatt[C, L](flat: false, kind: bnfConcat, patts: @[
                body,
                BnfPatt[C, L](flat: false, kind: bnfNterm, nterm: newsym)
        ])]))
      ] & subnewrules
    of pkOneOrMore:
      # IMPLEMENT
      # NOTE I'm not 100% sure if this is correct way to convert
      # one-or-more to bnf
      let newsym = makeBnfNterm(parent, idx)
      result.toprule = BnfPatt[C, L](flat: false, kind: bnfNterm, nterm: newsym)
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
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
      let (body, subnewrules) = toBNF(patt.opt, parent, idx & @[0])
      result.newrules = @[
        BnfRule[C, L](
          nterm: newsym,
          patt: BnfPatt[C, L](
            flat: false,
            kind: bnfAlternative,
            patts: @[BnfPatt[C, L](flat: false, kind: bnfEmpty), body]))
      ] & subnewrules


func flatten[C, L](patt: BnfPatt[C, L]): seq[seq[FlatBnf[C, L]]] =
 if patt.flat:
   return @[ patt.elems ]
 else:
   case patt.kind:
     of bnfEmpty:
       return @[ emptySeq[FlatBnf[C, L]]() ]
     of bnfTerm:
       return @[ @[ FlatBnf[C, L](isTerm: true, tok: patt.tok) ] ]
     of bnfNterm:
       return @[ @[ FlatBnf[C, L](isTerm: false, nterm: patt.nterm) ] ]
     of bnfConcat:
       for idx, sub in patt.patts:
         var newpatts: seq[seq[FlatBnf[C, L]]]
         for patt in sub.flatten():
           if result.len == 0:
             newpatts.add patt
           else:
             for val in result:
               newpatts.add val & patt

         result = newpatts
     of bnfAlternative:
       for alt in patt.patts:
         result &= alt.flatten()


func toBNF*[C, L](
  rule: Rule[C, L],
  noAltFlatten: bool = false,
  renumerate: bool = true): seq[BnfRule[C, L]] =
  let (top, newrules) = rule.patts.toBnf(rule.nterm)
  if noAltFlatten:
    block:
      let topflat = top.flatten()
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

        # if false:
        # else:
        #   # let elems = elems.filterIt(it.kind != fbkEmpty)
        #   result.add rule(nterm, patt(elems)) # FIXME XXXX
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

func toBNF*[C, L](grammar: Grammar[C, L]): BnfGrammar[C, L] =
  mixin toBNF
  result = makeGrammar(
    grammar.rules.mapIt(it.toBNF(noAltFlatten = true, renumerate = false)).concat())

  result.start = BnfNterm(generated: false, name: grammar.start)



#========================  Predicates/accessors  =========================#

func isEmpty*[C, L](patt: BnfPatt[C, L]): bool =
  ## Check if pattern describes empty production
  (patt.elems.len == 0) # and (patt.elems[0].kind == fbkEmpty)

func `[]`*[C, L](grammar: BnfGrammar[C, L], rule: RuleId): BnfPatt[C, L] =
  ## Get BNF pattern for rule
  grammar.rules[rule.head][rule.alt]

func getProductions*[C, L](
  grammar: BnfGrammar[C, L], id: RuleId): seq[FlatBnf[C, L]] =
  ## Get list of productions from flat bnf pattern at `id`
  grammar.rules[id.head][id.alt].elems

func first*[C, L](patt: BnfPatt[C, L]): FlatBnf[C, L] =
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
  fbnf: FlatBnf[C, L],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  if fbnf.isTerm:
    let lex = fbnf.tok.hasLex.tern(&".{fbnf.tok.lex}", "")
    (&"{fbnf.tok.cat}{lex}").wrap(conf.termWrap)
  else:
    (fbnf.nterm.exprRepr(conf.normalizeNterms)).wrap(conf.ntermWrap)
    # of fbkNterm:
    # of fbkTerm:
    # of fbkEmpty:
    #   conf.emptyProd

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
        ($bnf.tok).wrap(conf.termWrap)
      of bnfNTerm:
        (bnf.nterm.exprRepr()).wrap(conf.ntermWrap)
      of bnfAlternative, bnfConcat:
        bnf.patts.mapIt(exprRepr(it, conf)).join(
          (bnf.kind == bnfConcat).tern(conf.concatSep, conf.alternSep)
        ).wrap("{  }")


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

func `$`*(id: RuleId): string = id.exprRepr()
func `$`*(nterm: BnfNterm): string = nterm.exprRepr()

#*************************************************************************#
#*****************************  Rule lookup  *****************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

type
  RuleLookup*[C, L] = object
    ## Lookup table for rules. Mapping `Token -> RuleId`
    rules: seq[RuleId]
    tokMap: TokLookup[C, L]

#=============================  Predicates  ==============================#

#==============================  Accessors  ==============================#

func getRule*[C, L, I](rlookup: RuleLookup[C, L],
                       tok: Token[C, L, I]
                      ): RuleId =
  ## Get single rule from lookup table. Raise exception on rule
  ## conflict.
  rlookup.rules[rlookup.tokMap.getAlt(tok)]

func addRule*[C, L](rl: var RuleLookup[C, L],
                    first: TokSet[C, L],
                    ruleId: RuleId,
                    canconflict: bool = false
                   ): void =
  ## Add new rule to lookup table
  let idx = rl.rules.len
  rl.rules.add ruleId
  for tok in items(first):
    # debugecho tok.exprRepr(), " -> ", ruleId.exprRepr(), " id: ", idx
    rl.tokMap.addAlt(tok, idx, canconflict = canconflict)

#============================  Constructors  =============================#
func initRuleLookup*[C, L](): RuleLookup[C, L] =
  RuleLookup[C, L](tokMap: initTokLookup[C, L]())

func initRuleLookup*[C, L](first: TokSet[C, L],
                           ruleId: RuleId,
                           canconflict: bool = false
                          ): RuleLookup[C, L] =
  ## Create new rule lookup table
  result = initRuleLookup[C, L]()
  result.addRule(first, ruleId, canconflict = canconflict)
  # raiseAssert("#[ IMPLEMENT ]#")

#========================  Other implementation  =========================#

#===========================  Pretty-printing  ===========================#
