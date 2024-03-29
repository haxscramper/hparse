import tables, hashes, sugar, sequtils, strformat, options, colors
import strutils
export tables
import hmisc/helpers
import hmisc/hasts/graphviz_ast
import hmisc/types/[hvariant, colorstring]
import hmisc/algo/[halgorithm, htree_mapping, hseq_mapping]
import lexer, token

import parse_primitives


type
  Patt*[C, L] = object
    ## Ebnf grammar pattern. `Tok` is a type for token object.
    # head*: NTermSym ## Nonterminal symbol
    action*: TreeAct
    case kind*: PattKind
      of pkNterm:
        nterm*: NTermSym ## Nonterminal to parse

      of pkTerm:
        # NOTE might be replaced with something like 'ExpectedToken'
        # which also contains information about comparison strategy
        # (category, lexeme or both).
        tok*: ExpectedToken[C, L] ## Single token to match literally

      of pkAlternative, pkConcat:
        patts*: seq[Patt[C, L]] ## List of alternative patterns

      of pkOptional, pkZeroOrMore, pkOneOrMore:
        item*: seq[Patt[C, L]] ## Single instance that will be repeated
        ## [0..1], [0..n] or [1..n] times respectively

  Rule*[C, L] = object
    nterm*: NTermSym
    patts*: Patt[C, L]

  Grammar*[C, L] = object
    start*: NtermSym
    rules*: seq[Rule[C, L]]

#==============================  Aux proc  ===============================#

func hash*[C, L](p: Patt[C, L]): Hash =
  var h: Hash = 0
  h = h !& hash(p.action)

  case p.kind:
    of pkNterm:
      h = h !& hash(p.nterm)
    of pkTerm:
      h = h !& hash(p.tok)
    of pkAlternative, pkConcat:
      for it in p.patts:
        h = h !& hash(it)
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      h = h !& hash(p.item[0])

  result = !$h

#=============================  Predicates  ==============================#

func `==`*[C, L](lhs, rhs: Patt[C, L]): bool =
  lhs.action == rhs.action and lhs.kind == rhs.kind and (
    case lhs.kind:
      of pkNterm:
        lhs.nterm == rhs.nterm
      of pkTerm: lhs.tok == rhs.tok
      of pkAlternative, pkConcat:
        subnodesEq(lhs, rhs, patts)
      of pkOptional, pkZeroOrMore, pkOneOrMore:
        lhs.item[0] == rhs.item[0]
  )

#====================  generic pattern construction  =====================#

func rule*[C, L](name: string, patt: Patt[C, L]): Rule[C, L] =
  Rule[C, L](nterm: name, patts: patt)

func zeroP*[C, L](patt: Patt[C, L]): Patt[C, L] =
  Patt[C, L](kind: pkZeroOrMore, item: @[ patt ])

func oneP*[C, L](patt: Patt[C, L]): Patt[C, L] =
  Patt[C, L](kind: pkOneOrMore, item: @[ patt ])

func optP*[C, L](patt: Patt[C, L]): Patt[C, L] =
  Patt[C, L](kind: pkOptional, item: @[ patt ])

func andP*[C, L](patts: varargs[Patt[C, L]]): Patt[C, L] =
  mixin hash
  Patt[C, L](kind: pkConcat, patts: toSeq(patts))

func nullP*[C, L](): Patt[C, L] =
  Patt[C, L](kind: pkConcat)

func orP*[C, L](patts: varargs[Patt[C, L]]): Patt[C, L] =
  Patt[C, L](kind: pkAlternative, patts: toSeq(patts))

func tok*[C, L](tok: ExpectedToken[C, L]): Patt[C, L] =
  Patt[C, L](kind: pkTerm, tok: tok)

func tok*[C, L](cat: C, lex: L): Patt[C, L] =
  Patt[C, L](kind: pkTerm, tok: makeExpToken(cat, lex))


func noCatTok*[L](lex: L): Patt[NoCategory, L] =
  Patt[NoCategory, L](kind: pkTerm, tok: makeExpNoCat(lex))

func tok*[C, L](cat: C): Patt[C, L] =
  Patt[C, L](kind: pkTerm, tok: makeExpToken[C, L](cat))

func nterm*[C, L](nterm: string): Patt[C, L] =
  Patt[C, L](kind: pkNTerm, nterm: nterm)


# proc dslTok*[C](cat: C): Patt[C, L] =
#   tok(makeExpToken(cat))

proc dslTok*[C, L](cat: C, lex: L): Patt[C, L] =
  tok(makeExpToken(cat, lex))

# proc dslTok*[C, L](cat: C, lex: L): Patt[C, L] =
#   tok(makeExpToken(cat, lex))


#============================  Constructors  =============================#

func toGrammar*[C, L](
  table: openarray[(string, Patt[C, L])]): Grammar[C, L] =
  result.rules = table.mapPairs(rule(lhs, rhs))
  result.start = result.rules[0].nterm

#==============================  Accessors  ==============================#

func addAction*[C, L](patt: Patt[C, L], act: TreeAct): Patt[C, L] =
  result = patt
  result.action = act

func `opt`*[C, L](patt: Patt[C, L]): Patt[C, L] = patt.item[0]


import strutils

#*************************************************************************#
#***************************  pretty-printing  ***************************#
#*************************************************************************#
func tokKindStr*[C](tok: C, prefStr: string): string =
  result = $tok
  if result.startsWith(prefStr):
    result = result[prefStr.len .. ^1]

#=======================  grammar representation  ========================#

func exprRepr*(ta: TreeAct,
               conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  result = case ta:
    of taDefault: ""
    of taDrop: "!"
    of taSubrule: "v"
    of taSpliceDiscard: "@"
    of taSplicePromote: "^@"
    of taPromote: "^"

  return result.toRed(conf.colored)

func exprRepr*[C, L](
  patt: Patt[C, L],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  let act =
    if patt.action != taDefault: &"{patt.action.exprRepr(conf)}" else: ""
  case patt.kind:
    of pkTerm:
      act & (patt.tok.exprRepr(conf))
    of pkNTerm:
      mixin toYellow
      if patt.action != taDefault:
        (&"{patt.action.exprRepr(conf)}{toYellow(patt.nterm, conf.colored)}"
        ).wrap(conf.ntermWrap)
      else:
        toYellow($patt.nterm, conf.colored).wrap(conf.ntermWrap)
    of pkAlternative, pkConcat:
      act & patt.patts.mapIt(exprRepr(it, conf)).join(
        (patt.kind == pkConcat).tern(conf.concatSep, conf.alternSep)
      ).wrap("{}")
    of pkOptional, pkZeroOrMore, pkOneOrMore:
      let pref =
        case patt.kind:
          of pkOptional: "?"
          of pkZeroOrMore: "*"
          of pkOneOrMore: "+"
          else:
            ""

      fmt("{act}{toMagenta(pref, conf.colored)}({patt.opt.exprRepr(conf)})")


func exprRepr*[C, L](
  grammar: Grammar[C, L],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  grammar.rules.mapIt(exprRepr(it, conf)).joinl()

func exprRepr*[C, L](
  rule: Rule[C, L],
  conf: GrammarPrintConf = defaultGrammarPrintConf): string =
  return fmt("{rule.nterm:<12} {conf.prodArrow} {rule.patts.exprRepr(conf)}")
