import sets, sequtils, hashes, tables, strutils, strformat, macros
import hmisc/helpers
import hmisc/algo/halgorithm
import initcalls
import parse_primitives

## Parse tree contains actual token /values/ - concrete lexemes and
## additional information (whatever you deem necessary adding).
## `Token` represents lexical unit found in *input data*.
## `ExpectedToken` describes *required* token category and OPTIONAL
## lexeme value.

#*************************************************************************#
#********************************  Token  ********************************#
#*************************************************************************#

#===========================  Type definition  ===========================#

type
  LexInfo* = object
    ## Predefined type for `Info` field in token. Default
    ## implementation provided most common features. It is recommended
    ## but not mandatory to use this type for third generic parameter
    ## in `Token`.
    pos*: int ## Position of *token* in original lexer stream

  ETokKind* = enum
    etokRegular
    etokEOF

  Token*[Category, Lexeme, Info] = object
    ## Actual value of input token
    case kind*: ETokKind
      of etokRegular:
        cat*: Category ## Token category. It is REQUIRED to be correct
                       ## as it is always used in parsing
        lex*: Lexeme ## Lexeme information. It OPTIONAL and might be
                     ## used in parsing.
        info*: Info ## Additional information in token. It is OPTIONAL
                    ## and never used in parsing.
      of etokEOF:
        nil

  ExpectedToken*[C, L] = object
    ## Description of token to expect during parsing. In order for
    ## input token to match category MUST be identical (equality
    ## comparison). In case `hasLex` is true lexeme values also MUST
    ## match.
    # NOTE token comparison is done using simple equality - more
    # complex relations are not supported - they should be encoded in
    # lexeme or category comparison.
    case kind*: ETokKind
      of etokRegular:
        cat*: C ## Expected token category
        case hasLex*: bool ## Whether or not lexeme value should be
                           ## considered
          of true:
            lex*: L ## Expected lexeme value
          of false:
            nil
      of etokEOF:
        nil

  NoCategory* = enum
    catNoCategory

#============================  Constructors  =============================#

func makeExpToken*[C, L](category: C, lexeme: L): ExpectedToken[C, L] =
  ## Create regular expected token with category and lexeme
  ExpectedToken[C, L](kind: etokRegular, cat: category,
                      lex: lexeme, hasLex: true)

func makeExpToken*[C, L](category: C): ExpectedToken[C, L] =
  ## Create regular expected token with empty lexeme value
  ExpectedToken[C, L](kind: etokRegular, cat: category, hasLex: false)

func makeExpEOFToken*[C, L](): ExpectedToken[C, L] =
  ## Create `EOF` token
  ExpectedToken[C, L](kind: etokEOF)

# func makeExpEOFToken*()

func makeExpTokenVoidCat*[L](lex: L): ExpectedToken[void, L] =
  ExpectedToken[void, L](kind: etokRegular, hasLex: true, lex: lex)

func matches*[C, L, I](exp: ExpectedToken[C, L], tok: Token[C, L, I]): bool =
  ## Return true if token `tok` matches with expected token `exp`
  # TODO IMPLEMENT
  if tok.cat != exp.cat:
    false
  else:
    if exp.hasLex:
      exp.lex == tok.lex
    else:
      true


func makeToken*[C, L, I](cat: C, lex: L): Token[C, L, I] =
  Token[C, L, I](kind: etokRegular, cat: cat, lex: lex)

func makeTokenNoInfo*[C, L](cat: C, lex: L): Token[C, L, void] =
  Token[C, L, void](kind: etokRegular, cat: cat, lex: lex)

func makeTokenNoCat*[L, I](lex: L): Token[NoCategory, L, I] =
  Token[NoCategory, L, I](kind: etokRegular, lex: lex, cat: catNoCategory)

func makeTokens*[C, L](cats: seq[C]): seq[Token[C, L, void]] =
  cats.mapIt(Token[C, L, void](cat: it))

func makeTokens*(lexemes: seq[string]
                ): seq[Token[NoCategory, string, void]] =
  lexemes.mapIt(makeTokenNoCat[string, void](it))


#==============================  Accessors  ==============================#
func setPosInfo*[C, L](tok: var Token[C, L, LexInfo], pos: int): void =
  tok.info.pos = pos

func getPosInfo*[C, L](tok: Token[C, L, LexInfo]): int =
  tok.info.pos

template setPosInfo*[Tok](tok: var Tok, pos: int): untyped =
  ## Fallback implementation for `setPosInfo`.
  when compiles(setPosInfo(tok, pos)):
    setPosInfo(tok, pos)

template hasPosInfo*[C, L, I](tok: Token[C, L, I]): bool =
  compiles(getPosInfo(tok))

#*************************************************************************#
#*****************************  Lexeme set  ******************************#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  LexSet*[L] = object
    ## Set of lexemes
    hasAll: bool ## Whether or not all lexeme values are in set
    lexemes: HashSet[L]


#=============================  Predicates  ==============================#
func contains*[L](lset: LexSet[L], lex: L): bool =
  lex in lset.lexemes

func hash*[C, L](tok: ExpectedToken[C, L]): Hash =
  var h: Hash = 0
  h = h !& hash(tok.cat)

  when not (L is void):
    h = h !& hash(tok.lex)

  result = !$h

func `==`*[C, L](l, r: ExpectedToken[C, L]): bool =
  when not (C is void):
    if (l.cat == r.cat) and (l.hasLex == r.hasLex):
      if l.hasLex:
        when L is void:
          true
        else:
          l.lex == r.lex
      else:
        true
    else:
      false
  else:
    if (l.hasLex == r.hasLex):
      if l.hasLex:
        l.lex == r.lex
      else:
        true
    else:
      false


#==============================  Accessors  ==============================#

func incl*[L](s: var LexSet[L], lex: L): void = s.lexemes.incl(lex)

func incl*[L](s: var LexSet[L], other: LexSet[L]): void =
  s.hasAll = s.hasAll or other.hasAll
  s.lexemes.incl(other.lexemes)

iterator items*[L](lset: LexSet[L]): L =
  for lex in lset.lexemes:
    yield lex

func getHasAll*[L](lset: LexSet[L]): bool = lset.hasAll
func getLexemes*[L](lset: LexSet[L]): HashSet[L] = lset.lexemes

func containsOrIncl*[A](hs: var HashSet[A], other: HashSet[A]): bool =
  for elem in other:
    if elem notin hs:
      hs.incl elem
      result = false

func containsOrIncl*[L](ls: var LexSet[L], other: LexSet[L]): bool =
  if other.hasAll and (not ls.hasAll):
    ls.hasAll = true
    result = false

  if not containsOrIncl(ls.lexemes, other.lexemes):
    result = false

#============================  Constructors  =============================#
func makeLexSet*[L](): LexSet[L] =
  LexSet[L](lexemes: initHashSet[L](2))

func initLexSet*[L](hasAll: bool, lexemes: HashSet[L]): LexSet[L] =
  LexSet[L](hasAll: hasAll, lexemes: lexemes)

#===========================  Pretty-printing  ===========================#
func exprRepr*[L](lset: LexSet[L]): string =
  (
    lset.lexemes.mapIt(($it).wrap("''")) &
      lset.hasAll.tern(@["_"], @[])
  ).join(", ").wrap("{}")


#*************************************************************************#
#******************************  Token set  ******************************#
#*************************************************************************#
#===========================  Type definition  ===========================#
type
  EofTok* = object
  TokSet*[C, L] = object
    ## Set of expected tokens + EOF token (end of inptu sequence)
    tokens: Table[C, LexSet[L]] ## Map from
    ## token category to list of expected lexemes + whether or not
    ## token can be accepted if lexeme does not match.
    # `kwd."if"` matches _if_ `kwd -> (lex: {"if"})` - token with
    # category `kwd` is paired with lexeme "if" _or_ `kwd ->
    # (hasEmpty: true)` - token of kind `kwd` is accepted without
    # checking lexeme.

    hasEof: bool


#=============================  Predicates  ==============================#
func contains*[C, L, I](s: TokSet[C, L], tk: Token[C, L, I]): bool =
  if tk.cat in s.tokens:
    (s.tokens[tk.cat].hasAll) or (tk.lex in s.tokens[tk.cat])
  else:
    false

func contains*[C, L](s: TokSet[C, L], tk: EofTok): bool =
  s.hasEof


#===============================  Getters  ===============================#
func getTokens*[C, L](tset: TokSet[C, L]): Table[C, LexSet[L]] =
  tset.tokens

func getHasEof*[C, L](tset: TokSet[C, L]): bool =
  tset.hasEof

const eofTok*: EofTok = EofTok()

iterator items*[C, L](s: TokSet[C, L]): ExpectedToken[C, L] =
  for cat, lset in pairs(s.tokens):
    if lset.hasAll:
      yield makeExpToken[C, L](cat)

    for lex in items(lset):
      yield makeExpToken[C, L](cat, lex)


iterator pairs*[C, L](s: TokSet[C, L]): (C, LexSet[L]) =
  for cat, lset in s.tokens:
    yield (cat, lset)


#===============================  Setters  ===============================#
func incl*[C, L, I](s: var TokSet[C, L], tk: Token[C, L, I]): void =
  s.vals.incl tk

func incl*[C, L](s: var TokSet[C, L], etk: ExpectedToken[C, L]): void =
  if etk.cat notin s.tokens:
    s.tokens[etk.cat] = makeLexSet[L]()

  if etk.hasLex:
    s.tokens[etk.cat].incl etk.lex
  else:
    s.tokens[etk.cat].hasAll = true

func incl*[C, L](s: var TokSet[C, L], tk: EofTok): void =
  (s.hasEof = true)

func incl*[C, L](
  s: var TokSet[C, L], other: TokSet[C, L]): void =
  for cat, lex in other.tokens:
    if cat notin s.tokens:
      s.tokens[cat] = lex
    else:
      s.tokens[cat].incl lex

func containsOrIncl*[C, L](
  s: var TokSet[C, L], other: TokSet[C, L]): bool =
  if other.hasEof and (not s.hasEof):
    s.hasEof = true
    result = false

  for cat, lset in other.tokens:
    if cat notin s.tokens:
      result = false
      s.tokens[cat] = lset
    else:
      if not containsOrIncl(s.tokens[cat], lset):
        result = false


#============================  Constructors  =============================#
func makeTokSet*[C, L](): TokSet[C, L] =
  TokSet[C, L](tokens: initTable[C, LexSet[L]](2))

func initTokSet*[C, L](
  tokens: Table[C, LexSet[L]], hasEof: bool): TokSet[C, L] =
  TokSet[C, L](tokens: tokens, hasEof: hasEof)

# func makeTokSet*[C, L, I](tok: Token[C, L, I]): TokSet[C, L] =
#   result = makeTokSet[C, L]()
#   result.vals.incl tok

func makeTokSet*[C, L](tok: ExpectedToken[C, L]): TokSet[C, L] =
  result = makeTokSet[C, L]()
  result.incl tok

func makeTokSet*[C, L](eof: EofTok): TokSet[C, L] =
  result = makeTokSet[C, L]()
  (result.hasEof = true)

func union*[C, L](s: seq[TokSet[C, L]]): TokSet[C, L] =
  result.hasEof = s.anyOfIt(it.hasEof)
  for it in s:
    for cat, lset in pairs(it):
      if cat notin result.tokens:
        result.tokens[cat] = lset
      else:
        result.tokens[cat].incl lset



#========================  Other implementation  =========================#
func hash*[C, L](s: TokSet[C, L]): Hash =
  var h: Hash = 0
  h = h !& hash(s.vals) !& hash(s.hasEof)
  result = !$h


#===========================  Pretty-printing  ===========================#
func exprRepr*[C, L](s: TokSet[C, L]): string =
  s.tokens.mapPairs(fmt("{lhs} -> {rhs.exprRepr()}")
  ).join(", ").wrap("{}")

#*************************************************************************#
#*************************  Lexeme lookup table  *************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

type
  LexLookup*[L] = object
    ## Lookup table from lexemes to one or more parsing alternatives
    table: Table[L, seq[int]] ## Expected tokens with concrete lexeme
                              ## specified
    hasAll: seq[int] ## Expected tokens without specification for
                     ## lexeme

#=============================  Predicates  ==============================#

func contains*[L](ll: LexLookup[L], lex: L): bool =
  ## Check if one of the tokens in lookup table *explicitly expects*
  ## this lexeme - `hasAll` is ignored.
  lex in ll.table

#==============================  Accessors  ==============================#

iterator pairs*[L](tl: LexLookup[L]): (L, int) =
  for lex, alts in tl.table:
    for alt in alts:
      yield (lex, alt)

func `[]`*[L](tl: var LexLookup[L], lex: L): var seq[int] =
  tl.table[lex]

func `[]`*[L](tl: LexLookup[L], lex: L): seq[int] =
  tl.table[lex]

func addAlt*[L](ll: var LexLookup[L],
                lex: L,
                alt: int,
                canconflict: bool = false): void =
  ## Add `lex` -> `alt` pair to lookup table.
  if lex notin ll.table:
    ll.table[lex] = @[alt]
  else:
    # debugecho ll.table[lex].len
    if ll.table[lex].len > 0 and (not canconflict):
      raiseAssert(msgjoin(
        "New pair for lexeme lookup: `", lex,
        "` conflicts with already existing id #", ll.table[lex][0],
        "- cannot add #", alt, "(NOTE: canconflict = true)"
      ))
    else:
      ll.table[lex].add alt

func addHasAll*[L](ll: var LexLookup[L],
                   alt: int,
                   canconflict: bool = false): void =
  if ll.hasAll.len > 0 and (not canconflict):
    raiseAssert("#[ IMPLEMENT Cannot add conflicting has-all to lexeme lookup ]#")
  else:
    ll.hasAll.add alt

#============================  Constructors  =============================#
func initLexLookup*[L](): LexLookup[L] =
  LexLookup[L](table: initTable[L, seq[int]](2))

func initLexLookup*[L](
  table: Table[L, seq[int]], hasAll: seq[int]): LexLookup[L] =
  LexLookup[L](table: table, hasAll: hasAll)

func makeInitCalls*[L](lookup: LexLookup[L]): NimNode =
  mixin makeInitCalls
  result = newCall(
    "initLexLookup",
    nnkExprEqExpr.newTree(ident "table", lookup.table.makeInitCalls()),
    nnkExprEqExpr.newTree(ident "hasAll", lookup.hasAll.makeInitCalls())
  )

func makeLexLookup*[L](): LexLookup[L] =
  LexLookup[L](table: initTable[L, seq[int]](2))


#*************************************************************************#
#****************************  Token lookup  *****************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

type
  TokLookup*[C, L] = object
    table: Table[C, LexLookup[L]]

#=============================  Contructors  =============================#

func `[]`*[C, L](tl: var TokLookup[C, L], cat: C): var LexLookup[L] =
  tl.table[cat]

func `[]`*[C, L](tl: TokLookup[C, L], cat: C): LexLookup[L] =
  tl.table[cat]


func contains*[C, L](tl: TokLookup[C, L], cat: C): bool = cat in tl.table

#===================  Predicates/accessors/iterators  ====================#
func initTokLookup*[C, L](): TokLookup[C, L] =
  TokLookup[C, L](table: initTable[C, LexLookup[L]](2))

func initTokLookup*[C, L](table: Table[C, LexLookup[L]]): TokLookup[C, L] =
  TokLookup[C, L](table: table)


func makeInitCalls*[C, L](lookup: TokLookup[C, L]): NimNode =
  mixin makeInitCalls
  result = newCall(
    "initTokLookup",
    nnkExprEqExpr.newTree(ident "table", lookup.table.makeInitCalls()))

func makeTokLookup*[C, L](): TokLookup[C, L] =
  TokLookup[C, L](table: initTable[C, LexLookup[L]](2))

func makeTokLookup*[C, L](
  altSets: seq[TokSet[C, L]], canConflict: bool = false): TokLookup[C, L] =
  ## Create token lookup from sequence of alternatives
  # TODO detect ambiguity
  # TODO IMPLEMENT
  result = makeTokLookup[C, L]()
  for idx, alt in altSets:
    for cat, lset in pairs(alt):
      if cat notin result.table:
        result.table[cat] = makeLexLookup[L]()

      if lset.hasAll:
        if canConflict and result[cat].hasAll.len > 0:
          raiseAssert("Conflict") # TODO better error msg
        else:
          result[cat].hasAll.add idx

      for lex in items(lset):
        if lex notin result.table[cat].table:
          result[cat].table[lex] = @[]

        if canConflict and result[cat][lex].len > 0:
          raiseAssert("Conflict") # TODO better error msg
        else:
          result[cat][lex].add idx




#==============================  Accessors  ==============================#

iterator pairs*[C, L](lookup: TokLookup[C, L]
                     ): (ExpectedToken[C, L], int) =
  for cat, lexset in lookup.table:
    for hasall in lexset.hasAll:
      yield (makeExpToken[C, L](cat), hasall)

    for lex, altid in lexset:
      yield (makeExpToken(cat, lex), altid)

func getAlt*[C, L, I](
  lookup: TokLookup[C, L], token: Token[C, L, I]): int =
  ## Get select alternative set based on token category and lexeme values.
  # TODO raise exception if token is not found
  # TODO IMPLEMENT
  if token.cat in lookup:
    if token.lex in lookup[token.cat]:
      let alts = lookup[token.cat][token.lex]
      if alts.len > 1:
        raiseAssert(msgjoin(
          "Attempt to get single option from more than one alternatives for",
          token.exprRepr(), "- it has", alts.len, "alternatives"
        ))
      else:
        return alts[0]
    elif lookup[token.cat].hasAll.len > 0:
      let alts = lookup[token.cat].hasAll
      if alts.len > 1:
        raiseAssert("#[ IMPLEMENT more than one alternative ]#")
      else:
        return alts[0]
    else:
      raiseAssert(
        &"No item in lookup matches lexeme for token: {token.exprRepr()}"
      )
  else:
    raiseAssert("#[ IMPLEMENT token category not found ]#")

func addAlt*[C, L](tl: var TokLookup[C, L],
                   tok: ExpectedToken[C, L],
                   alt: int,
                   canconflict: bool = false): void =
  if tok.cat notin tl.table:
    tl.table[tok.cat] = initLexLookup[L]()

  if tok.hasLex:
    tl.table[tok.cat].addAlt(tok.lex, alt, canconflict = canconflict)
  else:
    tl.table[tok.cat].addHasAll(alt, canconflict = canconflict)


#*************************************************************************#
#*********************  Unexpected token exceptions  *********************#
#*************************************************************************#

type
  LinePosInfo* = object
    line*: int
    column*: int
    filename*: string

  # ParserErrorKind* = enum

  #   pekUnexpectedToken

  # ParserError = ref object of CatchableError
  #   linepos*: Option[LinePosInfo]
  #   case kind*: ParserErrorKind
  #     of pekUnexpectedToken:
  #       expected*: string
  #       intoken*: string
  #       inputpos*: int

func exprRepr*[C, L, I](tok: Token[C, L, I]): string =
  # TODO remove prefix from category enum
  when C is NoCategory:
    fmt("'{tok.lex}'")
  else:
    fmt("({tok.cat} '{tok.lex}')")

func exprRepr*[C, L](exp: ExpectedToken[C, L],
                     conf: GrammarPrintConf = defaultGrammarPrintConf
                    ): string =
  when C is NoCategory:
    fmt("{exp.lex}").toGreen(conf.colored).wrap(conf.termWrap)
  else:
    if exp.hasLex:
      fmt("({exp.cat} '{exp.lex}')")
    else:
      fmt("({exp.cat} _)")

func getLinePos*[C, L, I](tok: Token[C, L, I]): LinePosInfo =
  getLinePos(tok.info)

func hasLinePosInfo*[C, L, I](tok: Token[C, L, I]): bool =
  const res = compiles(getLinePos(tok.info)) and
    (getLinePos(tok.info) is LinePosInfo)

template assertToken*[C, L, I](
  exp: ExpectedToken[C, L], tok: Token[C, L, I]): untyped =
  if not matches(exp, tok):
    raiseAssert(
      "Unexpected token _" & exprRepr(tok) & "_, expected _" &
        exprRepr(exp) & "_")
