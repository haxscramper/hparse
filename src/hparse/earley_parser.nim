## unfinished implementation of Earley parser from
## http://loup-vaillant.fr/tutorials/earley-parsing/ . The only
## implementation part missing is a tree construction.

import sugar, strutils, sequtils, strformat, options,
       sets, algorithm, hashes, tables

template withIt*(val, body: untyped): untyped =
  block:
    var it {.inject.} = val
    block:
      body
    it

#*************************************************************************#
#**************************  Type definitions  ***************************#
#*************************************************************************#

type
  # `C` is a terminal category. Lexeme is represented as string
  Tok[C] = Option[C] -> bool
  Input[C] = proc(pos: int): Option[C] {.noSideEffect.}
  Symbol[C] = object
    case isTerm: bool
      of true:
        terminal: tuple[ok: Tok[C], lex: string]
      of false:
        nterm: string

  NullSet = object
    nulls: HashSet[string]

  Rule[C] = object
    lhs: string
    rhs: seq[Symbol[C]]

  EItem = object
    ruleId: int
    startPos: int
    nextPos: int
    ranges: seq[tuple[start, finish: int]]

  SSetId = int # 'The nodes are the state sets. One of them is the
               # root (the search starts there), and one of them is
               # the leaf (the search stops there).'

  SItemId = object # 'The edges are the items themselves. They start
                   # from a node (indeed, they are stored in a node,
                   # and they point to another node (wherever they
                   # finish).'
    ruleId: int
    finish: int

  Path = tuple
    sset: SSetId
    ssetItem: SItemId

  State = seq[seq[EItem]]
  Chart = seq[seq[SItemId]]

  Grammar[C] = object
    rules: seq[Rule[C]]
    start: string

  ParseTree[C] = object
    start, finish: int
    case isToken: bool
      of true:
        token: C
      of false:
        ruleId: int
        subnodes: seq[ParseTree[C]]

func contains(ns: NullSet, s: string): bool = s in ns.nulls
func len(ns: NullSet): int = ns.nulls.len

#*************************************************************************#
#*******************  Lising of all nullable symbols  ********************#
#*************************************************************************#

func isNullable[C](ns: NullSet, rule: Rule[C]): bool =
  for item in rule.rhs:
    if item.isTerm:
      return false
    else:
      if item.nterm notin ns:
        return false

  return true

func updateNss[C](ns: var NullSet, gr: Grammar[C]): void =
  for rule in gr.rules:
    if ns.isNullable(rule):
      ns.nulls.incl rule.lhs

func nullableSymbols[C](grammar: Grammar[C]): NullSet =
  while true:
    let size = result.len
    updateNss(result, grammar)
    if result.len == size:
      break

func hasLoops[C](grammar: Grammar[C]): bool =
  let nullable: NullSet = nullableSymbols grammar

func ruleName[C](gr: Grammar[C], idx: int): string =
  ## Get rule name at `idx`
  gr.rules[idx].lhs

func ruleBody[C](gr: Grammar[C], idx: int): seq[Symbol[C]] =
  ## Get rhs from rule at `idx`
  gr.rules[idx].rhs

func nextSymbol[C](gr: Grammar[C], item: EItem): Option[Symbol[C]] =
  if gr.ruleBody(item.ruleId).len > item.nextPos:
    some(gr.ruleBody(item.ruleId)[item.nextPos])
  else:
    none(Symbol[C])

func append[A](a: var seq[A], b: A): void =
  for it in a:
    if it == b:
      return

  a.add b

import hpprint

func chartOfItems[C](gr: Grammar[C],
                     s: State): Chart =
  result = s.mapIt(newSeqWith(0, SItemId()))
  for idx, itemSet in s:
    for item in itemSet:
      let sym: Option[Symbol[C]] = gr.nextSymbol(item)

      if sym.isSome():
        discard # Item not fully completed

      else:
        result[item.startPos].add SItemId(
          ruleId: item.ruleId,
          finish: idx # REVIEW NOTE this is an index of itemset, not
                      # position in item itself
        )

  for edgeset in mitems(result):
    edgeset.sort do (e2, e1: SItemId) -> int:
      if e1.ruleId == e2.ruleId:
        e2.finish - e1.finish # FIXME
      else:
        e2.ruleId - e1.ruleId # FIXME

func ruleName[C](gr: Grammar[C], e: SItemId): string =
  gr.ruleName(e.ruleId)

func hash(id: SItemId): Hash =
  var h: Hash = 0
  h = h !& id.ruleId !& id.finish
  result = !$h

type
  TryParams = object
    start: int
    altId: int
    name: string

func hash(pr: TryParams): Hash = !$(pr.start !& pr.altId !& hash(pr.name))

proc dfSearch(
    edges: proc(depth: int, start: int): seq[SItemId],
    child: proc(depth: int, edge: SItemId): int,
    check: proc(depth: int, start: int): bool,
    root: int
  ): seq[(int, SItemId)] =

  proc aux(depth: int, root: int): Option[seq[(int, SItemId)]] =
    if check(depth, root):
      return some newSeq[typeof(result.get()[0])]()

    else:
      for edge in edges(depth, root):
        let auxRes = aux(depth + 1, child(depth, edge))
        if auxRes.isSome():
          return some @[(root, edge)] & auxRes.get()

  return aux(0, root).get()


proc topList[C](
    grammar: Grammar[C],
    input: Input[C],
    chart: Chart,
    start: int,
    edge: SItemId
  ): seq[(int, SItemId)] =

  let
    finish = edge.finish
    rule = edge.ruleId
    symbols = ruleBody(grammar, rule)
    bottom = symbols.len

  func check(depth, start: int): bool =
    depth == bottom and start == finish

  func child(depth: int, edge: SItemId): int =
    edge.finish

  func edges(depth: int, start: int): seq[SItemId] =
    if depth >= symbols.len:
      return @[]

    else:
      if symbols[depth].isTerm:
        if symbols[depth].terminal.ok(input(start)):
          return @[SItemId(finish: start + 1, ruleId: -1)]

        else:
          return @[]

      else:
        let name = symbols[depth].nterm
        for item in chart[start]:
          if ruleName(grammar, item.ruleId) == name:
            result.add item

  dfSearch(edges, child, check, start)




proc parseTree[C](grammar: Grammar[C],
                  input: Input[C],
                  chart: Chart): seq[ParseTree[C]] =

  let
    start = 0
    finish = chart.len - 1
    name = grammar.start

  proc aux(start: int, edge: SItemId): ParseTree[C] =
    if edge.ruleId == -1:
      result = ParseTree[C](isToken: true, token: input(start).get())

    else:
      result = ParseTree[C](isToken: false, ruleId: edge.ruleId)

      result.subnodes = collect(newSeq):
        for (node, edge) in topList(grammar, input, chart, start, edge):
          aux(node, edge)

  for edge in chart[start]:
    if edge.finish == finish and ruleName(grammar, edge.ruleId) == name:
      return @[aux(start, edge)]





func predict[C](s: var State, # DOC
             i: int,
             j: int, # DOC
             nullable: NullSet,
             symbol: string, # FIXME ???
             gr: Grammar[C]
            ): int =
  # Prediction. The symbol at the right of the fat dot is
  # non-terminal. We add the the corresponding rules to the current
  # state set.
  for ruleId, rule in gr.rules:
    if rule.lhs == symbol:
      s[i].append(EItem(ruleId: ruleId, startPos: i, nextPos: 0))

    if symbol in nullable:
      s[i].append s[i][j].withIt do:
        inc it.nextPos

func scan[C](s: var State,
             i, j: int,
             symbol: Tok[C],
             gr: Grammar[C],
             input: int -> Option[C]): int =
  # Scan. The symbol at the right of the fat dot is terminal. We check
  # if the input matches this symbol. If it does, we add this item
  # (advanced one step) to the next state set.
  if symbol(input i):
    if s.len - 1 <= i:
      s.add @[]

    s[i + 1].add s[i][j].withIt do:
      inc it.nextPos


func complete[C](s: var State,
                 i, j: int,
                 gr: Grammar[C],
                 input: int -> Option[C]): int =
  # Completion. There is nothing at the right of the fat dot. This
  # means we have a successful partial parse. We look for the parent
  # items, and add them (advanced one step) to this state set.
  let item = s[i][j]
  for oldItem in s[item.startPos]:
    let next = gr.nextSymbol(oldItem)
    if next.isNone():
      discard
    else:
      let sym = next.get()
      if sym.isTerm:
        discard
      else:
        if sym.nterm == gr.ruleName(item.ruleId):
          s[i].append oldItem.withIt do:
            inc it.nextPos


proc buildItems[C](gr: Grammar[C],
                   input: int -> Option[C]): State =
  let nullable = nullableSymbols gr

  result.add @[]

  # Seed s with the start symbol
  for idx, rule in gr.rules:
    if rule.lhs == gr.start:
      result[0].add EItem(ruleId: idx, startPos: 0, nextPos: 0)

  var i = 0 # DOC
  echo "- ", result.len
  while i < result.len: # Loop over main array of state sets
    var j = 0 # DOC
    echo " <- ", result[i].len
    while j < result[i].len: # Loop over elements in particular state set
      let next: Option[Symbol[C]] = gr.nextSymbol(result[i][j])
      if next.isNone():
        discard complete(result, i, j, gr, input)

      else:
        let sym: Symbol[C] = next.get()
        if sym.isTerm:
          discard scan(result, i, j, sym.terminal.ok, gr, input)

        else:
          discard predict(result, i, j, nullable, sym.nterm, gr)

      inc j
      echo "   -> ", result[i].len

    inc i

#*************************************************************************#
#****************************  Test grammar  *****************************#
#*************************************************************************#

func lispRepr[C](pt: ParseTree[C], gr: Grammar[C]): string =
  if pt.isToken:
    $pt.token
  else:
    fmt("({gr.ruleName(pt.ruleId)}") & (
      block:
        if pt.subnodes.len > 0:
          " " & pt.subnodes.mapIt(lispRepr(it, gr)).join(" ") & ")"
        else:
          ")"
    )

func treeRepr[C](pt: ParseTree[C], gr: Grammar[C], level: int = 0): string =
  let pref = "  ".repeat(level)
  if pt.isToken:
    &"[*]{pref}\e[32m'{pt.token}'\e[39m"

  else:
    fmt("[{pt.subnodes.len}] {pref}\e[33m{gr.ruleName(pt.ruleId)}\e[39m") & (
      block:
        if pt.subnodes.len > 0:
          "\n" & pt.subnodes.mapIt(treeRepr(it, gr, level + 1)).join("\n")
        else:
          ""
    )

proc printItems[C](gr: Grammar[C], state: State, onlyFull: bool = false): void =
  for idx, stateset in state:
    echo fmt("   === {idx:^3} ===   ")
    for item in stateset:
      if (item.nextPos == gr.ruleBody(item.ruleId).len) or (not onlyFull):
        var buf = fmt("{gr.ruleName(item.ruleId):<12}") & " ->"
        for idx, sym in gr.ruleBody(item.ruleId):
          if idx == item.nextPos:
            buf &= " •"

          if sym.isTerm:
            buf &= " " & sym.terminal.lex
          else:
            buf &= " " & sym.nterm

        if item.nextPos == gr.ruleBody(item.ruleId).len:
          buf = fmt("{buf:<60} \e[4m#\e[24m ({item.startPos})")
        else:
          buf = fmt("{buf:<60}   ({item.startPos})")

        echo buf


proc printChart[C](gr: Grammar[C], state: Chart): void =
  echo "\e[31mCHART :\e[39m"
  for idx, stateset in state:
    echo fmt("\e[36mSTARTS:\e[39m {idx}")
    for item in stateset:
      var buf = fmt("{gr.ruleName(item.ruleId):<12}") & " ->"
      for idx, sym in gr.ruleBody(item.ruleId):
        if sym.isTerm:
          buf &= fmt(" {sym.terminal.lex:>8}")
        else:
          buf &= fmt(" {sym.nterm:>8}")

      buf = fmt("\e[32mEND   :\e[39m {item.finish} {buf:<60}")

      echo buf
    echo ""

func rule(lhs: string, elems: seq[Symbol[char]]): Rule[char] =
  Rule[char](lhs: lhs, rhs: elems)

func n(nterm: string): Symbol[char] =
  Symbol[char](isTerm: false, nterm: nterm)

func alt(alts: string): Symbol[char] =
  # Match any char from the string

  let altsset = alts.toHashSet()
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = c.isSome() and (c.get() in altsset),
    lex: &"[{alts}]"
  ))

func ch(ic: char): Symbol[char] =
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = c.isSome() and (c.get() == ic),
    lex: &"'{ic}'"
  ))

func rng(a, b: char): Symbol[char] =
  Symbol[char](isTerm: true, terminal: (
    ok: proc(c: Option[char]): bool = (c.get in {a .. b}),
    lex: &"[{a}-{b}]"
  ))

func makeInput(s: string): Input[char] =
  result = proc(pos: int): Option[char] {.noSideEffect.} =
    if pos < s.len:
      some(s[pos])
    else:
      none(char)


if false:
  let grammar1 = Grammar[char](
    start: "Sum",
    rules: @[
      rule("Sum",     @[n "Sum",       alt "+-", n "Product" ]),
      rule("Sum",     @[n "Product"                          ]),
      rule("Product", @[n "Product",   alt "*/", n "Factor"  ]),
      rule("Product", @[n "Factor"                           ]),
      rule("Factor",  @[ch '(',        n "Sum",  ch ')'      ]),
      rule("Factor",  @[n "Number"                           ]),
      rule("Number",  @[n "Number",    rng('0',  '9')        ]),
      rule("Number",  @[rng('0',       '9')                  ])
    ]
  )


  let input1 = makeInput "1+(2*3-4)"
  let s1     = buildItems(grammar1, input1)
  # printItems(grammar1, s1)
  let c1     = chartOfItems(grammar1, s1)
  # printItems(grammar1, s1, onlyFull = true)
  printChart(grammar1, c1)
  let pt1    = parseTree(grammar1, input1, c1)
  for tree in pt1:
    echo tree.treeRepr(grammar1)

if true:
  # Ambigous parse, should generate two trees
  let grammar1 = Grammar[char](
    start: "Block",
    rules: @[
      rule("Block", @[ch ';']),
      rule("Block", @[n "If"]),
      rule("If",    @[ch 'i',   n "Block"]),
      rule("If",    @[ch 'i',   n "Block",   ch 'e', n "Block"])
    ]
  )


  let input1 = makeInput "ii;e;"
  let s1     = buildItems(grammar1, input1)
  printItems(grammar1, s1)
  let c1     = chartOfItems(grammar1, s1)
  printChart(grammar1, c1)
  let pt1    = parseTree(grammar1, input1, c1)
  for tree in pt1:
    echo tree.treeRepr(grammar1)

if false:
  let grammar1 = Grammar[char](
    start: "List",
    rules: @[
      rule("List", @[ch '[', n "Elements", ch ']']),
      rule("Elements", @[n "Element", n"Elements1"]),
      rule("Elements1", @[]),
      rule("Elements1", @[ch ',', n "Element", n "Elements1"]),
      rule("Element", @[rng('a', 'z')]),
      rule("Element", @[n "List"])
    ]
  )


  let input1 = makeInput "[a,a,z]"
  let s1     = buildItems(grammar1, input1)
  # printItems(grammar1, s1)
  let c1     = chartOfItems(grammar1, s1)
  # printItems(grammar1, s1, onlyFull = true)
  printChart(grammar1, c1)
  let pt1    = parseTree(grammar1, input1, c1)
  for tree in pt1:
    echo tree.treeRepr(grammar1)
