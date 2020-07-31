import parse_primitives, token
import sequtils, strformat, strutils, colors

import hmisc/types/[ graphviz_ast, html_ast, hprimitives ]
import hmisc/algo/htree_mapping
import hmisc/helpers

#*************************************************************************#
#**************************  Type definitions  ***************************#
#*************************************************************************#

#=============================  Primitives  ==============================#


type
  ParseTreeKind* = enum
    ptkToken
    ptkNterm
    ptkList

  ParseTree*[C, L, I] = ref object
    ##[

Parse tree object

## Notes

- `subnodes` field is on top level but is not used for `pkTerm` - value is
  instead stored in `tok` field

    ]##
    action*: TreeAct ## Tree action to execute after construction
    # REVIEW wrap in getter/setter. Use single point for token and
    # multiple values for other elements?
    start*: int ## Start position of tree in input token stream
    finish*: int ## End position of tree in input token stream

    case kind*: ParseTreeKind
      of ptkToken:
        tok*: Token[C, L, I] ## Value of parsed token
      of ptkNTerm:
        # IDEA REVIEW maybe use `RuleId` to retain information about
        # original rules that has been used?
        nterm*: NTermSym ## Name of parsed nonterminal
        subnodes*: seq[ParseTree[C, L, I]] ## Sequence of parsed subnodes
      of ptkList:
        elements*: seq[ParseTree[C, L, I]]




#============================  Constructors  =============================#

proc newTree*[C, L, I](subtree: seq[ParseTree[C, L, I]]): ParseTree[C, L, I] =
  ## Create new parse tree object
  ParseTree[C, L, I](kind: ptkList, elements: subtree)

# proc newTree*[Tok](subtree: seq[ParseTree[Tok]]): ParseTree[Tok] =
#   ## Create new parse tree object
#   ParseTree[Tok](kind: ptkList, elements: toSeq(subtree))

proc newTree*[C, L, I](tok: Token[C, L, I]): ParseTree[C, L, I] =
  ParseTree[C, L, I](kind: ptkToken, tok: tok)

proc newTree*[C, L, I](
  name: NTermSym, subnodes: seq[ParseTree[C, L, I]]): ParseTree[C, L, I] =
  ParseTree[C, L, I](kind: ptkNTerm, nterm: name, subnodes: subnodes)

func tok*[C, L, I](tree: ParseTree[C, L, I]): Token[C, L, I] =
  assert tree.kind == pkTerm
  return tree.tok

func getSubnodes*[C, L, I](tree: ParseTree[C, L, I]): seq[ParseTree[C, L, I]] =
  case tree.kind:
    of ptkNterm: tree.subnodes
    of ptkList: tree.elements
    of ptkToken: @[]

func len*[C, L, I](tree: ParseTree[C, L, I]): int =
  case tree.kind:
    of ptkToken: 0
    of ptkNterm: tree.subnodes.len
    of ptkList: tree.elements.len

#========================  Accessors/predicates  =========================#

#===========================  Pretty-printing  ===========================#

#==============================  graphviz  ===============================#


type
  TokenStyle* = object
    color*: Color

  TokenStyleCb*[C, L, I] = object
    cb*: proc(token: Token[C, L, I]): TokenStyle {.noSideEffect.}

func highlight*[C, L, I](cb: TokenStyleCb[C, L, I], tok: Token[C, L, I]): TokenStyle =
  if cb.cb == nil:
    result.color = colNoColor
  else:
    result = cb.cb(tok)

func toDotGraphPretty*[C, L, I](
  tree: ParseTree[C, L, I],
  kindPref: string,
  bottomTokens: bool,
  colorCb: TokenStyleCb): Graph =
  result.styleNode = Node(shape: nsaRect)
  var tokNodes: seq[Node]

  tree.iterateItBFS(it.getSubnodes(), it.kind != ptkToken):
    let itaddr = toNodeId(cast[int](addr it[]))
    var nextaddr = toNodeId(cast[int](addr it[]) + 1)

    if it.kind == ptkToken:
      when hasPosInfo(it.tok):
        nextaddr = toNodeId(it.tok.getPosInfo() + 1)

      result.addEdge(makeEdge(itaddr, nextaddr))

      let tokNode = makeNode(
        nextaddr,
        ($it.tok).quote(),
        nsaCircle,
        color = colLightGrey,
        style = nstFilled
      )

      if bottomTokens:
        tokNodes.add tokNode
      else:
        result.addNode tokNode

    result.addNode(makeNode(
      itaddr,
      label = case it.kind:
        of ptkNTerm: it.nterm
        of ptkToken: fmt("{it.tok.cat.tokKindStr(kindPref)}")
        else: it.nodeKindStr()
      ,
      shape = case it.kind:
        of ptkNTerm: nsaDefault
        of ptkToken: nsaUnderline
        else: nsaEllipse
      ,
      color = case it.kind:
        of ptkNTerm:
          colLightBlue
        of ptkToken:
          colorCb.highlight(it.tok).color
        else:
          colNoColor
      ,
      style = case it.kind:
        of ptkNTerm: nstFilled
        else: nstDefault
    ))

    for tr in subt:
      result.addEdge(makeEdge(
        itaddr,
        toNodeId(addr tr[])
      ))

  if bottomTokens:
    result.addSubgraph(Graph(
      nodes: tokNodes,
      isWrapper: true,
      noderank: gnrSame
    ))

func toDotGraphPrecise*[C, L, I](tree: ParseTree[C, L, I], kindPref: string): Graph =
  result.styleNode = Node(shape: nsaRect)
  tree.iterateItBFS(it.subnodes, it.kind != ptkToken):
    let itaddr: int = cast[int](addr it[])
    let label = case it.kind:
      of ptkNTerm: it.nterm
      of ptkToken: fmt("{it.tok.cat.tokKindStr(kindPref)}\n'{it.tok}'")
      of ptkList: it.nodeKindStr()

    result.addNode(makeNode(
      itaddr.toNodeId(),
      label = label & (
        block:
          if tree.action != taDefault:
            fmt("\n{tree.action}")
          else:
            ""
      )
    ))

    for tr in subt:
      result.addEdge(makeEdge(
        itaddr.toNodeId(),
        toNodeId(addr tr[])
      ))

func toDotGraph*[C, L, I](
  tree: ParseTree[C, L, I],
  kindPref: string = "",
  preciseRepr: bool = false,
  bottomTokens: bool = false,
  colorCb: TokenStyleCb[C, L, I] = TokenStyleCb[C, L, I](),
  idshift: int = 0): Graph =
  ##[

## Parameters

:bottomTokens: Put all token nodes at bottom. Works only with pretty graph
:idshift: add this number to each node ID. Useful when putting several
  graphs on the same image - this way different subclusters won't be
  interfering with each other's nodes

  ]##
  if preciseRepr:
    result = toDotGraphPrecise(tree, kindPref)
  else:
    result = toDotGraphPretty(tree, kindPref, bottomTokens, colorCb)

  result.idshift = idshift

proc toPng*[C, L, I](
  tree: ParseTree[C, L, I],
  path: string = "/tmp/image.png",
  kindPref: string = "",
  preciseRepr: bool = false,
  bottomTokens: bool = false): void =
  tree.toDotGraph(kindPref, preciseRepr, bottomTokens).topng(path)

#=========================  tree representation  =========================#

func nodeKindStr*[C, L, I](node: ParseTree[C, L, I]): string =
  case node.kind:
    of ptkList: "[ ... ]"
    of ptkNTerm: node.nterm
    else:
      ""

func treeReprImpl*[C, L, I](
  node: ParseTree[C, L, I],
  pref: seq[bool],
  parentMaxIdx, currIdx: int,
  kindPref: string): seq[string] =
  let prefStr = pref.mapIt(
    if it: "|   " else: "    "
  ).join("") & "+-> " & (node.action != taDefault).tern(
    fmt("< {node.action} > "),
    ""
  )

  result = case node.kind:
    of ptkToken:
      @[ fmt(
        "{prefStr}[{node.tok.cat.tokKindStr(kindPref)}, '{node.tok.lex}']"
      ) ]
    of ptkNTerm:
      @[ fmt("{prefStr}{node.nterm}") ]
    of ptkList:
      @[ fmt("{prefStr}[ {node.nodeKindStr()} ]") ]

  for idx, subn in node.getSubnodes():
    result &= subn.treeReprImpl(pref & @[
      currIdx != parentMaxIdx
    ],
    node.len - 1, idx, kindPref)

func treeRepr*[C, L, I](node: ParseTree[C, L, I], kindPref: string = ""): string =
  treeReprImpl(node, @[], 0, 0, kindPref).join("\n")

func lispReprImpl*[C, L, I](
  node: ParseTree[C, L, I],
  kindPref: string, discardEmpty: bool): seq[string] =
  case node.kind:
    of ptkToken:
      var kindStr = $node.tok.kind
      if kindStr.startsWith(kindPref):
        kindStr = kindStr[kindPref.len .. ^1]

      @[ fmt("({kindStr} '{node.tok}')") ]
    else:
      if discardEmpty and (node.getSubnodes().len == 0):
        @[]
      else:
        @[ "(" & node.nodeKindStr() & " " &
          node.subnodes.mapIt(
            it.lispReprImpl(kindPref, discardEmpty)
          ).concat().join(" ") &
          ")" ]


func lispRepr*[C, L, I](
  node: ParseTree[C, L, I],
  kindPref: string = "",
  discardEmpty: bool = true): string =
  lispReprImpl(node, kindPref, discardEmpty).join(" ")


#=====================  Tree actions implementation  =====================#

func runTreeActions*[C, L, I](tree: var ParseTree[C, L, I]): void =
  case tree.action:
    of taDrop: # This tree should be dropped by it's parent
      return
    else:
      discard

  var newsubn: seq[ParseTree[C, L, I]]
  var hadPromotions: bool = false
  let subnodes =
    case tree.kind:
      of ptkNterm: tree.subnodes
      of ptkList: tree.elements
      else: @[]

  for idx in 0 ..< subnodes.len:
    let subnode = subnodes[idx]

    case subnode.kind:
      of ptkToken:
        case subnode.action:
          of taPromote:
            if subnodes.len > 1:
              raiseAssert(msgjoin(
                "Cannot promote terminal node in tree with",
                subnodes.len, "elements",
                # IMPLEMENT generate adequate error messages incuding
                # current token values

                # TODO do not print whole tree, only dump two upper
                # layers, everything else should be represented as
                # `...`

                # ($subnode.tok.kind), " in tree ", tree.lispRepr()
              ))
          of taSpliceDiscard, taSplicePromote:
            raiseAssert(msgjoin(
              "Cannot splice terminal node (it cannot have child",
              "elements): attempted splice",
              # IMPLEMENT
              # subnode.action, "of", ($subnode.tok.kind),
              # " in tree ", tree.lispRepr()
            ))
          else:
            discard
      else:
        discard

    case subnode.action:
      of taDefault:
        newsubn.add subnode
      of taDrop:
        discard
      of taSpliceDiscard:
        newsubn &= subnode.getSubnodes()
      of taSplicePromote:
        tree = subnode
        newsubn &= subnode.getSubnodes()
      of taPromote:
        if not hadPromotions:
          tree = subnode
          newsubn &= subnode.getSubnodes()
        else:
          discard #[ IMPLEMENT repeated promotions ]#
      of taSubrule:
        discard #[ IMPLEMENT ]#


  case tree.kind:
    of ptkNterm:
      tree.subnodes = newsubn
    of ptkList:
      tree.elements = newsubn
    else:
      discard

  tree.action = taDefault
