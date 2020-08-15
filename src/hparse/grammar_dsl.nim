import sugar, strutils, sequtils, strformat, algorithm, parseutils
import macros
import hmisc/[helpers, hexceptions]
import hmisc/types/initcalls
import grammars, token

#===========================  implementation  ============================#



type
  PattTreeKind = enum
    ptkTokenComposed
    ptkTokenKind
    ptkStrLiteral
    ptkNtermName
    ptkTreeAction
    ptkPrefix
    ptkInfix
    ptkCall

  GenConf = object
    catPrefix: string

  PattTree = object
    case kind: PattTreeKind
      of ptkStrLiteral:
        strVal: string
      of ptkNtermName:
        nterm: string
      of ptkTokenKind:
        token: string
      of ptkTokenComposed:
        catVal: string
        lexVal: NimNode
      of ptkTreeAction, ptkPrefix:
        prefix: string
        elementItem: seq[PattTree]
      of ptkInfix:
        infix: string
        elems: seq[PattTree]
      of ptkCall:
        expr: NimNode

func `==`*(lhs, rhs: PattTree): bool =
  lhs.kind == rhs.kind and (
    case lhs.kind:
      of ptkStrLiteral: lhs.strVal == rhs.strVal
      of ptkNtermName: lhs.nterm == rhs.nterm
      of ptkTokenKind: lhs.token == rhs.token
      of ptkTreeAction, ptkPrefix:
        (lhs.prefix == rhs.prefix) and
        (lhs.elementItem[0] == rhs.elementItem[0])
      of ptkInfix:
        (lhs.infix == rhs.infix) and
        subnodesEq(lhs, rhs, elems)
      of ptkTokenComposed:
        (lhs.catVal == rhs.catVal) and (lhs.lexVal == rhs.lexVal)
      of ptkCall:
        (lhs.expr == rhs.expr)
  )

func newPattTree(infix: string, elems: seq[PattTree]): PattTree =
  PattTree(kind: ptkInfix, infix: infix, elems: elems)

const
  treeActionSigils = ["!", "@", "^", "^@"]
  treeActionSigils2 = [
    "!*", "!+", "@+", "@*", "^*", "^+", "!?", "@?", "^?"]
  treeActionSigils3 = ["^@*", "^@+", "^@?"]
  treeActionSigilsInvalid = [
    "*!", "+!", "*@", "+@", "*^@", "+^@", "+^", "*^"]
  patternSigils = ["*", "+", "?"]



func newPattTree(prefixNode: NimNode, patt: PattTree): PattTree =
  let prefix = prefixNode.strVal()
  case prefix:
    of treeActionSigils:
      PattTree(kind: ptkTreeAction, prefix: prefix, elementItem: @[patt])
    of patternSigils:
      PattTree(kind: ptkPrefix, prefix: prefix, elementItem: @[patt])
    of treeActionSigils2:
      PattTree(kind: ptkTreeAction, prefix: $prefix[0], elementItem: @[
        PattTree(kind: ptkPrefix, prefix: $prefix[1], elementItem: @[patt])
      ])
    of treeActionSigils3:
      PattTree(kind: ptkTreeAction, prefix: "^@", elementItem: @[
        PattTree(kind: ptkPrefix, prefix: $prefix[2], elementItem: @[patt])
      ])
    else:
      let annot =
        case prefix:
          of treeActionSigilsInvalid:
            "Incorrect transposition of elements"
          else:
            "Incorrect combination"

      raiseCodeError(
        prefixNode, &"Unexpected prefix: '{prefix}'", annot, -1)


proc flattenPatt(node: NimNode, conf: GenConf): PattTree
proc newPattTree(node: NimNode, conf: GenConf): PattTree =
  case node.kind:
    of nnkIdent:
      let str: string = node.strVal()
      if str[0].isUpperAscii():
        PattTree(kind: ptkNtermName, nterm: str)
      else:
        PattTree(kind: ptkTokenKind, token: str)
    of nnkStrLit:
      PattTree(kind: ptkStrLiteral, strval: node.strVal())
    of nnkPar:
      flattenPatt(node[0], conf)
    of nnkPrefix, nnkDotExpr:
      flattenPatt(node, conf)
    of nnkBracket:
      if node.len > 1:
        raiseCodeError(
          node[1], "Expected one element for optional brace",
          "Use `&` for concatenation")

      PattTree(kind: ptkPrefix, prefix: "?", elementItem: @[
        flattenPatt(node[0], conf)
      ])
    of nnkCurly:
      if node.len > 1:
        raiseCodeError(node[1], "Expected one element for subrule",
                       "Use `&` for concatenation")
      PattTree(kind: ptkTreeAction, prefix: "{}", elementItem: @[
        flattenPatt(node[0], conf)
      ])
    else:
      raiseCodeError(
        node, &"Unexpected node kind for `newPattTree` {node.kind}",
        "", 0)

proc flattenPatt(node: NimNode, conf: GenConf): PattTree =
  # echo node.treeRepr()
  case node.kind:
    of nnkIdent, nnkStrLit:
      return newPattTree(node, conf)
    of nnkPrefix:
      if node[1].kind == nnkCurly:
        # NOTE right now I'm not sure how prefix for subrule should be
        # handled - it is quite easy to enable later.
        raiseCodeError(
          node,
          "Cannot use prefix for subrule tree action",
          "`taSubrule`")

      case $node[0]:
        of "%":
          return PattTree(kind: ptkCall, expr: node[1])
        else:
          return newPattTree(node[0], flattenPatt(node[1], conf))
    of nnkInfix:
      var curr = node
      let infix = $node[0]
      result = newPattTree(infix, @[])
      while curr.kind == nnkInfix and curr[0] == ident(infix):
        result.elems.add flattenPatt(curr[2], conf)
        curr = curr[1]

      result.elems.add newPattTree(curr, conf)
      result.elems.reverse
    of nnkPar:
      result = node[0].flattenPatt(conf)
    of nnkBracket:
      if node.len > 1:
        raiseCodeError(
          node[1], "Expected one element for optional brace",
          "Use `&` for concatenation")

      result = PattTree(kind: ptkPrefix, prefix: "?", elementItem: @[
        node[0].flattenPatt(conf)
      ])
    of nnkDotExpr:
      assertNodeIt(node[1], node[1].kind == nnkIdent, "Expected identifier")
      let nodeStr =
        block:
          let str = node[1].strVal()
          if str.startsWith(conf.catPrefix):
            str
          else:
            conf.catPrefix & str.capitalizeAscii()

      let catNode = node[1]
      result = PattTree(
        kind: ptkTokenComposed,
        lexVal: node[0],
        catVal: nodeStr
      )
    else:
      raiseCodeError(node, "Unexpected node kind", $node.kind)

proc toCalls(patt: PattTree): NimNode =
  case patt.kind:
    of ptkStrLiteral:
      newCall("tok", newLit(patt.strVal))
    of ptkTokenComposed:
      newCall("tok", ident(patt.catVal), patt.lexVal)
    of ptkCall:
      patt.expr
    of ptkTreeAction:
      let actionName: string =
        case patt.prefix:
          of "^": "taPromote"
          of "@": "taSpliceDiscard"
          of "!": "taDrop"
          of "^@": "taSplicePromote"
          of "{}": "taSubrule"
          else:
            raiseAssert(&"Invalid tree action prefix: '{patt.prefix}'")

      newCall(
        "addAction",
        toCalls(patt.elementItem[0]),
        ident actionName
      )

    of ptkPrefix:
      let opName: string =
        case patt.prefix:
          of "*": "zeroP"
          of "+": "oneP"
          of "?": "optP"
          else:
            raiseAssert(&"Invalid patt prefix: {patt.prefix}")

      newCall(opName, toCalls(patt.elementItem[0]))

    of ptkInfix:
      let infName: string =
        case patt.infix:
          of "&": "andP"
          of "|": "orP"
          else:
            raiseAssert(&"Invalid infix: {patt.infix}")

      var call = newCall(infName)
      for elem in patt.elems:
        call.add toCalls(elem)

      call
    of ptkTokenKind:
      newCall("tok", ident patt.token)
    of ptkNtermName:
      newCall("nt", newLit(patt.nterm))

proc generateGrammar*(body: NimNode, catPrefix: string = ""): NimNode =
  let body =
    if body.kind == nnkStmtList: body
    else: newStmtList(body)

  result = nnkTableConstr.newTree()
  for rule in body:
    assert rule.kind == nnkInfix and $rule[0] == "::="
    if rule[2] == ident("null"):
      result.add newColonExpr(newLit($rule[1]), newCall("null"))
    else:
      result.add newColonExpr(
        newLit($rule[1]),
        rule[2].flattenPatt(GenConf(
          catPrefix: catPrefix
        )).toCalls())

  # echo result.treeRepr()
  # echo result.toStrLit()

func tokMaker*[C, L](cat: C): Patt[C, L] = grammars.tok[C, L](cat)

func getEnumPref*(en: NimNode): string =
  let impl = en.getTypeImpl()
  # debugecho impl.treeRepr()
  let name = impl[1].strVal()
  let pref = name.parseUntil(result, {'A' .. 'Z'})

func isEnum*(en: NimNode): bool = en.getTypeImpl().kind == nnkEnumTy

macro initGrammarImpl*(body: untyped): untyped =
  generateGrammar(body)

macro initGrammarImplCat*(cat: typed, body: untyped): untyped =
  if cat.isEnum:
    generateGrammar(body, catPrefix = getEnumPref(cat))
  else:
    generateGrammar(body)

template initGrammarCalls*(catT, lexT: typed): untyped {.dirty.} =
  proc nt(str: string): Patt[catT, lexT] = nterm[catT, lexT](str)
  when catT is void:
    proc tok(lex: string): Patt[catT, lexT] = voidCatTok[lexT](lex)
  else:
    proc tok(lex: string): Patt[catT, lexT] = tok[catT, lexT](catT(0), lex)

  proc null(): Patt[catT, lexT] = nullP[catT, lexT]()

  when not (catT is void):
    proc tok(cat: catT): Patt[catT, lexT] = tokMaker[catT, lexT](cat)


template initGrammar*[C, L](body: untyped): untyped =
  # static: echo body.astTostr()
  block:
    initGrammarCalls(C, L)
    initGrammarImplCat(C, body)

template initGrammarConst*[C, L](cname: untyped, body: untyped): untyped =
  const cname =
    block:
      initGrammarCalls(C, L)
      initGrammarImplCat(C, body)
