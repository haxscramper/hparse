import sugar, strutils, sequtils, strformat, algorithm
import macros
import hmisc/[helpers, hexceptions]
import grammars

#===========================  implementation  ============================#



type
  PattTreeKind = enum
    ptkTokenKind
    ptkStrLiteral
    ptkNtermName
    ptkTreeAction
    ptkPrefix
    ptkInfix
    ptkCall

  PattTree = object
    case kind: PattTreeKind
      of ptkStrLiteral:
        strVal: string
      of ptkNtermName:
        nterm: string
      of ptkTokenKind:
        token: string
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


proc flattenPatt(node: NimNode): PattTree
proc newPattTree(node: NimNode): PattTree =
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
      flattenPatt(node[0])
    of nnkPrefix:
      flattenPatt(node)
    of nnkBracket:
      if node.len > 1:
        raiseCodeError(
          node[1], "Expected one element for optional brace",
          "Use `&` for concatenation")

      PattTree(kind: ptkPrefix, prefix: "?", elementItem: @[
        flattenPatt(node[0])
      ])
    of nnkCurly:
      if node.len > 1:
        raiseCodeError(node[1], "Expected one element for subrule",
                       "Use `&` for concatenation")
      PattTree(kind: ptkTreeAction, prefix: "{}", elementItem: @[
        flattenPatt(node[0])
      ])
    else:
      raiseCodeError(
        node, &"Unexpected node kind for `newPattTree` {node.kind}",
        "", 0)

proc flattenPatt(node: NimNode): PattTree =
  # echo node.treeRepr()
  case node.kind:
    of nnkIdent, nnkStrLit:
      return newPattTree(node)
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
          return newPattTree(node[0], flattenPatt(node[1]))
    of nnkInfix:
      var curr = node
      let infix = $node[0]
      result = newPattTree(infix, @[])
      while curr.kind == nnkInfix and curr[0] == ident(infix):
        result.elems.add flattenPatt(curr[2])
        curr = curr[1]

      result.elems.add newPattTree(curr)
      result.elems.reverse
    of nnkPar:
      result = node[0].flattenPatt()
    of nnkBracket:
      if node.len > 1:
        raiseCodeError(
          node[1], "Expected one element for optional brace",
          "Use `&` for concatenation")

      result = PattTree(kind: ptkPrefix, prefix: "?", elementItem: @[
        node[0].flattenPatt()
      ])
    else:
      raiseCodeError(node, "Unexpected node kind", $node.kind)

proc toCalls(patt: PattTree): NimNode =
  case patt.kind:
    of ptkStrLiteral:
      newCall("tok", newLit(patt.strVal))
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

proc generateGrammar*(body: NimNode): NimNode =
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
        rule[2].flattenPatt().toCalls())

  # echo result.toStrLit()


macro initGrammarImpl*(body: untyped): untyped = generateGrammar(body)

template initGrammarCalls*(catT, lexT: typed): untyped {.dirty.} =
  proc nt(str: string): Patt[catT, lexT] = nterm[catT, lexT](str)
  when catT is void:
    proc tok(lex: string): Patt[catT, lexT] = voidCatTok[lexT](lex)
  else:
    proc tok(lex: string): Patt[catT, lexT] = tok[catT, lexT](catT(0), lex)

  proc null(): Patt[catT, lexT] = nullP[catT, lexT]()

  when not (catT is void):
    proc tok(cat: catT): Patt[catT, lexT] =
      grammars.tok[catT, lexT](cat)


template initGrammar*[C, L](body: untyped): untyped =
  # static: echo body.astTostr()
  block:
    initGrammarCalls(C, L)
    initGrammarImpl(body)

template initGrammarConst*[C, L](cname: untyped, body: untyped): untyped =
  const cname =
    block:
      initGrammarCalls(C, L)
      initGrammarImpl(body)
