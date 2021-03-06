import sugar, strutils, sequtils, strformat, algorithm, parseutils
import macros
import hmisc/[helpers, hexceptions]
import hnimast
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
    ptkPredicate
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
      of ptkPredicate:
        expBuilder: NimNode

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
      of ptkCall: (lhs.expr == rhs.expr)
      of ptkPredicate:
        lhs.expBuilder == rhs.expBuilder
        # (lhs.predBodyStr == rhs.predBodyStr) and
        # (lhs.tokenCat == rhs.tokenCat) and
        # (lhs.defaultCat == rhs.defaultCat)
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


func toCatPrefix(str, pref: string): string =
  if str.startsWith(pref):
    str
  else:
    pref & str.capitalizeAscii()

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
            "Incorrect prefix combination"

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
        PattTree(
          kind: ptkTokenKind, token: str.toCatPrefix(conf.catPrefix))
    of nnkStrLit:
      PattTree(kind: ptkStrLiteral, strval: node.strVal())
    of nnkPar:
      flattenPatt(node[0], conf)
    of nnkPrefix, nnkDotExpr:
      flattenPatt(node, conf)
    of nnkBracket:
      if node[0].kind == nnkBracket:
        if node[0].len > 1:
          raiseCodeError(node[0][1],
                         "Expected one element for predicate token")

        let impl = node[0][0]
        let tokBuilder =
          case impl.kind:
            of nnkInfix, nnkCall, nnkDotExpr:
              let
                itId = ident "it"
              let cbImpl = quote do:
                (
                  block:
                    proc tmp(`itId`: LexType): bool {.noSideEffect.} =
                      `impl`

                    tmp
                )

              var tmp = newCall(
                "makeExpTokenPredBuilder",
                ident("defaultCategory"),
                cbImpl,
                impl.toStrLit(),
                cbImpl.toStrLit()
              )


              tmp[4] = tmp.toStrLit()
              # echov tmp.toStrLit()
              tmp
            else:
              let impl = quote do:
                makeExpTokenPredUsr(defaultCategory, `impl`)

              let implAssert = toCompilesAssert(
                node, impl, &"Generated {impl.toStrLit()}")

              let strlit = impl.toStrLit()

              let res = quote do:
                block:
                  `implAssert`
                  var tmp = `impl`
                  tmp.lexPredLiteral = `strlit`
                  tmp

              # debugecho res.toStrLit()
              res


        PattTree(kind: ptkPredicate, expBuilder: tokBuilder)
      else:
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
    of nnkIdent, nnkStrLit, nnkCurly:
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
      if node[0].kind == nnkBracket:
        result = newPattTree(node, conf)
      else:
        if node.len > 1:
          raiseCodeError(
            node[1], "Expected one element for optional brace",
            "Use `&` for concatenation")

        result = PattTree(kind: ptkPrefix, prefix: "?", elementItem: @[
          node[0].flattenPatt(conf)
        ])
    of nnkDotExpr:
      assertNodeIt(node[1], node[1].kind == nnkIdent, "Expected identifier")
      let nodeStr = node[1].strVal().toCatPrefix(conf.catPrefix)
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
      newCall("dslTok", newLit(patt.strVal))
    of ptkPredicate:
      newCall("tok", patt.expBuilder)
    of ptkTokenComposed:
      newCall("dslTok", ident(patt.catVal), patt.lexVal)
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
      newCall("dslTok", ident patt.token)
    of ptkNtermName:
      newCall("nt", newLit(patt.nterm))

proc generateGrammar*(body: NimNode, catPrefix: string = ""): NimNode =
  let body =
    if body.kind == nnkStmtList: body
    else: newStmtList(body)

  result = nnkTableConstr.newTree()
  for rule in body:
    assertNodeIt(rule, rule.kind == nnkInfix,
                 "Expected infix <head> ::= <production>",
                 &"Expression kind: {rule.kind}")

    assertNodeIt(rule[0], $rule[0] == "::=",
                 &"Invalid infix: {rule[0]}",
                  "expected ::=")

    # assert rule.kind == nnkInfix and $rule[0] == "::="
    if rule[2] == ident("null"):
      result.add newColonExpr(newLit($rule[1]), newCall("null"))
    else:
      result.add newColonExpr(
        newLit($rule[1]),
        rule[2].flattenPatt(GenConf(
          catPrefix: catPrefix
        )).toCalls())

  # echo result.treeRepr()
  # echov catPrefix
  # echo result.toStrLit()
  # for val in result:
  #   val[1].pprintCalls(0)

func tokMaker*[C, L](cat: C): Patt[C, L] = grammars.tok[C, L](cat)


macro initGrammarImpl*(body: untyped): untyped =
  generateGrammar(body)

macro initGrammarImplCat*(cat: typed, body: untyped): untyped =
  if cat.isEnum:
    generateGrammar(body, catPrefix = getEnumPref(cat))
  else:
    generateGrammar(body)

template initGrammarCalls*(catT, lexT: typed): untyped {.dirty.} =
  proc nt(str: string): Patt[catT, lexT] = nterm[catT, lexT](str)
  static:
    assert declared(defaultCategory),
      "Must declare `defaultCategory` const"
    when declared(defaultCategory):
      assert defaultCategory is catT
  # mixin makeExpectedToken
  type LexType = lexT
  # var defaultCategoryTmp {.compileTime}: catT
  # const defaultCategory = defaultCategoryTmp
  # when catT is void:
  #   proc dslTok(lex: string): Patt[catT, lexT] = voidCatTok[lexT](lex)
  # else:
  proc dslTok(lex: string): Patt[catT, lexT] = tok(
    makeExpToken(defaultCategory, lex))

  when not (catT is void) or (lexT is void):
    proc dslTok(cat: catT, lex: lexT): Patt[catT, lexT] = tok(
      makeExpToken(cat, lex))

  when not (lexT is string):
    proc dslTok(cat: catT, lex: string): Patt[catT, lexT] = tok(
      makeExpToken(cat, lex))
  # elif (lexT is string) and (catT is NoCategory):
  #   proc dslTok(lex: string): Patt[NoCategory, string] =
  #     tok(makeExpToken(catNoCategory, lex))

  proc null(): Patt[catT, lexT] = nullP[catT, lexT]()

  when not (catT is void):
    proc dslTok(cat: catT): Patt[catT, lexT] = tokMaker[catT, lexT](cat)

  # template makeExpTokenPred(
  #   lexStr: string, body: LexPredicate[lexT]): untyped =
  #   # var cat: catT
  #   makeExpTokenPred[catT, lexT](cat, lexStr, body, lexImplLiteral)


template initGrammar*[C, L](body: untyped): untyped =
  # static: echo body.astTostr()
  block:
    initGrammarCalls(C, L)
    initGrammarImplCat(C, body)

template initGrammarConst*[C, L](cname: untyped, body: untyped): untyped =
  const cname =
    block:
      mixin hash
      initGrammarCalls(C, L)
      initGrammarImplCat(C, body)
