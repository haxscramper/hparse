import sugar, strutils, sequtils, strformat, algorithm
import macros
import hmisc/helpers
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
  )

func newPattTree(infix: string, elems: seq[PattTree]): PattTree =
  PattTree(kind: ptkInfix, infix: infix, elems: elems)

func newPattTree(prefix: string, patt: PattTree): PattTree =
  case prefix:
    of "!", "@", "^", "^@":
      PattTree(kind: ptkTreeAction, prefix: prefix, elementItem: @[patt])
    of "*", "+":
      PattTree(kind: ptkPrefix, prefix: prefix, elementItem: @[patt])
    else:
      raiseAssert("Unexpected prefix ident: {prefix}")


func newPattTree(node: NimNode): PattTree =
  case node.kind:
    of nnkIdent:
      let str: string = node.strVal()
      if str[0].isUpperAscii():
        PattTree(kind: ptkNtermName, nterm: str)
      else:
        PattTree(kind: ptkTokenKind, token: str)
    of nnkStrLit:
      PattTree(kind: ptkStrLiteral, strval: node.strVal())
    else:
      raiseAssert(
        &"Unexpected node kind for `newPattTree` {node.kind} " &
        &"but expected `nnkIdent` or `nnkStrLit`")

proc flattenPatt(node: NimNode): PattTree =
  case node.kind:
    of nnkIdent, nnkStrLit:
      return newPattTree(node)
    of nnkPrefix:
      return newPattTree($node[0], flattenPatt(node[1]))
    of nnkInfix:
      var curr = node
      let infix = $node[0]
      result = newPattTree(infix, @[])
      while curr.kind == nnkInfix and curr[0] == ident(infix):
        result.elems.add flattenPatt(curr[2])
        curr = curr[1]

      result.elems.add newPattTree(curr)
      result.elems.reverse
    else:
      echo node.treeRepr()

proc toCalls(patt: PattTree): NimNode =
  case patt.kind:
    of ptkStrLiteral:
      newCall("strLit", newLit(patt.strVal))
    of ptkTreeAction:
      let actionName: string =
        case patt.prefix:
          of "^": "taPromote"
          of "@": "taSpliceDiscard"
          of "!": "taDrop"
          of "^@:": "taSplicePromote"
          else:
            raiseAssert(&"Invalid tree action prefix: {patt.prefix}")

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

macro makeGrammar(body: untyped): untyped =
  let body =
    if body.kind == nnkStmtList: body
    else: newStmtList(body)

  result = nnkTableConstr.newTree()
  for rule in body:
    assert rule.kind == nnkInfix and $rule[0] == "::="
    result.add newColonExpr(
      newLit($rule[1]),
      rule[2].flattenPatt().toCalls())

  echo result.toStrLit()



#================================  tests  ================================#


import unittest
import grammars
const nt = nterm[char]

suite "Grammar primitives":
  test "test":
    # makeGrammar(A ::= B)
    # makeGrammar(A ::= *B)
    assert makeGrammar(A ::= Q & B & C) ==
      {"A" : andP(nt("Q"), nt("B"), nt("C"))}
    # makeGrammar(A ::= "$" & *(Z))
    # makeGrammar(A ::= (A | B) & C)
    block:
      let grammar = makeGrammar:
        A ::= B
        C ::= D

      assertEq {"A" : nt("B"), "C" : nt("D")}, grammar

    block:
      let grammar = makeGrammar:
        List ::= "[" & Elements & "]"
        Elements ::= Element & *("," & Element)
        Element ::= ident | List

      # assertEq {
      #   "List" :
      # }
