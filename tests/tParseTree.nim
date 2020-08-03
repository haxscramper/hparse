import sugar, strutils, sequtils, strformat

import hparse/[parse_tree, parse_primitives, token]
import hmisc/helpers

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

func ntree(nterm: string,
           acts: openarray[(int, TreeAct)] = []
          ): ParseTree[int, string, void] =
  initParseTree[int, string, void](nterm, acts)

func ntree(tok: int, lex: string = "--"): ParseTree[int, string, void] =
  newTree(Token[int, string, void](cat: tok, lex: lex))

suite "Tree actions primitives":
  test "test":
    let actions = @{0 : taDrop, 1 : taSpliceDiscard}
    for act in [actions, emptySeq[(int, TreeAct)]()]:
      echo "---"
      var tree = ntree("top", act)
      tree.add ntree(30)
      tree.add newTree("sube", @[ntree(2), ntree(12)])

      echo tree.treeRepr()
