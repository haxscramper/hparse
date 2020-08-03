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
    let actions = @{
      0 : taDrop,
      1 : taSpliceDiscard,
      2 : taSubrule,
      3 : taSubrule,
      5 : taSplicePromote
    }
    for act in [actions, emptySeq[(int, TreeAct)]()]:
      echo "---"
      var tree = ntree("top", act)
      tree.add ntree(30) # Dropped
      tree.add newTree("sube", @[ntree(2), ntree(12)]) # Spliced
      tree.add ntree(20) # Moved to subrule
      tree.add ntree(90)
      tree.add ntree(20) # Added as usual
      tree.add newTree("splice-promote", @[ntree(777)])


      echo tree.treeRepr()
