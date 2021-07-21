# {.define(plainStdout).}

import sets, sequtils, options
import grammar_dsl,
  parse_tree,
  grammars,
  token,
  lexer,
  parse_primitives,
  bnf_grammars,
  llstar_gen,
  ll1_table

import hmisc/extra/hdrawing/term_buf

export grammar_dsl,
  parse_tree,
  grammars,
  token,
  lexer,
  parse_primitives,
  bnf_grammars,
  sets,
  sequtils,
  llstar_gen,
  options,
  ll1_table

template exampleGrammar*(body: untyped): untyped =
  block:
    const defaultCategory {.inject.} = catNoCategory
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, body)


template exampleGrammarConst*(name, body: untyped): untyped =
  const name = block:
    const defaultCategory {.inject.} = catNoCategory
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, body)

template exampleParser*(body: untyped): untyped =
  newLLStarParser[NoCategory, string, void]:
    body

template exampleStream*(str: typed): untyped =
  var stream =
    when str is string:
      str.mapIt($it).filterIt(it == " ").makeTokens().makeStream()
    else:
      str.makeTokens().makeStream()

  stream



template exampleParse*(str: typed, grammar: untyped): untyped =
  block:
    const defaultCategory {.inject.} = catNoCategory
    const name = block:
      initGrammarCalls(NoCategory, string)
      initGrammarImplCat(NoCategory, grammar)

    let grm = name
    # echo grm.toGrammar().exprRepr()
    let parser = exampleParser(name)
    var stream = exampleStream(str)
    let tree = parser.parse(stream)
    tree

template eparse*(str: typed, grm: untyped): untyped =
  let tree = exampleParse(str, grm)
  tree.treeRepr()

template ecompare*(input: typed, gr1, gr2: untyped): untyped =
  sideBySide(
    eparse(input, gr1),
    eparse(input, gr2)
  )

# func sideBySide*(str1, str2: string): string =
#   concatBufsLeft(@[
#     str1.toTermBuf(),
#     makeTermBuf(w = 3, h = 1),
#     str2.toTermBuf()
#   ]).toString()

template exampleParseBNF*(str: typed, grammar: untyped): untyped =
  let name = block:
    const defaultCategory = catNoCategory
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, grammar)

  let parser = newLL1Tableparser[NoCategory, string](name.toGrammar())
  var stream = exampleStream(str)
  let tree = parser.parse(stream)
  tree

template exampleParseBNF_noFix*(str: typed, grammar: untyped): untyped =
  let name = block:
    const defaultCategory = catNoCategory
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, grammar)

  let parser = newLL1Tableparser[NoCategory, string](
    name.toGrammar(),
    dofixup = false,
    retainGenerated = true
  )

  var stream = exampleStream(str)
  let tree = parser.parse(stream)
  tree

template treeCompare*(str: typed, grammar: untyped): untyped =
  let ebnfTree: string = treeRepr(exampleParse(str, grammar))

  var bnfTree: string
  try:
    bnfTree = treeRepr(exampleParseBnf_noFix(str, grammar))
  except:
    bnfTree = getCurrentExceptionMsg()

  let instr {.inject.} = sideBySide(
    "EBNF tree\n" & ebnfTree,
    "BNF tree\n" & bnfTree
  )

  let grm {.inject.} = strip(astToStr(grammar))
  let inToks {.inject.} = str

  &"""
Grammar: {grm}
Input__: {inToks}

{instr}
"""
