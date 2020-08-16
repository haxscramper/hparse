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
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, body)


template exampleGrammarConst*(name, body: untyped): untyped =
  const name = block:
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
  const name = block:
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, grammar)

  let parser = exampleParser(name)
  var stream = exampleStream(str)
  let tree = parser.parse(stream)
  tree



template exampleParseBNF*(str: typed, grammar: untyped): untyped =
  let name = block:
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, grammar)

  let parser = newLL1Tableparser[NoCategory, string](name.toGrammar())
  var stream = exampleStream(str)
  let tree = parser.parse(stream)
  tree

template exampleParseBNF_noFix*(str: typed, grammar: untyped): untyped =
  let name = block:
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, grammar)

  let parser = newLL1Tableparser[NoCategory, string](name.toGrammar())
  var stream = exampleStream(str)
  let tree = parser.parse(stream)
  tree
