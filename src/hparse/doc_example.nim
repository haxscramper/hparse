# {.define(plainStdout).}

import sets, sequtils, options
import grammar_dsl,
  parse_tree,
  grammars,
  token,
  lexer,
  parse_primitives,
  bnf_grammars,
  llstar_gen


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
  options

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

template exampleParse*(str: string, grammar: untyped): untyped =
  const name = block:
    initGrammarCalls(NoCategory, string)
    initGrammarImplCat(NoCategory, grammar)

  let parser = exampleParser(name)
  var stream =
    when str is string:
      str.mapIt($it).filterIt(it == " ").makeTokens().makeStream()
    else:
      str.makeTokens().makeStream()

  let tree = parser.parse(stream)
  tree
