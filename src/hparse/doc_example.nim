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
