# import hparse/[
#   lexer,
#   token,
#   parse_tree,
#   grammars,
#   parse_primitives,
#   ll1_gen
# ]

# import sets, tables

# include test_grammar
# include /tmp/nimast_tmp


# var toks = mapString("[[c,z,d,[e,d]],[e,d,f]]").makeStream(
#   nextTokCb = (
#     proc(tok: LTok, pos: int) = echo "Reading ", tok.toTokStr(), " @ ", pos
#   )
# )
# let tree = parser.parse(toks)

# echo "--- FINAL ---"
# echo tree.treeRepr("tk")
# tree.topng("/tmp/image.png", "tk", bottomTokens = true)
