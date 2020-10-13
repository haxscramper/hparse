{.push header: "<tree_sitter/api.h>"}

{.pragma: cstruct, importc, incompleteStruct.}
{.pragma: cproc, importc, cdecl.}

type
  TSTree* {.cstruct.} = object
  PtsTree* = ptr TSTree

  TSLanguage* {.cstruct.} = object
  PtsLanguage* = ptr TSLanguage

  TSParser* {.cstruct.} = object
  PtsParser* = ptr TSParser

  TSInput* {.cstruct.} = object
  PtsInput* = ptr TSInput

  TSNode* {.cstruct.} = object
    context*: array[4, uint32]
    id*: pointer
    tree*: PtsTree

proc ts_parser_parse(
  self: PtsParser, oldTree: PtsTree, input: PtsInput): PtsTree {.cproc.}

proc ts_parser_new*(): PtsParser {.cproc.}
proc ts_parser_delete*(parser: PtsParser) {.cproc.}
proc ts_parser_set_language*(
  self: PtsParser; language: PtsLanguage): bool {.cproc.}

proc ts_parser_language*(self: PtsParser): PtsLanguage {.cproc.}
# proc ts_parser_set_included_ranges*(
#   self: PtsParser; ranges: PtsRange; length: uint32) {.cproc.}

# proc ts_parser_included_ranges*(
#   self: PtsParser; length: ptr uint32): ptr TSRange {.cproc.}

proc ts_parser_parse*(
  self: PtsParser; old_tree: PtsTree; input: TSInput): TSTree {.cproc.}

proc ts_parser_parse_string*(
  self: PtsParser; old_tree: PtsTree;
  inString: cstring; length: uint32): PtsTree {.cproc.}

# proc ts_parser_parse_string_encoding*(
#   self: PtsParser; old_tree: ptr TSTree;
#   string: cstring; length: uint32; encoding: TSInputEncoding): ptr TSTree {.

#     importc, cdecl, impapiHdr.}
# proc ts_parser_reset*(self: PtsParser) {.importc, cdecl, impapiHdr.}
# proc ts_parser_set_timeout_micros*(self: PtsParser; timeout: uint64) {.importc,
#     cdecl, impapiHdr.}
# proc ts_parser_timeout_micros*(self: PtsParser): uint64 {.importc, cdecl, impapiHdr.}
# proc ts_parser_set_cancellation_flag*(self: PtsParser; flag: ptr uint) {.importc,
#     cdecl, impapiHdr.}
# proc ts_parser_cancellation_flag*(self: PtsParser): ptr uint {.importc, cdecl,
#     impapiHdr.}
# proc ts_parser_set_logger*(self: PtsParser; logger: TSLogger) {.importc, cdecl,
#     impapiHdr.}
# proc ts_parser_logger*(self: PtsParser): TSLogger {.importc, cdecl, impapiHdr.}
# proc ts_parser_print_dot_graphs*(self: PtsParser; file: cint) {.importc, cdecl,
#     impapiHdr.}
# proc ts_parser_halt_on_error*(self: PtsParser; halt: bool) {.importc, cdecl,
#     impapiHdr.}
# proc ts_tree_copy*(self: ptr TSTree): ptr TSTree {.importc, cdecl, impapiHdr.}
# proc ts_tree_delete*(self: ptr TSTree) {.importc, cdecl, impapiHdr.}
proc ts_tree_root_node*(self: PtsTree): TSNode {.cproc.}
# proc ts_tree_language*(a1: ptr TSTree): ptr TSLanguage {.importc, cdecl, impapiHdr.}
# proc ts_tree_edit*(self: ptr TSTree; edit: ptr TSInputEdit) {.importc, cdecl, impapiHdr.}
# proc ts_tree_get_changed_ranges*(old_tree: ptr TSTree; new_tree: ptr TSTree;
#                                 length: ptr uint32): ptr TSRange {.importc, cdecl,
#     impapiHdr.}
# proc ts_tree_print_dot_graph*(a1: ptr TSTree; a2: File) {.importc, cdecl, impapiHdr.}
proc ts_node_type*(a1: TSNode): cstring {.cproc.}
# proc ts_node_symbol*(a1: TSNode): TSSymbol {.cproc.}
proc ts_node_start_byte*(a1: TSNode): uint32 {.cproc.}
# proc ts_node_start_point*(a1: TSNode): TSPoint {.cproc.}
proc ts_node_end_byte*(a1: TSNode): uint32 {.cproc.}
# proc ts_node_end_point*(a1: TSNode): TSPoint {.cproc.}
proc ts_node_string*(a1: TSNode): cstring {.cproc.}
proc ts_node_is_null*(a1: TSNode): bool {.cproc.}
proc ts_node_is_named*(a1: TSNode): bool {.cproc.}
proc ts_node_is_missing*(a1: TSNode): bool {.cproc.}
proc ts_node_is_extra*(a1: TSNode): bool {.cproc.}
proc ts_node_has_changes*(a1: TSNode): bool {.cproc.}
proc ts_node_has_error*(a1: TSNode): bool {.cproc.}
proc ts_node_parent*(a1: TSNode): TSNode {.cproc.}
proc ts_node_child*(a1: TSNode; a2: uint32): TSNode {.cproc.}
proc ts_node_child_count*(a1: TSNode): uint32 {.cproc.}
proc ts_node_named_child*(a1: TSNode; a2: uint32): TSNode {.cproc.}
proc ts_node_named_child_count*(a1: TSNode): uint32 {.cproc.}
proc ts_node_child_by_field_name*(
  self: TSNode; field_name: cstring;
  field_name_length: uint32): TSNode {.cproc.}

# proc ts_node_child_by_field_id*(
#   a1: TSNode; a2: TSFieldId): TSNode {.cproc.}

proc ts_node_next_sibling*(a1: TSNode): TSNode {.cproc.}
proc ts_node_prev_sibling*(a1: TSNode): TSNode {.cproc.}
proc ts_node_next_named_sibling*(a1: TSNode): TSNode {.cproc.}
proc ts_node_prev_named_sibling*(a1: TSNode): TSNode {.cproc.}
proc ts_node_first_child_for_byte*(
  a1: TSNode; a2: uint32): TSNode {.cproc.}

proc ts_node_first_named_child_for_byte*(
  a1: TSNode; a2: uint32): TSNode {.cproc.}

proc ts_node_descendant_for_byte_range*(
  a1: TSNode; a2: uint32; a3: uint32): TSNode {.cproc.}
# proc ts_node_descendant_for_point_range*(
#   a1: TSNode; a2: TSPoint; a3: TSPoint): TSNode {.cproc.}

proc ts_node_named_descendant_for_byte_range*(
  a1: TSNode; a2: uint32; a3: uint32): TSNode {.cproc.}

# proc ts_node_named_descendant_for_point_range*(
#   a1: TSNode; a2: TSPoint; a3: TSPoint): TSNode {.cproc.}


# proc ts_node_edit*(a1: ptr TSNode; a2: ptr TSInputEdit) {.cproc.}
# proc ts_node_eq*(a1: TSNode; a2: TSNode): bool {.cproc.}
# proc ts_tree_cursor_new*(a1: TSNode): TSTreeCursor {.cproc.}
# proc ts_tree_cursor_delete*(a1: ptr TSTreeCursor) {.cproc.}
# proc ts_tree_cursor_reset*(a1: ptr TSTreeCursor; a2: TSNode) {.cproc.}
# proc ts_tree_cursor_current_node*(a1: ptr TSTreeCursor): TSNode {.importc, cdecl,
#     impapiHdr.}
# proc ts_tree_cursor_current_field_name*(a1: ptr TSTreeCursor): cstring {.importc,
#     cdecl, impapiHdr.}
# proc ts_tree_cursor_current_field_id*(a1: ptr TSTreeCursor): TSFieldId {.importc,
#     cdecl, impapiHdr.}
# proc ts_tree_cursor_goto_parent*(a1: ptr TSTreeCursor): bool {.importc, cdecl,
#     impapiHdr.}
# proc ts_tree_cursor_goto_next_sibling*(a1: ptr TSTreeCursor): bool {.importc, cdecl,
#     impapiHdr.}
# proc ts_tree_cursor_goto_first_child*(a1: ptr TSTreeCursor): bool {.importc, cdecl,
#     impapiHdr.}
# proc ts_tree_cursor_goto_first_child_for_byte*(a1: ptr TSTreeCursor; a2: uint32): int64 {.
#     cproc.}
# proc ts_tree_cursor_copy*(a1: ptr TSTreeCursor): TSTreeCursor {.importc, cdecl,
#     impapiHdr.}
# proc ts_query_new*(language: ptr TSLanguage; source: cstring; source_len: uint32;
#                   error_offset: ptr uint32; error_type: ptr TSQueryError): ptr TSQuery {.
#     cproc.}
# proc ts_query_delete*(a1: ptr TSQuery) {.cproc.}
# proc ts_query_pattern_count*(a1: ptr TSQuery): uint32 {.cproc.}
# proc ts_query_capture_count*(a1: ptr TSQuery): uint32 {.cproc.}
# proc ts_query_string_count*(a1: ptr TSQuery): uint32 {.cproc.}
# proc ts_query_start_byte_for_pattern*(a1: ptr TSQuery; a2: uint32): uint32 {.importc,
#     cdecl, impapiHdr.}
# proc ts_query_predicates_for_pattern*(self: ptr TSQuery; pattern_index: uint32;
#                                      length: ptr uint32): ptr TSQueryPredicateStep {.
#     cproc.}
# proc ts_query_capture_name_for_id*(a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.
#     cproc.}
# proc ts_query_string_value_for_id*(a1: ptr TSQuery; id: uint32; length: ptr uint32): cstring {.
#     cproc.}
# proc ts_query_disable_capture*(a1: ptr TSQuery; a2: cstring; a3: uint32) {.importc,
#     cdecl, impapiHdr.}
# proc ts_query_cursor_new*(): ptr TSQueryCursor {.cproc.}
# proc ts_query_cursor_delete*(a1: ptr TSQueryCursor) {.cproc.}
# proc ts_query_cursor_exec*(a1: ptr TSQueryCursor; a2: ptr TSQuery; a3: TSNode) {.importc,
#     cdecl, impapiHdr.}
# proc ts_query_cursor_set_byte_range*(a1: ptr TSQueryCursor; a2: uint32; a3: uint32) {.
#     cproc.}
# proc ts_query_cursor_set_point_range*(a1: ptr TSQueryCursor; a2: TSPoint; a3: TSPoint) {.
#     cproc.}
# proc ts_query_cursor_next_match*(a1: ptr TSQueryCursor; match: ptr TSQueryMatch): bool {.
#     cproc.}
# proc ts_query_cursor_remove_match*(a1: ptr TSQueryCursor; id: uint32) {.importc, cdecl,
#     impapiHdr.}
# proc ts_query_cursor_next_capture*(a1: ptr TSQueryCursor; match: ptr TSQueryMatch;
#                                   capture_index: ptr uint32): bool {.importc, cdecl,
#     impapiHdr.}
# proc ts_language_symbol_count*(a1: ptr TSLanguage): uint32 {.cproc.}
# proc ts_language_symbol_name*(a1: ptr TSLanguage; a2: TSSymbol): cstring {.importc,
#     cdecl, impapiHdr.}
# proc ts_language_symbol_for_name*(self: ptr TSLanguage; string: cstring;
#                                  length: uint32; is_named: bool): TSSymbol {.importc,
#     cdecl, impapiHdr.}
# proc ts_language_field_count*(a1: ptr TSLanguage): uint32 {.cproc.}
# proc ts_language_field_name_for_id*(a1: ptr TSLanguage; a2: TSFieldId): cstring {.
#     cproc.}
# proc ts_language_field_id_for_name*(a1: ptr TSLanguage; a2: cstring; a3: uint32): TSFieldId {.
#     cproc.}
# proc ts_language_symbol_type*(a1: ptr TSLanguage; a2: TSSymbol): TSSymbolType {.
#     cproc.}
# proc ts_language_version*(a1: ptr TSLanguage): uint32 {.cproc.}
# {.pop.}
