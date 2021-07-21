// Tree-sitter grammar to parse list of simple arithmetic expressions

module.exports = grammar({
    name: "simple_1",
    rules: {
        main: $ => seq(
            optional(field("optional_field1", $.ident11)),
            optional(field("optional_field2", $.ident12)),
            field("non_optional_field", $.ident2),
            $.ident3
        ),
        ident11: $ => "ident11",
        ident12: $ => "ident12",
        ident2: $ => "ident2",
        ident3: $ => "ident3"
    }
});
