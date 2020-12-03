module.exports = grammar({
    name: 'test',
    externals: $ => [
        $.nimExternal,
    ],

    rules: {
        // TODO: add the actual grammar rules
        source_file: $ => repeat($.nimExternal)
    }
});
