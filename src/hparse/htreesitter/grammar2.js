module.exports = grammar({
    name: 'otherTest',
    externals: $ => [
        $.externalToken,
    ],

    rules: {
        source_file: $ => seq(
            "(",
            repeat($.externalToken),
            ")"
        )
    }
});
