#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
set -o nounset
set -o errexit

nim c -o:hts_wrapgen ../src/hparse/htreesitter/hts_wrapgen.nim

cd assets

../hts_wrapgen \
    grammar \
    --langPrefix=otherTest \
    --grammarJs=nimGrammar.js \
    --parserUser=nimGrammar.nim

