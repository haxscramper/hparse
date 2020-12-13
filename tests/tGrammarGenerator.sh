#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"

set -o nounset
set -o errexit

nim c \
    --warnings:off \
    --hints:off \
    -o:hts_wrapgen \
    ../src/hparse/htreesitter/hts_wrapgen.nim

cd assets

../hts_wrapgen \
    grammar \
    --langPrefix=otherTest \
    --grammarJs=nimGrammar.js \
    --parserUser=nimGrammar.nim

