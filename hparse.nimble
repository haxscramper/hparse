version       = "0.2.6"
author        = "haxscramper"
description   = "Text parsing utilities"
license       = "Apache-2.0"
srcDir        = "src"
packageName   = "hparse"
bin           = @["hparse/htreesitter/hts_wrapgen"]
installExt    = @["nim"]
binDir        = "bin"
namedBin      = {
  "hparse/htreesitter/hts_wrapgen" : "hts-wrapgen"
}.toTable()

requires "nim >= 1.4.0"
requires "regex"
requires "hmisc >= 0.11.17", "hpprint"
requires "hnimast >= 0.3.35"

task docgen, "Generate documentation":
  exec("hmisc-putils docgen")

before test:
  try:
    exec("sh ./tests/tGrammarGenerator.sh")
  except:
    echo "\e[31mGrammar generator run failed\e[39m"

task dockertest, "Run test in docker container":
  exec("""
hmisc-putils \
  dockertest \
  --projectDir:$(pwd) \
  -lcligen \
  -lhmisc \
  -lhasts \
  -lhdrawing \
  -lhdrawing \
  -lregex \
  -lhnimast \
  -lhpprint \
  -lunicodeplus \
  --preTestCmds='export NPM_PACKAGES=$HOME/.npm-packages' \
  --preTestCmds='npm config set prefix $NPM_PACKAGES' \
  --preTestCmds='npm install tree-sitter-cli -g' \
  --preTestCmds='export PATH="$PATH:$NPM_PACKAGES/bin"' \
  --preTestCmds='cd /tmp' \
  --preTestCmds='wget https://github.com/tree-sitter/tree-sitter/archive/0.17.3.tar.gz' \
  --preTestCmds='tar -xvf 0.17.3.tar.gz && cd tree-sitter-0.17.3' \
  --preTestCmds='whereis tree-sitter' \
  --preTestCmds='sudo make install' \
  --preTestCmds='sh'
""")

#   --preTestCmds='echo $PATH' \
# --preTestCmds='/home/docker-user/.npm-packages/bin/tree-sitter' \
# --preTestCmds='tree-sitter --help' \
