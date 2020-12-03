version       = "0.2.3"
author        = "haxscramper"
description   = "Text parsing utilities"
license       = "Apache-2.0"
srcDir        = "src"

requires "nim >= 1.2.4"
requires "regex"
requires "hmisc >= 0.6.0", "hasts", "hpprint"

task docgen, "Generate documentation":
  exec("hmisc-putils docgen")

