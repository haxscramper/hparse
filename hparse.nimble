# Package

version       = "0.2.3"
author        = "haxscramper"
description   = "Text parsing utilities"
license       = "Apache-2.0"
srcDir        = "src"
# bin = @["src/hparse/htreesitter/hts_wrapgen.nim"]



# Dependencies

requires "nim >= 1.2.4"
requires "regex"
requires "hmisc >= 0.6.0", "hasts", "hpprint"


let
  testDir = "/tmp/docker-hparse"
  localDevel = @["hmisc", "hasts", "hnimast", "hdrawing", "hpprint"]

template canImport(x: untyped): untyped =
  compiles:
    import x


when canImport(hmisc/other/nimbleutils):
  import hmisc/other/nimbleutils
  startColorLogger()

  task dockertestDevel, "Test in docker container with local packages":
    runDockerTestDevel(
      AbsDir thisDir(),
      AbsDir testDir, localDevel, "nimble test") do:
        writeTestConfig("""
          --verbosity:0
          --hints:off
          --warnings:off
        """)

    rmDir testDir


  task dockertestAllDevel, "Test in docker container with local packages":
    runDockerTestDevel(
      AbsDir thisDir(),
      AbsDir testDir, localDevel, "nimble testallTask") do:
        writeTestConfig("""
          --verbosity:0
          --hints:off
          --warnings:off
        """)

  task dockertest, "Test in new docker container":
    ## Run unit tests in new docker conatiner; download all
    ## dependencies using nimble.
    runDockerTest(AbsDir thisDir(), AbsDir testDir, "nimble test") do:
      notice "Running test in docker container"

  task installtest, "Test installation from cloned repo":
    runDockerTest(AbsDir thisDir(), AbsDir testDir, "nimble install")

  task testall, "Run full test suite in all variations":
    runDockerTest(
      AbsDir thisDir(), AbsDir testDir,
      "nimble install -n hmisc@#head && nimble testallTask")

  task testallTask, "~~~ testall implementation ~~~":
    testAllImpl()
