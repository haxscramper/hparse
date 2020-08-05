import sugar, strutils, sequtils, strformat
import ../tscanf
import hasts/latex_ast
import hmisc/hexceptions

## Parse latex log output


#===========================  implementation  ============================#


type
  LogChunkKind* = enum
    lckError

  LogChunk* = object
    # srcline*: string
    case kind*: LogChunkKind
      of lckError:
        errpos*: int
        errmsg*: string

func parseLog*(log: string): seq[LogChunk] =
  let outlines = log.split("\n")
  var idx = 0
  while idx < outlines.len():
    let line = outlines[idx]
    if line[0] == '!':
      if tscanf(outlines[idx + 1], "l.$i \\$*"):
        result.add LogChunk(
          kind: lckError,
          errpos: ml[0],
          errmsg: ml[1],
          # srcline: outlines[idx + 1]
        )

      idx += 2

    else:
      debugecho line
      inc idx

# func annotateErrs*(chunks: seq[LogChunk]): seq[CodeError] =
#   for ch in chunks:
#     case ch.kind:
#       of lckError:
#         result.add CodeError(
#         )


#================================  tests  ================================#

import unittest

when isMainModule:
  let res = makeDocument(@[
    makeMacroCall("HHHH")
  ]).compileToPdf()

  echo res.log.parseLog()

