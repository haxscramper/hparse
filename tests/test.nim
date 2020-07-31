type
  P = object
    name: string
    subn: seq[P]

proc conv(a: varargs[(string, P)]): P =
  for (name, subn) in a:
    result.subn.add P(name: name, subn: @[subn])

const p = {
  "ae" : P(),
  "AE" : P()
}

static:
  echo p.conv()

echo p.conv()
