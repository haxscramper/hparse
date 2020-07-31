import macros
import options

type
  Parser[Val, Buf] = proc(buf: Buf): Val

when false:
  proc parseStr(str: string): Parser[string, char] = discard
  var parser: Parser[string, char]


  proc genericImpl[T](arg: T) =
    echo "using generic"

  proc genericImpl[T](arg: seq[T]) =
    echo "Using less generic"

  proc genericImpl(arg: string) =
    echo "using speciialiation"

  genericImpl("hello")
  genericImpl(@["Hello"])
  genericImpl(12)

  # macro makeBufType(t: untyped): untyped =
  #   echo t.toStrLit()
  #   # let res =
  #   #   if t is string:
  #   #     typeof(string)
  #   #   else:
  #   #     typeof(seq[t])

  # echo typeof(makeBufType(string))

  import options

  proc t[T](arg: Option[tuple[field1: T, field2: int]]) = discard

  t(some((1, 2)))

when false:
  iterator pairs(a: int): (int, int) = discard
  iterator items(a: int): int = discard

  proc nthType1[T1, T2](a: (T1, T2)): T1 = discard
  proc nthType2[T1, T2](a: (T1, T2)): T2 = discard

  type T1 = type((pairs(12).nthType1))
  type T2 = type((items(2)))
  type T3 = type(((12, "float")[1]))


  iterator pairs*[T1, T2](s: openarray[(T1, T2)]): (T1, T2) =
    for item in s:
      yield item

  echo typeof(pairs([(1, "12")]).nthType2)

  let inp = {12 : "float"}

  echo (inp is array) or (inp is seq)

  # echo typeof(inp[0][0])
  # echo typeof(inp[0][1])


when false:

  func assrt() = assert false

  assrt()

  func test() = raise newException(IOError, "12")

  test()

#=================================  v1  ==================================#
if false:
  proc test(pr: static proc(p: int): string = nil) =
    when pr == nil:
      let impl = proc(p: int): string = "default"
    else:
      let impl = pr

    echo "res: ", impl(12)

  test(proc(b: int): string = "hhh")


#=================================  v2  ==================================#
import hashes

if false:
  proc test(pr: static proc(p: int): Hash = nil) =
    when pr == nil:
      let impl = proc(p: int): Hash = discard
    else:
      let impl = pr

    echo "res: ", impl(12)

  test(proc(b: int): Hash = discard)


#=================================  v3  ==================================#
import tables, sets
if false:
  proc test(
    vt: openarray[string],
    dp: proc(v: string): seq[Hash],
    pr: static proc(p: string): Hash =
                   (proc(p: string): Hash = hash(p))) =

    var t: Table[Hash, HashSet[Hash]]

  test(
    vt = ["12"],
    dp = proc(v: string): seq[Hash] = @[hash(v)]
  )

#=================================  v4  ==================================#

if false:
  proc test[T](
    vt: openarray[T],
    dp: proc(v: T): seq[Hash],
    pr: proc(p: T): Hash = (proc(p: T): Hash = hash(p))) =

    var t: Table[Hash, HashSet[Hash]]

  test(
    ["12"],
    proc(v: string): seq[Hash] = @[hash(v)],
    proc(b: string): Hash = hash(b)
  )

  test(
    ["12"],
    proc(v: string): seq[Hash] = @[hash(v)]
  )

#========================  recursive case object  ========================#

dumpTree:
  (cnt < aaa)
  true

iterator testIter(test: seq[int]): int {.closure.} =
  for item in test:
    yield item


#==================  generate comments from macro code  ==================#
dumpTree:
  block: ## Comment
    discard

static:
  let t = quote do:
    block: ## Comment string
      discard

  echo t.toStrLit()
  echo t.astGenRepr()

  echo toStrLit newStmtList(
    newCommentStmtNode("Hello world"),
    quote do:
      block:
        "Nice weather?"
  )

dumpTree:
  @[a1, a2, a3]
