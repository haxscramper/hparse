import macros, tables, sets, typetraits

type
  NimType = object
    head*: string
    genParams*: seq[NimType]

func toNimNode(ntype: NimType): NimNode =
  if ntype.genParams.len == 0:
    return ident(ntype.head)
  else:
    result = nnkBracketExpr.newTree(newIdentNode(ntype.head))
    for param in ntype.genParams:
      result.add param.toNimNode()

func toNimTypeAst*[T](): NimType =
  let str = $typeof(T)
  let expr = parseExpr(str)

func makeInitCalls*[T](val: T): NimNode =
  when T is enum:
    ident($val)
  else:
    newLit(val)

func makeInitCalls*[A, B](table: Table[A, B]): NimNode =
  mixin makeInitCalls
  result = nnkTableConstr.newTree()
  for key, val in table:
    result.add newColonExpr(key.makeInitCalls, val.makeInitCalls)

  result = newCall(
    nnkBracketExpr.newTree(
      ident("toTable"),
      parseExpr($typeof(A)),
      parseExpr($typeof(B))
    ),
    result
  )

func makeInitCalls*[A](hset: HashSet[A]): NimNode =
  mixin makeInitCalls
  result = nnkBracket.newTree()
  for val in hset:
    result.add val.makeInitCalls()

  result = newCall("toHashSet", result)
