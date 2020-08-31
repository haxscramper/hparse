import parseutils, strutils, macros, strscans, sequtils, macroutils, sugar

import strscans
export strscans

# IDEA add scanf-based pattern matching.

func matcherTypeGetter_Impl*[T](
  arg: proc(s: string, arg: var T, start: int): int): T =
    discard

func matcherTypeGetter_Impl*[T, T1](
  arg: proc(s: string, arg: var T, start: int, a1: T1): int): T =
    discard

func matcherTypeGetter_Impl*[T, T1, T2](
  arg: proc(s: string, arg: var T, start: int, a1: T1, a2: T2): int): T =
    discard

func matcherTypeGetter_Impl*[T, T1, T2, T3](
  arg: proc(s: string, arg: var T, start: int, a1: T1, a2: T2, a3: T3): int
                               ): T =
    discard

func anything*(input: string, argument: var string, start: int): int =
  let diff = input.len - start
  argument = input[start..^1]
  return diff


macro tscanf*(input, pattNode: string): untyped =
  ## Statically typed `scanf` wrapper. Similar to `=~` template from
  ## `re` module. The `ml` variable is implicitly declared.
  ##
  ## `ml` is a tuple. Types are inferred from pattern. User
  ## defined matchers are supported too.
  # TODO DOC types of injected variables
  runnableExamples:
    proc matcher1(s: string, arg: var seq[string], start: int): int =
      arg = @["##", "$$"]
      return s.len - start

    if tscanf("12x12---%1,1,1,1", "$ix$i$+%${matcher1}"):
      echo ml[0] / 2, " = ", ml[2]," ", ml[3]

      assert declared(ml)
      assert type(ml[3]) is seq[string]
    else:
      assert not declared(ml)
      echo "does not match"

    assert not declared(ml)


  var matchers: seq[(InterpolatedKind, string)]
  var p = 0
  let pattern: string = $pattNode.toStrLit()
  while p < pattern.len:
    if pattern[p] == '$':
      inc p
      case pattern[p]
        of '$': discard
        of 'w', 'b', 'o', 'i', 'h', 'f', '+', '*':
          matchers.add (ikVar, $pattern[p])
          inc p
        of '{':
          inc p
          var nesting = 0
          let start = p
          while true:
            case pattern[p]
              of '{': inc nesting
              of '}':
                if nesting == 0: break
                dec nesting
              of '\0': error("expected closing '}'")
              else: discard
            inc p
          let expr = pattern.substr(start, p-1)
          matchers.add (ikExpr, expr)
        else:
          inc p
    else:
      inc p

  let ml: NimNode =
    block:
      var tupleType: seq[NimNode]
      for idx, str in matchers:
        if str[0] == ikVar:
          # echo str
          tupleType.add Ident(
            case str[1][0]:
              of 'i', 'o', 'b', 'h': "int"
              of 'f': "float"
              of '*', '+': "string"
              else: "float"
          )
        else:
          tupleType.add Call(
            "type", Call("matcherTypeGetter_Impl", ident str[1]))

      tupleType.add ident("int")
          # echo getType(Call("matcherTypeGetter_Impl", ident str[1]))


      superQuote do:
        var ml {.inject.}: `Par(tupleType)`

  var call = Call(
    "scanf",
    @[input, pattNode] &
    (block:
      collect(newSeq):
        for idx, _ in matchers:
          BracketExpr(ident "ml", newIntLitNode(idx)))
  )

  result = quote do:
    `ml`
    `call`

  # echo result.toStrLit()


template mapIt2D*(s: typed, expr: untyped): untyped =
  var it {.inject.}: type(s[0][0])
  var res1: seq[seq[type(expr)]]
  for val in s:
    var buf: seq[type(expr)]
    for tmp in val:
      it = tmp
      buf.add expr

    res1.add buf
  res1

# echo (@[@[12, 12], @[124, 12]]).mapIt2D(it * 7)
