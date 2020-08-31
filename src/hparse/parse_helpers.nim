import sequtils
import hmisc/algo/[halgorithm, hseq_mapping]

import sets, hashes, sugar


#==========================  Topological sort  ===========================#

import hashes, sequtils, algorithm, sets

type
  LogicError = ref object of CatchableError



func topoSort*[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  idgen: proc(vert: Vertex): Hash,
  revese: bool = true): seq[Vertex] =
  ## ..code-block::
  ##     assert @[3, 2, 1] == topoSort(
  ##       verts = @[1, 2, 3],
  ##       deps = proc(v: int): seq[Hash] =
  ##                  case v:
  ##                    of 1: @[Hash(2), Hash(3)]
  ##                    of 2: @[Hash(3)]
  ##                    else: @[]
  ##     )

  mixin items

  var adjList: Table[Hash, HashSet[Hash]]
  var vertData: Table[Hash, seq[Vertex]]
  var inCounts: Table[Hash, int]

  for vert in verts:
    let depsList = deps(vert)
    let vHash = idgen(vert)
    # debugecho "Deps for vert ", $vert, ": ", depsList
    # debugecho "Id: ", vHash, "\n-----"

    adjList[vHash] = depsList.toHashSet()
    vertData.mgetOrPut(vHash, @[]).add vert


    for dep in depsList:
      # For each dependency - increase number of items that depend on this one
      inc inCounts.mgetOrPut(dep, 0)

  let counts: seq[(Hash, int)] = adjList.mapPairs(
    (lhs in inCounts).tern((lhs, inCounts[lhs]), (lhs, 0))
  ).filterIt(it[1] == 0)

  assert counts.len > 0,
      "Graph contains no vertices that have zero in-edges"

  var noincoming: seq[Hash] = counts.mapIt(it[0])
  var sortednodes: seq[Hash]

  while noincoming.len > 0:
    let node = noincoming.pop()
    sortednodes.add node
    # For all adjacent
    for adj in items(adjList[node]):
      # Remove edge `(adj, node)`
      adjList[node].excl adj
      # Decrease number of incoming edges for `adj`
      dec inCounts[adj]


      # If has no incoming
      if inCounts[adj] == 0:
        noincoming.add adj

  for vert, adj in adjList:
    if adj.len > 0:
      raiseAssert(msgjoin(
        "Cannot perform topological sort on graph with cycles",
        adj
      ))

  if revese:
    return sortednodes.reversed().mapIt(vertData[it]).concat()
  else:
    return sortednodes.mapIt(vertData[it]).concat()


func topoSort*[Vertex](
  verts: openarray[Vertex],
  deps: proc(vert: Vertex): seq[Hash],
  revese: bool = true): seq[Vertex] =
  topoSort(verts, deps, reverse, (r) => hash(r))
