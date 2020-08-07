import bnf_grammars, parse_primitives, token, parse_tree, grammars
import deques
import hmisc/helpers

type
  AltId* = int
  FirstTable*[C, L] = Table[BnfNterm, Table[AltId, TokSet[C, L]]]
  FollowTable*[C, L] = Table[BnfNterm, TokSet[C, L]]

func `|=`*(a: var bool, b: bool): void = a = a or b

func isNullable*[C, L](
  fbnf: FlatBnf[C, L],
  nulls: Table[BnfNterm, seq[AltId]]): bool =
  if fbnf.isTerm:
    false
  else:
    fbnf.nterm in nulls



func getSets*[C, L](grammar: BnfGrammar[C, L]): tuple[
  first: FirstTable[C, L],
  follow: FollowTable[C, L],
  nullable: Table[BnfNterm, seq[AltId]]] =

  for head, alts in grammar.rules:
    for altId, altBody in alts:
      # Generate empty `FIRST/FOLLOW` sets - each rule+alternative pair
      # corresponds to empty set.
      if head notin result.first:
        result.first[head] = {altId : makeTokSet[C, L]()}.toTable()
      else:
        result.first[head][altId] = makeTokSet[C, L]()
      # result.follow[rule.head] = {AltId(rule.alt) : makeTokSet[C, L]()}.toTable()
      result.follow[head] = makeTokSet[C, L]()

  block: # Add end token to `FOLLOW`
    var endNterms = initDeque[BnfNterm]()
    endNterms.addLast grammar.start
    var cnt = 0
    while cnt < 10 and endNterms.len > 0:
      inc cnt
      let nterm = endNterms.popFirst()
      result.follow[nterm] = makeTokSet[C, L](eofTok)
      for altId, alt in grammar.rules[nterm]:
        # echov alt.exprRepr()
        if (alt.elems.len > 0) and (not alt.elems.last.isTerm):
          if alt.elems.last.nterm != nterm: # HACK prevent infinite loop on right recursive rules
            endNterms.addLast alt.elems.last.nterm

      # for shit in items(endnterms):
      #   echov shit

  while true:
    var updated: bool = false

    for rule, body in grammar.iterrules():
      # showLog fmt "Processing {rule.exprRepr()} {body.exprRepr()}"
      # runIndentedLog:
      block: # `FIRST` set construction
        # Iterate over all rules in grammar
        if body.isEmpty():
          if rule.head notin result.nullable:
            result.nullable[rule.head] = @[ rule.alt ] # Remember index of nullable alternative
            updated |= true
          else:
            updated |= rule.alt notin result.nullable[rule.head]
            result.nullable[rule.head].add rule.alt
        else:
          for idx, elem in body.elems: # Iterate over all elements in `X ->
            # Y1 Y2` Store `FIRST` sets separately for each
            # alternative. Finish execution after first non-nullable
            # element is found.
            let first =
              if elem.isTerm:
                # Add token to `FIRST[X]` directly
                # showLog fmt "Found {elem.tok} @ {body.exprRepr()}[{idx}]"
                makeTokSet(elem.tok)
              else:
                # Add elements from `FIRST[Yi]` to `FIRST[X, <alt>]`.
                # Since `Yi` might have more than one alternative in
                # grammar we have to merge all possible `FIRST` sets.
                result.first[elem.nterm].mapPairs(rhs).union()


            # showLog fmt "Adding {first:>30} to FIRST of
            # {rule.head}[{rule.alt}]"
            updated |= result.first[rule.head][rule.alt].containsOrIncl(first)


            if not elem.isNullable(result.nullable):
              # showInfo fmt "Found non-nullable element
              # {elem.exprRepr()}"
              break # Found non-nullable element, finishing FIRST computation
            # else:
              # showInfo fmt "Element {elem.exprRepr()} of kind {elem.kind} is nullable"

          # showInfo fmt "Finished processing {body.exprRepr()}"

      block: # `FOLLOW` set construction
        var tailFollow: TokSet[C, L] = result.follow[rule.head] # `FOLLOW`
        # for the nonterminal we are working with - need to add this
        # as `FOLLOW` for element at the end
        for elem in body.elems.reversed(): # Iterate over all elements
          # in production in reverse order: `Y1 Y2 Y3 <- X`
          if not elem.isTerm:
            updated |= result.follow[elem.nterm].containsOrIncl(tailFollow)

          if elem.isNullable(result.nullable):
            # Continue snowballing `FOLLOW` tail - current element is
            # nullable => whatever we have accumulated in tail can
            # possibly appear in production.
            tailFollow.incl(result.first[elem.nterm].mapPairs(rhs).union())
          else:
            # Current elemen is not nullable => current tail is no
            # longer needed anc can be replaced with whatever
            # `FIRST[Yi]` contains.
            if elem.isTerm:
              tailFollow = makeTokSet(elem.tok)
            else:
              tailFollow = result.first[elem.nterm].mapPairs(rhs).union()

    if not updated:
      break
