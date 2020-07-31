type
  NTermSym* = string

type
  TreeAct* = enum
    ## Types of tree actions
    taDefault ## No tree action specified

    taDrop ## Drop element from tree
    taPromote ## Promote - make current element into topmost
    taSubrule ## Move section into separate tree
    taSpliceDiscard ## Lift node's children, discard node itself
    taSplicePromote ## Splice, replace current node with spliced one

  PattKind* = enum
    pkTerm ## Terminal token
    pkNterm ## Nonterminal symbol

    # 'nested' patterns
    pkAlternative ## Any of several (non)terminals. `OR` for (non)terminals
    pkConcat ## All (non)terminals in sequence `AND` for (non)terminals

    pkOptional ## Optional (non)terminal
    pkZeroOrMore ## Zero or more occurencies of (non)terminal
    pkOneOrMore ## One or more occurence of (non)terminal
