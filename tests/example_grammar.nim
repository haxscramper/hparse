import hparse/[parse_tree, token]
import sequtils

type
  TokenKind = enum
    tkPunct
    tkIdent

  LTok = Token[TokenKind, string, LexInfo]
  TPatt = Patt[TokenKind, string]
  PTree = ParseTree[TokenKind, string, LexInfo]

func `$`(tok: LTok): string = tok.lex
func `==`(lhs, rhs: Token): bool =
  lhs.kind == rhs.kind and (
    case lhs.kind:
      of tkIdent:
        lhs.strVal == rhs.strVal
      else:
        true
  )

func mapString(s: string): seq[LTok] =
  s.mapIt(
    case it:
      of '[', ']', ',':
        makeToken[TokenKind, string, LexInfo](tkPunct, $it)
      else:
        makeToken[TokenKind, string, LexInfo](tkIdent, $it)
  )
