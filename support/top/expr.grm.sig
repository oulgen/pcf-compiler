signature Exp_TOKENS =
sig
type ('a,'b) token
type svalue
val MATCH:  'a * 'a -> (svalue,'a) token
val CASE:  'a * 'a -> (svalue,'a) token
val INR:  'a * 'a -> (svalue,'a) token
val INL:  'a * 'a -> (svalue,'a) token
val ABORT:  'a * 'a -> (svalue,'a) token
val DOTR:  'a * 'a -> (svalue,'a) token
val DOTL:  'a * 'a -> (svalue,'a) token
val FIX:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val LAM:  'a * 'a -> (svalue,'a) token
val IFZ:  'a * 'a -> (svalue,'a) token
val NUM: (int) *  'a * 'a -> (svalue,'a) token
val SUCC:  'a * 'a -> (svalue,'a) token
val ZERO:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val VOID:  'a * 'a -> (svalue,'a) token
val TIMES:  'a * 'a -> (svalue,'a) token
val UNIT:  'a * 'a -> (svalue,'a) token
val NAT:  'a * 'a -> (svalue,'a) token
val WILD:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val IS:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val VAL:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val EQUALS:  'a * 'a -> (svalue,'a) token
val DARROW:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val RANGLE:  'a * 'a -> (svalue,'a) token
val LANGLE:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RBRACK:  'a * 'a -> (svalue,'a) token
val LBRACK:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val SEMI:  'a * 'a -> (svalue,'a) token
val IDENT: (string) *  'a * 'a -> (svalue,'a) token
val LOAD:  'a * 'a -> (svalue,'a) token
val STEP:  'a * 'a -> (svalue,'a) token
val EVAL:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Exp_LRVALS=
sig
structure Tokens : Exp_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
