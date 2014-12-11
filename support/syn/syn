structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

exception Illegal_character of pos

(*
"true"   => (Tokens.TRUE(!pos,!pos));
"false"  => (Tokens.FALSE(!pos,!pos));
"not"    => (Tokens.NOT(!pos,!pos));
*)

%%
%header (functor Exp_LexFun(structure Tokens: Exp_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
any = [@a-zA-Z0-9];

ws = [\ \t];

%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM( let val SOME(n) = (Int.fromString yytext) in n end,!pos,!pos));
"eval"   => (Tokens.EVAL(!pos,!pos));
"step"   => (Tokens.STEP(!pos,!pos));
"load"   => (Tokens.LOAD(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));

"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"["      => (Tokens.LBRACK(!pos,!pos));
"]"      => (Tokens.RBRACK(!pos,!pos));
"{"      => (Tokens.LBRACE(!pos,!pos));
"}"      => (Tokens.RBRACE(!pos,!pos));
"<"      => (Tokens.LANGLE(!pos,!pos));
">"      => (Tokens.RANGLE(!pos,!pos));

"->"     => (Tokens.ARROW(!pos,!pos));
"=>"     => (Tokens.DARROW(!pos,!pos));
"="      => (Tokens.EQUALS(!pos,!pos));
"|"      => (Tokens.BAR(!pos,!pos));
":"      => (Tokens.COLON(!pos,!pos));
"val"    => (Tokens.VAL(!pos,!pos));
"in"     => (Tokens.IN(!pos,!pos));
"is"     => (Tokens.IS(!pos,!pos));
"end"    => (Tokens.END(!pos,!pos)); 
","      => (Tokens.COMMA(!pos,!pos));
"_"      => (Tokens.WILD(!pos,!pos)); 

"nat"    => (Tokens.NAT(!pos,!pos));
"unit"   => (Tokens.UNIT(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
"void"   => (Tokens.VOID(!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));

"z"      => (Tokens.ZERO(!pos,!pos));
"s"      => (Tokens.SUCC(!pos,!pos));
"ifz"    => (Tokens.IFZ(!pos,!pos));
"fn"     => (Tokens.LAM(!pos,!pos));
"lam"    => (Tokens.LAM(!pos,!pos));
"\\"     => (Tokens.LAM(!pos,!pos));
"let"    => (Tokens.LET(!pos,!pos));
"fix"    => (Tokens.FIX(!pos,!pos));

".l"     => (Tokens.DOTL(!pos,!pos));
".r"     => (Tokens.DOTR(!pos,!pos));
"abort"  => (Tokens.ABORT(!pos,!pos));
"inl"    => (Tokens.INL(!pos,!pos));
"inr"    => (Tokens.INR(!pos,!pos));
"case"   => (Tokens.CASE(!pos,!pos));
"match"  => (Tokens.MATCH(!pos,!pos));

{alpha}{any}* => (Tokens.IDENT(yytext,!pos,!pos));
