functor ExpLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Exp_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure T = Term
structure TY = Type
structure TO = TypeOps
structure TLC = TopLevelCommands
exception Parse of string
		                 
datatype apples = Dotl | Dotr | Atomic of Term.t

fun despine (exp, []) = exp
  | despine (exp, Dotl :: spine) = despine (Lang.PrL exp, spine)
  | despine (exp, Dotr :: spine) = despine (Lang.PrR exp, spine)
  | despine (exp, Atomic arg :: spine) =
       despine (T.$$(TermOps.Ap, [exp, arg]), spine)

fun addpatvars newvars usedvars = 
   case newvars of 
      [] => []
    | (newvar :: newvars) => 
      (if List.exists (fn x => x = newvar) usedvars
       then raise Parse ("Variable '" ^ newvar ^
                         "' used more than once in pattern")
       else (ParserState.addvar newvar 
             :: addpatvars newvars (newvar :: usedvars)))

fun patwrapper (pat, vars) exp = (pat, List.foldr T.\\ exp vars)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\006\000\000\000\000\000\
\\001\000\002\000\006\000\003\000\005\000\000\000\
\\001\000\005\000\034\000\007\000\033\000\013\000\032\000\014\000\060\000\
\\031\000\031\000\032\000\030\000\034\000\029\000\035\000\028\000\
\\036\000\027\000\037\000\026\000\040\000\025\000\041\000\024\000\
\\042\000\023\000\043\000\022\000\044\000\021\000\000\000\
\\001\000\005\000\034\000\007\000\033\000\013\000\032\000\031\000\031\000\
\\032\000\030\000\034\000\029\000\035\000\028\000\036\000\027\000\
\\037\000\026\000\040\000\025\000\041\000\024\000\042\000\023\000\
\\043\000\022\000\044\000\021\000\000\000\
\\001\000\005\000\034\000\007\000\033\000\013\000\032\000\031\000\031\000\
\\034\000\029\000\036\000\027\000\043\000\022\000\044\000\021\000\000\000\
\\001\000\005\000\041\000\000\000\
\\001\000\005\000\041\000\007\000\134\000\000\000\
\\001\000\005\000\041\000\007\000\163\000\000\000\
\\001\000\005\000\041\000\007\000\168\000\000\000\
\\001\000\005\000\072\000\007\000\071\000\013\000\070\000\014\000\107\000\
\\025\000\069\000\031\000\068\000\032\000\067\000\041\000\066\000\
\\042\000\065\000\000\000\
\\001\000\005\000\072\000\007\000\071\000\013\000\070\000\025\000\069\000\
\\031\000\068\000\000\000\
\\001\000\005\000\072\000\007\000\071\000\013\000\070\000\025\000\069\000\
\\031\000\068\000\032\000\067\000\041\000\066\000\042\000\065\000\000\000\
\\001\000\005\000\082\000\000\000\
\\001\000\007\000\043\000\000\000\
\\001\000\007\000\091\000\026\000\090\000\027\000\089\000\029\000\088\000\000\000\
\\001\000\008\000\096\000\000\000\
\\001\000\008\000\127\000\000\000\
\\001\000\008\000\139\000\000\000\
\\001\000\008\000\149\000\000\000\
\\001\000\008\000\165\000\000\000\
\\001\000\008\000\175\000\000\000\
\\001\000\008\000\178\000\000\000\
\\001\000\009\000\054\000\000\000\
\\001\000\009\000\056\000\000\000\
\\001\000\010\000\120\000\000\000\
\\001\000\010\000\154\000\000\000\
\\001\000\011\000\038\000\000\000\
\\001\000\011\000\084\000\000\000\
\\001\000\012\000\101\000\018\000\100\000\000\000\
\\001\000\012\000\161\000\000\000\
\\001\000\014\000\143\000\000\000\
\\001\000\014\000\158\000\000\000\
\\001\000\016\000\102\000\000\000\
\\001\000\016\000\142\000\000\000\
\\001\000\016\000\145\000\000\000\
\\001\000\016\000\152\000\000\000\
\\001\000\016\000\169\000\000\000\
\\001\000\016\000\171\000\000\000\
\\001\000\016\000\172\000\000\000\
\\001\000\016\000\179\000\000\000\
\\001\000\016\000\180\000\000\000\
\\001\000\017\000\112\000\000\000\
\\001\000\018\000\132\000\000\000\
\\001\000\019\000\073\000\000\000\
\\001\000\019\000\110\000\000\000\
\\001\000\020\000\050\000\000\000\
\\001\000\020\000\050\000\021\000\079\000\000\000\
\\001\000\022\000\128\000\000\000\
\\001\000\023\000\081\000\000\000\
\\001\000\024\000\095\000\000\000\
\\001\000\024\000\118\000\000\000\
\\001\000\024\000\126\000\000\000\
\\001\000\031\000\122\000\000\000\
\\001\000\032\000\156\000\000\000\
\\001\000\041\000\114\000\000\000\
\\001\000\042\000\151\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\005\000\034\000\007\000\033\000\013\000\032\000\031\000\031\000\
\\032\000\030\000\034\000\029\000\035\000\028\000\036\000\027\000\
\\037\000\026\000\040\000\025\000\041\000\024\000\042\000\023\000\
\\043\000\022\000\044\000\021\000\000\000\
\\188\000\000\000\
\\189\000\005\000\034\000\007\000\033\000\013\000\032\000\031\000\031\000\
\\032\000\030\000\034\000\029\000\035\000\028\000\036\000\027\000\
\\037\000\026\000\040\000\025\000\041\000\024\000\042\000\023\000\
\\043\000\022\000\044\000\021\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\015\000\117\000\028\000\116\000\030\000\115\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\005\000\034\000\007\000\033\000\013\000\032\000\031\000\031\000\
\\034\000\029\000\036\000\027\000\038\000\047\000\039\000\046\000\
\\043\000\022\000\044\000\021\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\000\000\
\\228\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\\231\000\000\000\
\\232\000\000\000\
\\233\000\000\000\
\\234\000\000\000\
\\235\000\000\000\
\\236\000\000\000\
\\237\000\000\000\
\\238\000\000\000\
\\239\000\000\000\
\\240\000\000\000\
\\241\000\000\000\
\\242\000\000\000\
\\243\000\000\000\
\\244\000\000\000\
\\245\000\000\000\
\\246\000\000\000\
\\247\000\000\000\
\\248\000\000\000\
\\249\000\000\000\
\\250\000\000\000\
\\251\000\000\000\
\\252\000\000\000\
\\253\000\000\000\
\\254\000\000\000\
\\255\000\000\000\
\\000\001\000\000\
\\001\001\000\000\
\\002\001\000\000\
\\003\001\000\000\
\\004\001\000\000\
\\005\001\000\000\
\\006\001\000\000\
\\007\001\000\000\
\\008\001\000\000\
\"
val actionRowNumbers =
"\001\000\056\000\057\000\059\000\
\\061\000\097\000\026\000\093\000\
\\092\000\005\000\013\000\079\000\
\\078\000\085\000\077\000\045\000\
\\091\000\058\000\090\000\003\000\
\\003\000\022\000\022\000\023\000\
\\067\000\114\000\063\000\003\000\
\\004\000\089\000\002\000\003\000\
\\099\000\060\000\119\000\003\000\
\\011\000\066\000\043\000\100\000\
\\062\000\005\000\085\000\080\000\
\\085\000\085\000\046\000\048\000\
\\012\000\120\000\027\000\004\000\
\\014\000\004\000\014\000\027\000\
\\081\000\049\000\094\000\015\000\
\\028\000\130\000\032\000\010\000\
\\010\000\010\000\133\000\131\000\
\\009\000\011\000\132\000\014\000\
\\044\000\088\000\087\000\086\000\
\\115\000\003\000\113\000\118\000\
\\041\000\054\000\106\000\083\000\
\\076\000\050\000\071\000\070\000\
\\069\000\014\000\082\000\024\000\
\\052\000\003\000\096\000\123\000\
\\121\000\003\000\011\000\126\000\
\\124\000\128\000\127\000\129\000\
\\051\000\134\000\016\000\047\000\
\\014\000\116\000\003\000\042\000\
\\006\000\014\000\014\000\014\000\
\\014\000\017\000\004\000\042\000\
\\033\000\030\000\028\000\034\000\
\\011\000\136\000\003\000\018\000\
\\117\000\055\000\107\000\035\000\
\\005\000\074\000\073\000\075\000\
\\025\000\072\000\084\000\053\000\
\\003\000\095\000\122\000\125\000\
\\031\000\068\000\065\000\003\000\
\\029\000\007\000\003\000\019\000\
\\098\000\029\000\008\000\110\000\
\\135\000\064\000\101\000\108\000\
\\036\000\005\000\103\000\037\000\
\\109\000\038\000\005\000\003\000\
\\020\000\003\000\003\000\021\000\
\\105\000\039\000\102\000\112\000\
\\040\000\003\000\003\000\104\000\
\\111\000\000\000"
val gotoT =
"\
\\001\000\181\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\017\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\004\000\018\000\006\000\033\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\037\000\035\000\040\000\034\000\000\000\
\\000\000\
\\000\000\
\\005\000\038\000\022\000\037\000\000\000\
\\020\000\040\000\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\015\000\043\000\
\\016\000\042\000\024\000\008\000\027\000\007\000\035\000\006\000\
\\036\000\005\000\000\000\
\\000\000\
\\009\000\047\000\010\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\049\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\004\000\018\000\006\000\050\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\013\000\051\000\000\000\
\\013\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\055\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\016\000\056\000\
\\024\000\008\000\027\000\007\000\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\004\000\018\000\006\000\057\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\004\000\018\000\006\000\059\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\060\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\042\000\062\000\043\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\072\000\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\015\000\073\000\
\\016\000\042\000\024\000\008\000\027\000\007\000\035\000\006\000\
\\036\000\005\000\000\000\
\\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\015\000\074\000\
\\016\000\042\000\024\000\008\000\027\000\007\000\035\000\006\000\
\\036\000\005\000\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\015\000\075\000\
\\016\000\042\000\024\000\008\000\027\000\007\000\035\000\006\000\
\\036\000\005\000\000\000\
\\009\000\076\000\010\000\046\000\000\000\
\\011\000\078\000\000\000\
\\000\000\
\\000\000\
\\030\000\081\000\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\016\000\083\000\
\\024\000\008\000\027\000\007\000\035\000\006\000\036\000\005\000\000\000\
\\012\000\085\000\047\000\084\000\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\016\000\090\000\
\\024\000\008\000\027\000\007\000\035\000\006\000\036\000\005\000\000\000\
\\012\000\091\000\047\000\084\000\000\000\
\\030\000\092\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\038\000\097\000\039\000\096\000\041\000\095\000\000\000\
\\000\000\
\\000\000\
\\043\000\101\000\000\000\
\\043\000\102\000\000\000\
\\043\000\103\000\000\000\
\\000\000\
\\000\000\
\\042\000\104\000\043\000\061\000\000\000\
\\042\000\106\000\043\000\061\000\000\000\
\\000\000\
\\012\000\107\000\047\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\109\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\025\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\117\000\047\000\084\000\000\000\
\\000\000\
\\000\000\
\\028\000\119\000\000\000\
\\004\000\018\000\006\000\121\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\122\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\042\000\123\000\043\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\127\000\047\000\084\000\000\000\
\\000\000\
\\004\000\018\000\006\000\128\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\031\000\129\000\000\000\
\\005\000\131\000\000\000\
\\012\000\133\000\047\000\084\000\000\000\
\\012\000\134\000\047\000\084\000\000\000\
\\012\000\135\000\047\000\084\000\000\000\
\\012\000\136\000\047\000\084\000\000\000\
\\000\000\
\\004\000\018\000\007\000\016\000\008\000\015\000\016\000\138\000\
\\024\000\008\000\027\000\007\000\035\000\006\000\036\000\005\000\000\000\
\\031\000\139\000\000\000\
\\000\000\
\\000\000\
\\038\000\097\000\039\000\142\000\041\000\095\000\000\000\
\\000\000\
\\042\000\144\000\043\000\061\000\000\000\
\\000\000\
\\004\000\018\000\006\000\146\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\023\000\145\000\024\000\008\000\
\\027\000\007\000\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\026\000\148\000\000\000\
\\000\000\
\\000\000\
\\005\000\151\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\029\000\153\000\000\000\
\\004\000\018\000\006\000\155\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\146\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\023\000\157\000\024\000\008\000\
\\027\000\007\000\035\000\006\000\036\000\005\000\000\000\
\\032\000\158\000\000\000\
\\005\000\160\000\000\000\
\\004\000\018\000\006\000\162\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\032\000\164\000\000\000\
\\005\000\165\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\168\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\171\000\000\000\
\\004\000\018\000\006\000\172\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\004\000\018\000\006\000\174\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\004\000\018\000\006\000\175\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\018\000\006\000\179\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\004\000\018\000\006\000\180\000\007\000\016\000\008\000\015\000\
\\014\000\014\000\016\000\013\000\017\000\012\000\018\000\011\000\
\\019\000\010\000\021\000\009\000\024\000\008\000\027\000\007\000\
\\035\000\006\000\036\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 182
val numrules = 81
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID' | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | IDENT of unit ->  (string)
 | ascBase of unit ->  (TY.t) | matchpatternvar of unit ->  (T.t)
 | matchpatternlist of unit ->  (T.t)
 | matchmid of unit ->  (Var.t list)
 | atomic_pat of unit ->  (string list*Pattern.t)
 | pat of unit ->  (string list*Pattern.t)
 | matchend of unit ->  (unit)
 | matchrules of unit ->  ( ( Pattern.t * T.t )  list)
 | matchrest of unit ->  ( ( Pattern.t * T.t )  list)
 | matchsnd of unit ->  (Pattern.t*Var.t list)
 | matchfst of unit ->  (Pattern.t*Var.t list)
 | matchexp of unit ->  (T.t) | matchstart of unit ->  (T.t)
 | explist of unit ->  (T.t) | maybeasc of unit ->  (Type.t option)
 | endcase of unit ->  (unit) | midcase of unit ->  (unit)
 | stcase of unit ->  (unit) | sifz of unit ->  (Var.t*T.t)
 | zifz of unit ->  (T.t) | ifzexp of unit ->  (T.t)
 | rcase of unit ->  (Var.t*T.t) | lcase of unit ->  (Var.t*T.t)
 | caseexp of unit ->  (T.t) | lamend of unit ->  (T.t)
 | fix1 of unit ->  (T.t) | fixstart of unit ->  (unit)
 | lam1 of unit ->  (T.t) | lamstart of unit ->  (unit)
 | fixexp of unit ->  (T.t) | lamexp of unit ->  (T.t)
 | atomic_exp of unit ->  (T.t)
 | atomic_exp_list of unit ->  (apples list)
 | infix_exp of unit ->  (T.t) | sumtyp of unit ->  (TY.t*TY.t)
 | asc of unit ->  (TY.t) | letend of unit ->  (unit)
 | decl of unit ->  (Term.t*Var.t) | decls of unit ->  (T.t)
 | letstart of unit ->  (unit) | letexp of unit ->  (T.t)
 | exp of unit ->  (Term.t) | addvar of unit ->  (Var.t)
 | var of unit ->  (T.t) | step of unit ->  (TLC.cmd)
 | eval of unit ->  (TLC.cmd) | start of unit ->  (TLC.cmd)
end
type svalue = MlyValue.svalue
type result = TLC.cmd
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | (T 5) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "EVAL"
  | (T 2) => "STEP"
  | (T 3) => "LOAD"
  | (T 4) => "IDENT"
  | (T 5) => "SEMI"
  | (T 6) => "LPAREN"
  | (T 7) => "RPAREN"
  | (T 8) => "LBRACK"
  | (T 9) => "RBRACK"
  | (T 10) => "LBRACE"
  | (T 11) => "RBRACE"
  | (T 12) => "LANGLE"
  | (T 13) => "RANGLE"
  | (T 14) => "ARROW"
  | (T 15) => "DARROW"
  | (T 16) => "EQUALS"
  | (T 17) => "BAR"
  | (T 18) => "COLON"
  | (T 19) => "VAL"
  | (T 20) => "IN"
  | (T 21) => "IS"
  | (T 22) => "END"
  | (T 23) => "COMMA"
  | (T 24) => "WILD"
  | (T 25) => "NAT"
  | (T 26) => "UNIT"
  | (T 27) => "TIMES"
  | (T 28) => "VOID"
  | (T 29) => "PLUS"
  | (T 30) => "ZERO"
  | (T 31) => "SUCC"
  | (T 32) => "NUM"
  | (T 33) => "IFZ"
  | (T 34) => "LAM"
  | (T 35) => "LET"
  | (T 36) => "FIX"
  | (T 37) => "DOTL"
  | (T 38) => "DOTR"
  | (T 39) => "ABORT"
  | (T 40) => "INL"
  | (T 41) => "INR"
  | (T 42) => "CASE"
  | (T 43) => "MATCH"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID'
end
val terms : term list = nil
 $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37)
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.step step1, step1left, step1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (step
 as step1) = step1 ()
 in (step)
end)
 in ( LrTable.NT 0, ( result, step1left, step1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.eval eval1, eval1left, eval1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (eval
 as eval1) = eval1 ()
 in (eval)
end)
 in ( LrTable.NT 0, ( result, eval1left, eval1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
STEP1left, _)) :: rest671)) => let val  result = MlyValue.step (fn _
 => let val  (exp as exp1) = exp1 ()
 in (TLC.Step(SOME(exp)))
end)
 in ( LrTable.NT 2, ( result, STEP1left, exp1right), rest671)
end
|  ( 3, ( ( _, ( _, STEP1left, STEP1right)) :: rest671)) => let val  
result = MlyValue.step (fn _ => (TLC.Step(NONE)))
 in ( LrTable.NT 2, ( result, STEP1left, STEP1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
EVAL1left, _)) :: rest671)) => let val  result = MlyValue.eval (fn _
 => let val  (exp as exp1) = exp1 ()
 in (TLC.Eval(SOME(exp)))
end)
 in ( LrTable.NT 1, ( result, EVAL1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( _, EVAL1left, EVAL1right)) :: rest671)) => let val  
result = MlyValue.eval (fn _ => (TLC.Eval(NONE)))
 in ( LrTable.NT 1, ( result, EVAL1left, EVAL1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.lam1 lam11, _, lam11right)) :: ( _, ( 
MlyValue.lamstart lamstart1, lamstart1left, _)) :: rest671)) => let
 val  result = MlyValue.lamexp (fn _ => let val  lamstart1 = lamstart1
 ()
 val  (lam1 as lam11) = lam11 ()
 in (lam1)
end)
 in ( LrTable.NT 16, ( result, lamstart1left, lam11right), rest671)

end
|  ( 7, ( ( _, ( _, LAM1left, LAM1right)) :: rest671)) => let val  
result = MlyValue.lamstart (fn _ => (ParserState.savetable()))
 in ( LrTable.NT 18, ( result, LAM1left, LAM1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.lamend lamend1, _, lamend1right)) :: _ :: (
 _, ( MlyValue.asc asc1, _, _)) :: _ :: ( _, ( MlyValue.addvar addvar1
, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.lam1 (fn _ => let val  (addvar as addvar1) = addvar1 ()
 val  (asc as asc1) = asc1 ()
 val  (lamend as lamend1) = lamend1 ()
 in (Lang.Lam (addvar,asc) lamend)
end)
 in ( LrTable.NT 19, ( result, LPAREN1left, lamend1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.lamend (fn _ => let val  (exp as exp1)
 = exp1 ()
 in (ParserState.restoretable(); exp)
end)
 in ( LrTable.NT 22, ( result, exp1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.fix1 fix11, _, fix11right)) :: ( _, ( 
MlyValue.fixstart fixstart1, fixstart1left, _)) :: rest671)) => let
 val  result = MlyValue.fixexp (fn _ => let val  fixstart1 = fixstart1
 ()
 val  (fix1 as fix11) = fix11 ()
 in (fix1)
end)
 in ( LrTable.NT 17, ( result, fixstart1left, fix11right), rest671)

end
|  ( 11, ( ( _, ( _, FIX1left, FIX1right)) :: rest671)) => let val  
result = MlyValue.fixstart (fn _ => (ParserState.savetable()))
 in ( LrTable.NT 20, ( result, FIX1left, FIX1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.lamend lamend1, _, lamend1right)) :: _ :: (
 _, ( MlyValue.asc asc1, _, _)) :: _ :: ( _, ( MlyValue.addvar addvar1
, addvar1left, _)) :: rest671)) => let val  result = MlyValue.fix1 (fn
 _ => let val  (addvar as addvar1) = addvar1 ()
 val  (asc as asc1) = asc1 ()
 val  (lamend as lamend1) = lamend1 ()
 in (Lang.Fix (addvar,asc) lamend)
end)
 in ( LrTable.NT 21, ( result, addvar1left, lamend1right), rest671)

end
|  ( 13, ( ( _, ( _, NAT1left, NAT1right)) :: rest671)) => let val  
result = MlyValue.ascBase (fn _ => (TY.$$(TO.NAT, [])))
 in ( LrTable.NT 46, ( result, NAT1left, NAT1right), rest671)
end
|  ( 14, ( ( _, ( _, UNIT1left, UNIT1right)) :: rest671)) => let val  
result = MlyValue.ascBase (fn _ => (TY.$$(TO.UNIT,[])))
 in ( LrTable.NT 46, ( result, UNIT1left, UNIT1right), rest671)
end
|  ( 15, ( ( _, ( _, VOID1left, VOID1right)) :: rest671)) => let val  
result = MlyValue.ascBase (fn _ => (TY.$$(TO.VOID, [])))
 in ( LrTable.NT 46, ( result, VOID1left, VOID1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.asc asc1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.ascBase (fn _ => let val  (asc as asc1) = asc1 ()
 in (asc)
end)
 in ( LrTable.NT 46, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 17, ( ( _, ( MlyValue.asc asc1, _, asc1right)) :: _ :: ( _, ( 
MlyValue.ascBase ascBase1, ascBase1left, _)) :: rest671)) => let val  
result = MlyValue.asc (fn _ => let val  (ascBase as ascBase1) = 
ascBase1 ()
 val  (asc as asc1) = asc1 ()
 in (TY.$$(TO.PROD, [ascBase,asc]))
end)
 in ( LrTable.NT 11, ( result, ascBase1left, asc1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.asc asc1, _, asc1right)) :: _ :: ( _, ( 
MlyValue.ascBase ascBase1, ascBase1left, _)) :: rest671)) => let val  
result = MlyValue.asc (fn _ => let val  (ascBase as ascBase1) = 
ascBase1 ()
 val  (asc as asc1) = asc1 ()
 in (TY.$$(TO.SUM, [ascBase,asc]))
end)
 in ( LrTable.NT 11, ( result, ascBase1left, asc1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.asc asc1, _, asc1right)) :: _ :: ( _, ( 
MlyValue.ascBase ascBase1, ascBase1left, _)) :: rest671)) => let val  
result = MlyValue.asc (fn _ => let val  (ascBase as ascBase1) = 
ascBase1 ()
 val  (asc as asc1) = asc1 ()
 in (TY.$$(TO.PARR, [ascBase,asc]))
end)
 in ( LrTable.NT 11, ( result, ascBase1left, asc1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ascBase ascBase1, ascBase1left, 
ascBase1right)) :: rest671)) => let val  result = MlyValue.asc (fn _
 => let val  (ascBase as ascBase1) = ascBase1 ()
 in ((TextIO.print(""));ascBase)
end)
 in ( LrTable.NT 11, ( result, ascBase1left, ascBase1right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.infix_exp infix_exp1, infix_exp1left, 
infix_exp1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (infix_exp as infix_exp1) = infix_exp1 ()
 in (infix_exp)
end)
 in ( LrTable.NT 5, ( result, infix_exp1left, infix_exp1right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.lamexp lamexp1, lamexp1left, lamexp1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
lamexp as lamexp1) = lamexp1 ()
 in (lamexp)
end)
 in ( LrTable.NT 5, ( result, lamexp1left, lamexp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.fixexp fixexp1, fixexp1left, fixexp1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
fixexp as fixexp1) = fixexp1 ()
 in (fixexp)
end)
 in ( LrTable.NT 5, ( result, fixexp1left, fixexp1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.atomic_exp_list atomic_exp_list1, _, 
atomic_exp_list1right)) :: ( _, ( MlyValue.atomic_exp atomic_exp1, 
atomic_exp1left, _)) :: rest671)) => let val  result = 
MlyValue.infix_exp (fn _ => let val  (atomic_exp as atomic_exp1) = 
atomic_exp1 ()
 val  (atomic_exp_list as atomic_exp_list1) = atomic_exp_list1 ()
 in (despine (atomic_exp, atomic_exp_list))
end)
 in ( LrTable.NT 13, ( result, atomic_exp1left, atomic_exp_list1right)
, rest671)
end
|  ( 25, ( ( _, ( MlyValue.atomic_exp atomic_exp1, _, atomic_exp1right
)) :: ( _, ( _, SUCC1left, _)) :: rest671)) => let val  result = 
MlyValue.infix_exp (fn _ => let val  (atomic_exp as atomic_exp1) = 
atomic_exp1 ()
 in (T.$$(TermOps.S,[atomic_exp]))
end)
 in ( LrTable.NT 13, ( result, SUCC1left, atomic_exp1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.atomic_exp atomic_exp1, _, atomic_exp1right
)) :: ( _, ( MlyValue.sumtyp sumtyp1, _, _)) :: ( _, ( _, INL1left, _)
) :: rest671)) => let val  result = MlyValue.infix_exp (fn _ => let
 val  (sumtyp as sumtyp1) = sumtyp1 ()
 val  (atomic_exp as atomic_exp1) = atomic_exp1 ()
 in (Lang.InL sumtyp atomic_exp)
end)
 in ( LrTable.NT 13, ( result, INL1left, atomic_exp1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.atomic_exp atomic_exp1, _, atomic_exp1right
)) :: ( _, ( MlyValue.sumtyp sumtyp1, _, _)) :: ( _, ( _, INR1left, _)
) :: rest671)) => let val  result = MlyValue.infix_exp (fn _ => let
 val  (sumtyp as sumtyp1) = sumtyp1 ()
 val  (atomic_exp as atomic_exp1) = atomic_exp1 ()
 in (Lang.InR sumtyp atomic_exp)
end)
 in ( LrTable.NT 13, ( result, INR1left, atomic_exp1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.atomic_exp atomic_exp1, _, atomic_exp1right
)) :: _ :: ( _, ( MlyValue.asc asc1, _, _)) :: _ :: ( _, ( _, 
ABORT1left, _)) :: rest671)) => let val  result = MlyValue.infix_exp
 (fn _ => let val  (asc as asc1) = asc1 ()
 val  (atomic_exp as atomic_exp1) = atomic_exp1 ()
 in (Lang.Abort asc atomic_exp)
end)
 in ( LrTable.NT 13, ( result, ABORT1left, atomic_exp1right), rest671)

end
|  ( 29, ( rest671)) => let val  result = MlyValue.atomic_exp_list (fn
 _ => ([]))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 30, ( ( _, ( MlyValue.atomic_exp_list atomic_exp_list1, _, 
atomic_exp_list1right)) :: ( _, ( _, DOTL1left, _)) :: rest671)) =>
 let val  result = MlyValue.atomic_exp_list (fn _ => let val  (
atomic_exp_list as atomic_exp_list1) = atomic_exp_list1 ()
 in (Dotl :: atomic_exp_list)
end)
 in ( LrTable.NT 14, ( result, DOTL1left, atomic_exp_list1right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.atomic_exp_list atomic_exp_list1, _, 
atomic_exp_list1right)) :: ( _, ( _, DOTR1left, _)) :: rest671)) =>
 let val  result = MlyValue.atomic_exp_list (fn _ => let val  (
atomic_exp_list as atomic_exp_list1) = atomic_exp_list1 ()
 in (Dotr :: atomic_exp_list)
end)
 in ( LrTable.NT 14, ( result, DOTR1left, atomic_exp_list1right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.atomic_exp_list atomic_exp_list1, _, 
atomic_exp_list1right)) :: ( _, ( MlyValue.atomic_exp atomic_exp1, 
atomic_exp1left, _)) :: rest671)) => let val  result = 
MlyValue.atomic_exp_list (fn _ => let val  (atomic_exp as atomic_exp1)
 = atomic_exp1 ()
 val  (atomic_exp_list as atomic_exp_list1) = atomic_exp_list1 ()
 in (Atomic atomic_exp :: atomic_exp_list)
end)
 in ( LrTable.NT 14, ( result, atomic_exp1left, atomic_exp_list1right)
, rest671)
end
|  ( 33, ( ( _, ( _, ZERO1left, ZERO1right)) :: rest671)) => let val  
result = MlyValue.atomic_exp (fn _ => (T.$$(TermOps.Z,[])))
 in ( LrTable.NT 15, ( result, ZERO1left, ZERO1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.var var1, var1left, var1right)) :: rest671)
) => let val  result = MlyValue.atomic_exp (fn _ => let val  (var as 
var1) = var1 ()
 in (var)
end)
 in ( LrTable.NT 15, ( result, var1left, var1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.letexp letexp1, letexp1left, letexp1right))
 :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ => let
 val  (letexp as letexp1) = letexp1 ()
 in (letexp)
end)
 in ( LrTable.NT 15, ( result, letexp1left, letexp1right), rest671)

end
|  ( 36, ( ( _, ( MlyValue.caseexp caseexp1, caseexp1left, 
caseexp1right)) :: rest671)) => let val  result = MlyValue.atomic_exp
 (fn _ => let val  (caseexp as caseexp1) = caseexp1 ()
 in (caseexp)
end)
 in ( LrTable.NT 15, ( result, caseexp1left, caseexp1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.ifzexp ifzexp1, ifzexp1left, ifzexp1right))
 :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ => let
 val  (ifzexp as ifzexp1) = ifzexp1 ()
 in (ifzexp)
end)
 in ( LrTable.NT 15, ( result, ifzexp1left, ifzexp1right), rest671)

end
|  ( 38, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( _, LANGLE1left, _))
 :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ => (
T.$$(TermOps.Triv,[])))
 in ( LrTable.NT 15, ( result, LANGLE1left, RANGLE1right), rest671)

end
|  ( 39, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( MlyValue.exp exp2, _,
 _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, LANGLE1left
, _)) :: rest671)) => let val  result = MlyValue.atomic_exp (fn _ =>
 let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (T.$$(TermOps.Pair,[exp1,exp2]))
end)
 in ( LrTable.NT 15, ( result, LANGLE1left, RANGLE1right), rest671)

end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atomic_exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 15, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 41, ( ( _, ( MlyValue.matchexp matchexp1, matchexp1left, 
matchexp1right)) :: rest671)) => let val  result = MlyValue.atomic_exp
 (fn _ => let val  (matchexp as matchexp1) = matchexp1 ()
 in (matchexp)
end)
 in ( LrTable.NT 15, ( result, matchexp1left, matchexp1right), rest671
)
end
|  ( 42, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.asc asc2, _,
 _)) :: _ :: ( _, ( MlyValue.asc asc1, _, _)) :: ( _, ( _, LBRACK1left
, _)) :: rest671)) => let val  result = MlyValue.sumtyp (fn _ => let
 val  asc1 = asc1 ()
 val  asc2 = asc2 ()
 in ((asc1,asc2))
end)
 in ( LrTable.NT 12, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.var (fn _ => let val  (IDENT
 as IDENT1) = IDENT1 ()
 in (T.`` (ParserState.getvar IDENT))
end)
 in ( LrTable.NT 3, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.addvar (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 in (ParserState.addvar IDENT)
end)
 in ( LrTable.NT 4, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.endcase endcase1, _, endcase1right)) :: ( _
, ( MlyValue.rcase rcase1, _, _)) :: ( _, ( MlyValue.midcase midcase1,
 _, _)) :: ( _, ( MlyValue.lcase lcase1, _, _)) :: ( _, ( 
MlyValue.stcase stcase1, _, _)) :: ( _, ( MlyValue.exp exp1, _, _)) ::
 ( _, ( _, CASE1left, _)) :: rest671)) => let val  result = 
MlyValue.caseexp (fn _ => let val  (exp as exp1) = exp1 ()
 val  stcase1 = stcase1 ()
 val  (lcase as lcase1) = lcase1 ()
 val  midcase1 = midcase1 ()
 val  (rcase as rcase1) = rcase1 ()
 val  endcase1 = endcase1 ()
 in (Lang.Case exp lcase rcase)
end)
 in ( LrTable.NT 23, ( result, CASE1left, endcase1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.addvar addvar1, _, _)) :: _ :: ( _, ( _, INL1left, _)) :: 
rest671)) => let val  result = MlyValue.lcase (fn _ => let val  (
addvar as addvar1) = addvar1 ()
 val  (exp as exp1) = exp1 ()
 in ((addvar,exp))
end)
 in ( LrTable.NT 24, ( result, INL1left, exp1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.addvar addvar1, _, _)) :: ( _, ( _, INL1left, _)) :: rest671)
) => let val  result = MlyValue.lcase (fn _ => let val  (addvar as 
addvar1) = addvar1 ()
 val  (exp as exp1) = exp1 ()
 in ((addvar,exp))
end)
 in ( LrTable.NT 24, ( result, INL1left, exp1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.addvar addvar1, _, _)) :: _ :: ( _, ( _, INR1left, _)) :: 
rest671)) => let val  result = MlyValue.rcase (fn _ => let val  (
addvar as addvar1) = addvar1 ()
 val  (exp as exp1) = exp1 ()
 in ((addvar,exp))
end)
 in ( LrTable.NT 25, ( result, INR1left, exp1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.addvar addvar1, _, _)) :: ( _, ( _, INR1left, _)) :: rest671)
) => let val  result = MlyValue.rcase (fn _ => let val  (addvar as 
addvar1) = addvar1 ()
 val  (exp as exp1) = exp1 ()
 in ((addvar,exp))
end)
 in ( LrTable.NT 25, ( result, INR1left, exp1right), rest671)
end
|  ( 50, ( ( _, ( _, LBRACE1left, LBRACE1right)) :: rest671)) => let
 val  result = MlyValue.stcase (fn _ => (ParserState.savetable()))
 in ( LrTable.NT 29, ( result, LBRACE1left, LBRACE1right), rest671)

end
|  ( 51, ( ( _, ( _, BAR1left, BAR1right)) :: rest671)) => let val  
result = MlyValue.midcase (fn _ => (
ParserState.restoretable();
                                   ParserState.savetable()
))
 in ( LrTable.NT 30, ( result, BAR1left, BAR1right), rest671)
end
|  ( 52, ( ( _, ( _, RBRACE1left, RBRACE1right)) :: rest671)) => let
 val  result = MlyValue.endcase (fn _ => (ParserState.restoretable()))
 in ( LrTable.NT 31, ( result, RBRACE1left, RBRACE1right), rest671)

end
|  ( 53, ( ( _, ( MlyValue.endcase endcase1, _, endcase1right)) :: ( _
, ( MlyValue.sifz sifz1, _, _)) :: ( _, ( MlyValue.midcase midcase1, _
, _)) :: ( _, ( MlyValue.zifz zifz1, _, _)) :: ( _, ( MlyValue.stcase 
stcase1, _, _)) :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, 
IFZ1left, _)) :: rest671)) => let val  result = MlyValue.ifzexp (fn _
 => let val  (exp as exp1) = exp1 ()
 val  stcase1 = stcase1 ()
 val  (zifz as zifz1) = zifz1 ()
 val  midcase1 = midcase1 ()
 val  (sifz as sifz1) = sifz1 ()
 val  endcase1 = endcase1 ()
 in (Lang.Ifz exp zifz sifz)
end)
 in ( LrTable.NT 26, ( result, IFZ1left, endcase1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( _, 
ZERO1left, _)) :: rest671)) => let val  result = MlyValue.zifz (fn _
 => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 27, ( result, ZERO1left, exp1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.addvar addvar1, _, _)) :: _ :: ( _, ( _, SUCC1left, _)) :: 
rest671)) => let val  result = MlyValue.sifz (fn _ => let val  (addvar
 as addvar1) = addvar1 ()
 val  (exp as exp1) = exp1 ()
 in ((addvar,exp))
end)
 in ( LrTable.NT 28, ( result, SUCC1left, exp1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.addvar addvar1, _, _)) :: ( _, ( _, SUCC1left, _)) :: rest671
)) => let val  result = MlyValue.sifz (fn _ => let val  (addvar as 
addvar1) = addvar1 ()
 val  (exp as exp1) = exp1 ()
 in ((addvar, exp))
end)
 in ( LrTable.NT 28, ( result, SUCC1left, exp1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.letend letend1, _, letend1right)) :: ( _, (
 MlyValue.decls decls1, _, _)) :: ( _, ( MlyValue.letstart letstart1, 
letstart1left, _)) :: rest671)) => let val  result = MlyValue.letexp
 (fn _ => let val  letstart1 = letstart1 ()
 val  (decls as decls1) = decls1 ()
 val  letend1 = letend1 ()
 in (decls)
end)
 in ( LrTable.NT 6, ( result, letstart1left, letend1right), rest671)

end
|  ( 58, ( ( _, ( _, LET1left, LET1right)) :: rest671)) => let val  
result = MlyValue.letstart (fn _ => (ParserState.savetable()))
 in ( LrTable.NT 7, ( result, LET1left, LET1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.decls decls1, _, decls1right)) :: ( _, ( 
MlyValue.decl decl1, decl1left, _)) :: rest671)) => let val  result = 
MlyValue.decls (fn _ => let val  (decl as decl1) = decl1 ()
 val  (decls as decls1) = decls1 ()
 in (Lang.Let decl decls)
end)
 in ( LrTable.NT 8, ( result, decl1left, decls1right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.decl decl1, decl1left, _)) :: rest671)) => let val  result = 
MlyValue.decls (fn _ => let val  (decl as decl1) = decl1 ()
 val  (exp as exp1) = exp1 ()
 in (Lang.Let decl exp)
end)
 in ( LrTable.NT 8, ( result, decl1left, exp1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, VAL1left, _)) :: rest671))
 => let val  result = MlyValue.decl (fn _ => let val  (IDENT as IDENT1
) = IDENT1 ()
 val  (exp as exp1) = exp1 ()
 in ((exp, ParserState.addvar IDENT))
end)
 in ( LrTable.NT 9, ( result, VAL1left, exp1right), rest671)
end
|  ( 62, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.letend (fn _ => (ParserState.restoretable()))
 in ( LrTable.NT 10, ( result, END1left, END1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.matchrules matchrules1, _, matchrules1right
)) :: ( _, ( MlyValue.matchstart matchstart1, matchstart1left, _)) :: 
rest671)) => let val  result = MlyValue.matchexp (fn _ => let val  (
matchstart as matchstart1) = matchstart1 ()
 val  (matchrules as matchrules1) = matchrules1 ()
 in (Lang.Match matchstart matchrules)
end)
 in ( LrTable.NT 35, ( result, matchstart1left, matchrules1right), 
rest671)
end
|  ( 64, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
MATCH1left, _)) :: rest671)) => let val  result = MlyValue.matchstart
 (fn _ => let val  (exp as exp1) = exp1 ()
 in ((ParserState.savetable()); exp)
end)
 in ( LrTable.NT 34, ( result, MATCH1left, exp1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.matchrest matchrest1, _, matchrest1right))
 :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( MlyValue.matchfst 
matchfst1, matchfst1left, _)) :: rest671)) => let val  result = 
MlyValue.matchrules (fn _ => let val  (matchfst as matchfst1) = 
matchfst1 ()
 val  (exp as exp1) = exp1 ()
 val  (matchrest as matchrest1) = matchrest1 ()
 in (patwrapper matchfst exp :: matchrest)
end)
 in ( LrTable.NT 39, ( result, matchfst1left, matchrest1right), 
rest671)
end
|  ( 66, ( ( _, ( MlyValue.matchrest matchrest1, _, matchrest1right))
 :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( MlyValue.matchsnd 
matchsnd1, matchsnd1left, _)) :: rest671)) => let val  result = 
MlyValue.matchrest (fn _ => let val  (matchsnd as matchsnd1) = 
matchsnd1 ()
 val  (exp as exp1) = exp1 ()
 val  (matchrest as matchrest1) = matchrest1 ()
 in (patwrapper matchsnd exp :: matchrest)
end)
 in ( LrTable.NT 38, ( result, matchsnd1left, matchrest1right), 
rest671)
end
|  ( 67, ( ( _, ( MlyValue.matchend matchend1, matchend1left, 
matchend1right)) :: rest671)) => let val  result = MlyValue.matchrest
 (fn _ => let val  matchend1 = matchend1 ()
 in ([])
end)
 in ( LrTable.NT 38, ( result, matchend1left, matchend1right), rest671
)
end
|  ( 68, ( ( _, ( _, _, DARROW1right)) :: ( _, ( MlyValue.pat pat1, _,
 _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = 
MlyValue.matchfst (fn _ => let val  (pat as pat1) = pat1 ()
 in ((#2 pat, addpatvars (#1 pat) []))
end)
 in ( LrTable.NT 36, ( result, LBRACE1left, DARROW1right), rest671)

end
|  ( 69, ( ( _, ( _, _, DARROW1right)) :: ( _, ( MlyValue.pat pat1, _,
 _)) :: ( _, ( _, BAR1left, _)) :: rest671)) => let val  result = 
MlyValue.matchsnd (fn _ => let val  (pat as pat1) = pat1 ()
 in (
ParserState.restoretable();
                                   ParserState.savetable();
                                   (#2 pat, addpatvars (#1 pat) [])
)
end)
 in ( LrTable.NT 37, ( result, BAR1left, DARROW1right), rest671)
end
|  ( 70, ( ( _, ( _, RBRACE1left, RBRACE1right)) :: rest671)) => let
 val  result = MlyValue.matchend (fn _ => (ParserState.restoretable())
)
 in ( LrTable.NT 40, ( result, RBRACE1left, RBRACE1right), rest671)

end
|  ( 71, ( ( _, ( MlyValue.atomic_pat atomic_pat1, _, atomic_pat1right
)) :: ( _, ( _, INL1left, _)) :: rest671)) => let val  result = 
MlyValue.pat (fn _ => let val  (atomic_pat as atomic_pat1) = 
atomic_pat1 ()
 in (
(#1 atomic_pat, 
                                    Pattern.In (Pattern.L, #2 atomic_pat))
)
end)
 in ( LrTable.NT 41, ( result, INL1left, atomic_pat1right), rest671)

end
|  ( 72, ( ( _, ( MlyValue.atomic_pat atomic_pat1, _, atomic_pat1right
)) :: ( _, ( _, INR1left, _)) :: rest671)) => let val  result = 
MlyValue.pat (fn _ => let val  (atomic_pat as atomic_pat1) = 
atomic_pat1 ()
 in (
(#1 atomic_pat,
                                    Pattern.In (Pattern.R, #2 atomic_pat))
)
end)
 in ( LrTable.NT 41, ( result, INR1left, atomic_pat1right), rest671)

end
|  ( 73, ( ( _, ( MlyValue.atomic_pat atomic_pat1, _, atomic_pat1right
)) :: ( _, ( _, SUCC1left, _)) :: rest671)) => let val  result = 
MlyValue.pat (fn _ => let val  (atomic_pat as atomic_pat1) = 
atomic_pat1 ()
 in (
(#1 atomic_pat,
                                    Pattern.S (#2 atomic_pat))
)
end)
 in ( LrTable.NT 41, ( result, SUCC1left, atomic_pat1right), rest671)

end
|  ( 74, ( ( _, ( MlyValue.atomic_pat atomic_pat1, atomic_pat1left, 
atomic_pat1right)) :: rest671)) => let val  result = MlyValue.pat (fn
 _ => let val  (atomic_pat as atomic_pat1) = atomic_pat1 ()
 in (atomic_pat)
end)
 in ( LrTable.NT 41, ( result, atomic_pat1left, atomic_pat1right), 
rest671)
end
|  ( 75, ( ( _, ( _, WILD1left, WILD1right)) :: rest671)) => let val  
result = MlyValue.atomic_pat (fn _ => ([], Pattern.Wild))
 in ( LrTable.NT 42, ( result, WILD1left, WILD1right), rest671)
end
|  ( 76, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.atomic_pat (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 in ([IDENT], Pattern.Var)
end)
 in ( LrTable.NT 42, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 77, ( ( _, ( _, ZERO1left, ZERO1right)) :: rest671)) => let val  
result = MlyValue.atomic_pat (fn _ => ([], Pattern.Z))
 in ( LrTable.NT 42, ( result, ZERO1left, ZERO1right), rest671)
end
|  ( 78, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( _, LANGLE1left, _))
 :: rest671)) => let val  result = MlyValue.atomic_pat (fn _ => (
[], Pattern.Triv))
 in ( LrTable.NT 42, ( result, LANGLE1left, RANGLE1right), rest671)

end
|  ( 79, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( MlyValue.pat pat2, _,
 _)) :: _ :: ( _, ( MlyValue.pat pat1, _, _)) :: ( _, ( _, LANGLE1left
, _)) :: rest671)) => let val  result = MlyValue.atomic_pat (fn _ =>
 let val  pat1 = pat1 ()
 val  pat2 = pat2 ()
 in (
#1 pat1 @ #1 pat2, 
                                   Pattern.Pair (#2 pat1, #2 pat2)
)
end)
 in ( LrTable.NT 42, ( result, LANGLE1left, RANGLE1right), rest671)

end
|  ( 80, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.pat pat1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.atomic_pat (fn _ => let val  (pat as pat1) = pat1 ()
 in (pat)
end)
 in ( LrTable.NT 42, ( result, LPAREN1left, RPAREN1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID'
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Exp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID',p1,p2))
fun EVAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID',p1,p2))
fun STEP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID',p1,p2))
fun LOAD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID',p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID',p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID',p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID',p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID',p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID',p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID',p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID',p1,p2))
fun LANGLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID',p1,p2))
fun RANGLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID',p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID',p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID',p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID',p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID',p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID',p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID',p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID',p1,p2))
fun IS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID',p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID',p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID',p1,p2))
fun WILD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID',p1,p2))
fun NAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID',p1,p2))
fun UNIT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID',p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID',p1,p2))
fun VOID (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID',p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID',p1,p2))
fun ZERO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID',p1,p2))
fun SUCC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID',p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun IFZ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID',p1,p2))
fun LAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID',p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID',p1,p2))
fun FIX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID',p1,p2))
fun DOTL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID',p1,p2))
fun DOTR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID',p1,p2))
fun ABORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID',p1,p2))
fun INL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID',p1,p2))
fun INR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID',p1,p2))
fun CASE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID',p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID',p1,p2))
end
end
