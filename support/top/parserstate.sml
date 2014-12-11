structure ParserState =
struct

structure T = Term
structure TLC = TopLevelCommands
exception Parse of string

val symtable : Var.t symbols.table ref = ref (symbols.empty ())
val stack : Var.t symbols.table list ref = ref nil

fun settable table =
 (symtable := table; 
  stack := nil; 
  ())

fun savetable () = ((stack := !symtable :: !stack); ())

fun restoretable () = ((symtable := hd(!stack));(stack := tl (!stack)); ())

fun getvar id = 
  case symbols.find (!symtable) id of
     SOME (v) => v
   | _ => raise Parse("Undefined identifier: " ^ id)

fun addvar id = 
   let 
      val var = Var.newvar id
      (*val _ = TextIO.print ("addvar" ^ (Var.toString var) ^ "\n")*)
   in
     symtable := symbols.insert (fn (old,new) => new) (id, var) (!symtable);
     var
   end
		                 
fun createlet (e1,var) e2 =  T.$$(TermOps.Let, [e1, T.\\(var, e2)])

end 
