functor TopLevelFn(structure D : DYNAMICS
                   structure T : TYPECHECKER) : TOPLEVEL = 
struct

structure TM = Term
structure TLC = TopLevelCommands

exception InternalError
exception NothingToDo


fun uncheckedStep term = 
    (case D.view(D.trystep term) of 
          D.Step (t) => (TLC.Next (t), SOME (t))
        | D.Val      => (TLC.Val (term), NONE)
        | D.Err      => raise InternalError
    )

fun checkType term = 
    let
        val tau = T.checktype (Context.empty ()) term
        val _ = TextIO.print ("Statics : exp has type : " ^ (Type.toString tau) ^ "\n");
    in
      tau
    end


fun doStep stepmem x = 
    let
        val (term) = 
            (case x of 
                  SOME(t) => t
                | NONE => case stepmem of
                              SOME(t) => t
                            | NONE => raise NothingToDo)
        val _ = if(isSome x) 
                then checkType term else Type.$$ (TypeOps.NAT, [])
    in
            uncheckedStep term
    end

fun doEval stepmem x = 
    let
        val (term) = 
            (case x of 
                  SOME(t) => t
                | NONE => case stepmem of
                              SOME(t) => t
                            | NONE => raise NothingToDo)
        val _ = if(isSome x) 
                then checkType term else Type.$$ (TypeOps.NAT, [])
    in
                (TLC.Val (D.eval term), NONE)
    end


(* evalCore : (dynMode * Term.t) option -> TLC.cmd -> (TLC.res * (dynMode * Term.t) option)*)
fun evalCore stepmem parseRes = 
     (case parseRes of
         TLC.Step(x) => doStep stepmem x
       | TLC.Eval(x) => doEval stepmem x
     )
(*			
fun resToString result = 
      let 
         val TLC.Result(var, res, res_type) = result
      in
         ("val " ^ (Var.toUserString var) ^ " = " ^ (Real.toString res) 
            ^ " : " ^ (TypeChecker.toString res_type) ^ "\n" )
      end
*)
fun resToString result = 
    (case result of
         TLC.Next(e) => " --> " ^Term.toString(e) ^"\n\n"
       | TLC.Val(e)  => " " ^Term.toString(e) ^" VAL\n\n"
    )

fun processDef text (res,stepmem) = 
   let
      val parseRes = Parse.parse (symbols.empty ()) text
      val (res, stepmem') = evalCore stepmem parseRes
	  val output = resToString res					               
   in
   	  (SOME(res),stepmem')
   end
	   
fun processDefRepl text a = 
   let
      val (res,stepmem') = processDef text a
   in
      case res of
         SOME(r) => (TextIO.print (resToString r) ; (res,stepmem'))
      |  NONE => raise InternalError
   end
   
fun hdl f x y = (f x y)
      handle ParserState.Parse(s) => 
	  		((TextIO.print ("Parse error: " ^ s ^ "\n")) ; y)
	   | T.TypeError NONE => ((TextIO.print "TypeChecker error!\n") ; y)
	   | T.TypeError (SOME e) => ((TextIO.print "TypeChecker error!\n") ;
                                      (TextIO.print ("Location: " ^ 
                                                     Term.toString e ^"\n")) ;
                                      y)
	   | D.Malformed => ((TextIO.print "Something went seriously wrong in UncheckedDynamics!\n") ; y)
	   | NothingToDo => ((TextIO.print "Nothing to do!\n") ; y)
           | Lang.LangError s => ((TextIO.print ("Error: " ^ s ^ "\n")) ; y)
       | exn => ((TextIO.print ("Unexpected error: "^exnName exn^"["^exnMessage exn^"]\n")); y)
   
local
   fun revtostring L = String.implode (foldl (op ::) [] L)
in
fun stringStream L s = case Stream.force s of
      (Stream.Cons((#";",_),s)) => 
	     Stream.cons(revtostring L, 
		 Stream.delay(fn () => Stream.force(stringStream [] s)))
    | (Stream.Cons((c,_),s)) => stringStream (c::L) s
    | (Stream.Nil) => Stream.cons(revtostring L,Stream.empty)
end
	
fun repl () =
   (Stream.fold (hdl processDefRepl) (NONE,NONE)
      (Input.promptKeybd "->" "=>" (stringStream [])) ; ())
            
fun eval s =
   let
      val (res,_) = Stream.fold (processDef) (NONE,NONE)
         (stringStream [] 
      	   (foldr (Stream.cons) (Stream.empty) 
      	   (map (fn c => (c,[])) (String.explode s))))
   in
      case res of
         SOME(r) => r
      |  NONE => ( print "No result!\n" ; raise InternalError )
   end

(*fun parse_vars (vars,text) = 
	(case (Parse.parse 
		(foldr (fn (x,vs) => 
			symbols.insert(vs,x,Var.newvar x)) 
			(EN.symbolTable (EN.newenv())) vars) 
		text) of
		TLC.Def(_,_,t) => t
	|	TLC.Eval(t) => t)
*)      
end

 
