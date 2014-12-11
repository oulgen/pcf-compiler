structure TestHarness :> TESTHARNESS =
struct
   structure T = TypeChecker
   structure UD = UncheckedDynamics
   structure TLC = TopLevelCommands
   structure TL = TopLevel
                      
   datatype 'a expected_result =
            Pass of 'a 
            | Fail
            | TypeFail 
            | ParseFail
            | EvalFail

   (* Tests cases are a list of commands an expected results *)

    val natt = Type.$$(TypeOps.NAT, []);
    val unitt = Type.$$(TypeOps.UNIT, []);
    val voidt = Type.$$(TypeOps.VOID, []);

    fun Num n = 
        if (n = 0) 
        then Term.$$((TermOps.Z),[])
        else Term.$$((TermOps.S),[Num (n-1)])
    fun Pair (t1, t2) = Term.$$((TermOps.Pair, [t1, t2]))
    fun InL (t1, t2, e) = Term.$$(TermOps.In (t1, t2, TermOps.L), [e])
    fun InR (t1, t2, e) = Term.$$(TermOps.In (t1, t2, TermOps.R), [e])

    type test = string * (Term.t expected_result) 
    val tests : test list = [
      ("eval (fix sum : nat->nat->nat is fn (x : nat) fn (y : nat) " ^
       "      ifz x {z => y | s u => s(sum u y)}) (s(z)) (s(s(z)))",
       Pass (Num 3)),
      ("eval (fix sum : nat->nat->nat is fn (x : nat) fn (y : nat) " ^
       "      ifz x {z => y | s(u) => s(sum u y)}) (s z) (s(s z))",
       Pass (Num 3)),
      ("eval case inl[nat,unit] z " ^
       "       { inl (x) => <s x,x> " ^
       "       | inr(p) => <z,z>}", 
       Pass(Pair (Num 1, Num 0))),
      ("eval case inl[nat,void](z) " ^
       "       { inl x => <s x,x> " ^
       "       | inr p => abort[nat*nat] p}",
       Pass(Pair (Num 1, Num 0))),
      ("eval <z,s z> .l", Pass(Num 0)),
      ("eval <z,s z> .r", Pass(Num 1)),
      ("eval inl[nat,unit](z)", Pass(InL (natt, unitt, Num 0))),
      ("eval inl[nat,void] z", Pass(InL (natt, voidt, Num 0))),
      ("eval inr[nat,unit] <>", Pass(InR (natt, unitt, 
                                           Term.$$(TermOps.Triv, [])))),
      ("eval inr[nat,unit](z)", TypeFail)
    ]

                                    
   fun vprint verb s = if verb then print s else ()

   fun success verb n = 
       (if verb then print ((Int.toString n) ^ ": Success!\n") else ();
        true)
	   
   fun hdl verb n f comp exp = 
       let val res = f()
       in
           (case exp of
                Pass(x) => 
                let val passed = comp(res,x)
                in
		    if passed then success verb n 
		    else (vprint verb ((Int.toString n) 
				       ^ ": Failed: Result does not match\n");
                          false)
		end
	      | _ => ((vprint verb "Failed...\n") ; false))
       end handle (ParserState.Parse(s)) => 
		  (case exp of
		       Fail => success verb n
		      |	ParseFail => success verb n
		      |	_ => (vprint verb ((Int.toString n) 
                                           ^ ": Failed: Parse Error: "
					   ^ s ^ "\n");
			      false))
	        | T.TypeError _ => 
		  (case exp of
		       Fail => success verb n
		      |	TypeFail => success verb n
		      |	_ => (vprint verb ((Int.toString n) 
                                           ^ ": Failed: Type Error\n");
			      false))
		| UD.RuntimeError => 
		  (case exp of
		       Fail => success verb n
		      |	EvalFail => success verb n
		      |	_ => (vprint verb ((Int.toString n) ^ 
					   ": Failed: Runtime Error\n");
                              false))
		| UD.Malformed => 
		  (case exp of
		       Fail => success verb n
		      |	_ => (vprint verb ((Int.toString n) ^ 
					   ": Failed: Unexpected Error " ^
                                           "in UncheckedDynamics\n");
                              false))
		| _ => 
		  (case exp of
		       Fail => success verb n
		      |	_ => (vprint verb ((Int.toString n) ^ 
					   ": Failed: Other Exception\n");
                              false))
                      
   exception TestError
                 
                 
                 
   (*Each eval evaluates to either a number or a string. A test takes an 
    eval command and an expected result of the corresponding type. *)

	fun runtest v ((text,exp_res),(L,n)) = ((hdl v n (fn () => TL.eval text) 
			(fn (TLC.Val(t),x) => (Term.aequiv(t,x)) | _ => raise TestError) exp_res)::L,n+1)
	
    (*Each step command either steps to another term, is a val
      or results in an error. A steptest tests whether a command steps or not.
      In the case of unchecked dynamics, an ill typed expression is expected
      to result in a TypeFail.*)
    type steptest = string * (bool expected_result)
	val steptests = 
	[
    ]

	fun runsteptest v ((text,exp_res),(L,n)) = ((hdl v n (fn () => TL.eval text) 
			(fn (TLC.Next(t),x) => x | (_,x) => not(x)) exp_res)::L,n+1)


      
	fun summarize [] (pass,fail) = 
		((if fail > 0 then
        TextIO.print "-------------------------------------------------------\n"
		else ());
       TextIO.print ("\nTests completed: " 
	   		^ (Int.toString (pass + fail)) ^ "\n");
       TextIO.print ("Tests passed   : " ^ (Int.toString pass)        ^ "\n");
       TextIO.print ("Tests failed   : " ^ (Int.toString fail)        ^ "\n");
       if fail = 0 then
         TextIO.print "Congratulations!\n" 
       else
         () )
	|	summarize (result::results) (pass,fail) =
        let 
          val stats' = if result then (pass+1,fail) else (pass,fail+1)
        in
          summarize results stats'
        end

	fun fst (x,_) = x (* makes the next three functions much nicer *)
                 
	fun runtests verbose = (print "\n\nRunning normal tests...\n";
  		summarize (fst (foldl (runtest verbose) ([],1) tests)) (0,0))
	fun runsteptests verbose = (print "\n\nRunning step tests...\n";
  		summarize (fst (foldl (runsteptest verbose) ([],1) steptests)) (0,0))
           
	fun runalltests v = (runtests v ; runsteptests v)
end
