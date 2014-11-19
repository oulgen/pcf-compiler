structure Play =
struct
  open Term
  open TermOps
  open C0VM

  structure TLC = TopLevelCommands

  infix $
  infix $$

  fun buildC0Code num_vars code =
    let
      val fnI = FI {name = "main",
                    num_args = 0,
                    num_vars = num_vars,
                    code_length = code_length code,
                    code = code}

      val program = BC0File {int_pool = (0, Array.fromList []),
                              string_pool = (0, 0, Array.fromList []),
                              function_pool = (1, Array.fromList [SOME (fnI)]),
                              native_pool = []}

      val c0vm_version = 4
      val bytecode_arch = 64

      val result = C0VMPrint.pp_program c0vm_version bytecode_arch program
      val outs = TextIO.openOut ("test.bc0")
    in
      TextIO.output(outs, result);
      TextIO.closeOut outs
    end

  fun findIndexV v [] = raise Fail "variable unknown" 
    | findIndexV v (x::xs) = if Var.equal (v, x) then 0 else (1 + findIndexV v xs)

  fun convertToC0 valList (Z $ _ | Triv $ _) = [Inst (bipush 0, "zero", NONE)]
    | convertToC0 valList (S $ [z]) = 
          (convertToC0 valList (out z)) @ [Inst (bipush 1, "one", NONE), Inst (binop iadd, "add", NONE)]
    | convertToC0 valList (Pair $ [y, z]) =
      let
        val y' = convertToC0 valList (out y)
        val z' = convertToC0 valList (out z)
        val pre = [Inst (new 16, "alloc", NONE), Inst (dup, "dup", NONE)]
        val mid = [Inst (amstore, "store", NONE), Inst (dup, "dup", NONE), Inst (aaddf 8, "offset", NONE)]
        val final = [Inst (amstore, "store", NONE)]
      in
        pre @ y' @ mid @ z' @ final
      end
    | convertToC0 valList (Pr L $ [z]) = 
          (convertToC0 valList (out z)) @ [Inst (amload, "load", NONE)]
    | convertToC0 valList (Pr R $ [z]) = 
          (convertToC0 valList (out z)) @ [Inst (aaddf 8, "offset", NONE), Inst (amload, "load", NONE)]
    | convertToC0 valList (In (_,_,side) $ [z]) =
      let
        val n = if side = L then (Z $$ []) else (S $$ [Z $$ []])
      in
        convertToC0 valList (Pair $ [n, z])
      end
    | convertToC0 valList (Let $ [y,z]) =
      let
        val y' = convertToC0 valList (out y)
        val \(v, z') = (out z)
        val valList' = valList @ [v]
        val z'' = convertToC0 valList' (out z')
        val strY = [Inst (vstore (findIndexV v valList'), "store", NONE)]
      in
        y' @ strY @ z''
      end
    (* Variable *)
    | convertToC0 valList (` v) = [Inst (vload (findIndexV v valList), "load", NONE)]
    | convertToC0 valList (Ifz $ [x,y,z]) =
      let
        val x' = convertToC0 valList (out x)
        val y' = convertToC0 valList (out y)
        val comp = [Inst (bipush 0, "comp to 0", NONE), Inst (if_icmp (eq, (6, "then")), "comp", NONE), 
                    Inst (goto (code_length (y') + 6, "else"), "go", NONE), Label (0, "then")]

        (* else case *)
        val x'' = x' @ [Inst (bipush 1, "one", NONE), Inst (binop isub, "sub", NONE)]
        val \(v, z') = (out z)
        val valList' = valList @ [v]
        val z'' = convertToC0 valList' (out z')
        val strY = [Inst (vstore (findIndexV v valList'), "store", NONE)]

        val elseC = x'' @ strY @ z''
        
        (* jump to end *)
        val labelAndJump = [Inst (goto (code_length(elseC) + 3, "end"), "go", NONE), Label (0, "else")]
        val endLabel = [Label (0, "end")]
      in
        x' @ comp @ y' @ labelAndJump @ elseC @ endLabel
      end
    | convertToC0 valList _ = raise Fail "Die"

  fun eval s =
    let
      (*val TLC.Eval (SOME x) = Parse.parse (symbols.empty()) s;*)
      (*val Pr R $ [z] = out x;*)
      (*out x*)

      val TLC.Eval (SOME x) = Parse.parse (symbols.empty()) s

      val code = (convertToC0 [] (out x)) @ [Inst (return, "ret", NONE)]
      val num_vars = 10
    in
      buildC0Code num_vars code
    end

  fun printString s =
    let
      val TLC.Eval (SOME x) = Parse.parse (symbols.empty()) s;
    in
      Term.toString x
    end

  fun printOut s =
    let
      val TLC.Eval (SOME x) = Parse.parse (symbols.empty()) s;
    in
      Term.out x
    end

end
