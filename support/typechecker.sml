structure TypeChecker : TYPECHECKER =
struct
  (* The optional Term.t argument to TypeError. can be omitted, but it
   * will be reported by the toplevel repl when provided, which can be
   * useful for locating the source of type errors. *)
  exception TypeError of Term.t option
  exception Unimplemented

  type context = Type.t Context.table
	
  fun equiv t1 t2 = Type.aequiv (t1, t2)

  fun checktype ctx e = raise Unimplemented
end
