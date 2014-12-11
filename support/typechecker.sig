signature TYPECHECKER =
sig
  (* The optional Term.t argument to TypeError. can be omitted, but it
   * will be reported by the toplevel repl when provided, which can be
   * useful for locating the source of type errors. *)
  exception TypeError of Term.t option

  type context = Type.t Context.table 

  val checktype : context -> Term.t -> Type.t 
end
