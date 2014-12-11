structure TermOps =
struct

  datatype side = L | R

  datatype t = Z | S | Ifz | Let | Lam of Type.t | Ap | Fix of Type.t  (* PCF *)
             | Abort of Type.t | In of Type.t * Type.t * side | Case (* sums *)
             | Triv | Pair | Pr of side (* products *)

  exception Unimplemented
  fun arity Z = []
    | arity S = [0]
    | arity Ifz = [0,0,1]
    | arity Let = [0,1]
    | arity (Lam _) = [1]
    | arity Ap = [0,0]
    | arity (Fix _) = [1]
    | arity (Abort _) = [0]
    | arity (In _) = [0]
    | arity Case = [0,1,1]
    | arity Triv = []
    | arity Pair = [0,0]
    | arity (Pr _) = [0]

  fun equal (Z, Z) = true
    | equal (S, S) = true
    | equal (Ifz, Ifz) = true
    | equal (Let, Let) = true
    | equal (Lam t1, Lam t2) = Type.aequiv (t1, t2)
    | equal (Ap, Ap) = true
    | equal (Fix t1, Fix t2) = Type.aequiv (t1, t2)
    | equal ((Abort t1), (Abort t2)) = Type.aequiv (t1, t2)
    | equal (In (s1, s2, L), In (t1, t2, L)) =
        (Type.aequiv (s1, t1)) andalso
        (Type.aequiv (s2, t2))
    | equal (In (s1, s2, R), In (t1, t2, R)) =
        (Type.aequiv (s1, t1)) andalso
        (Type.aequiv (s2, t2))
    | equal (Case, Case) = true
    | equal (Triv, Triv) = true
    | equal (Pair, Pair) = true
    | equal (Pr L, Pr L) = true
    | equal (Pr R, Pr R) = true
    | equal _ = false

  fun toString Z = "z"
    | toString S = "s"
    | toString Ifz = "ifz"
    | toString Let = "let"
    | toString (Lam t) = "lam[" ^ (Type.toString t) ^ "]"
    | toString Ap = "ap"
    | toString (Fix t) = "fix[" ^ (Type.toString t) ^ "]"
    | toString (Abort t) = "abort[" ^ (Type.toString t) ^ "]"
    | toString (In (t1, t2, L)) = "in[" ^ Type.toString t1 ^ "; " ^ Type.toString t2 ^ "][l]"
    | toString (In (t1, t2, R)) = "in[" ^ Type.toString t1 ^ "; " ^ Type.toString t2 ^ "][r]"
    | toString Case = "case"
    | toString Triv = "triv"
    | toString Pair = "pair"
    | toString (Pr L) = "pr[l]"
    | toString (Pr R) = "pr[r]"
end
