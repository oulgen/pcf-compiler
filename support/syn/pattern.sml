structure Pattern : PATTERN =
struct

  datatype side = L | R                  

  datatype t = Wild | Var 
             | Z | S of t 
             | Triv | Pair of t * t | In of side * t 

  exception Unimplemented
  fun numvars p = raise Unimplemented

  fun equals (Z, Z) =  true
    | equals (S t1, S t2) = equals (t1,t2)
    | equals (Wild, Wild) = true
    | equals (Var, Var) = true
    | equals (Triv, Triv) = true
    | equals (Pair (p1, p2), Pair (q1, q2)) = (equals (p1, q1) 
                                               andalso equals (p2, q2))
    | equals (In (L, t1), In (L, t2)) = equals (t1,t2)
    | equals (In (R, t1), In (R, t2)) = equals (t1,t2)
    | equals _ = false

  fun toString p = 
      (case p of 
  	   Wild => "_"
  	 | Var => "#"
  	 | Z => "z"
  	 | S p => "s(" ^ (toString p) ^ ")"
  	 | Triv => "()"
  	 | Pair (p1, p2) => "(" ^ (toString p1) ^ ", " ^ (toString p2) ^ ")"
  	 | In (L, p) => "inl(" ^ (toString p) ^ ")"
  	 | In (R, p) => "inr(" ^ (toString p) ^ ")")
   
end
