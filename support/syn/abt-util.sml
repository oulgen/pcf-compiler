functor ABT_Util(A : ABT) : ABT_UTIL = 
struct
  open A 

  fun freevars e =
    case A.out e of
	A.` x        => [x]
      | A.$(f, es) => List_Util.collate A.Variable.equal (List.map freevars es)
      | A.\(z, e')  => List_Util.remove A.Variable.equal z (freevars e')

  val `` = A.into o A.`
  val \\ = A.into o A.\
  val $$ = A.into o A.$ 

  fun subst e x body =
    A.into(case A.out body of 
	     A.` y => if A.Variable.equal(x, y) then A.out e else A.` y
	   | A.$(f, args) => A.$(f, List.map (subst e x) args)
	   | A.\(z, arg) => A.\(z, subst e x arg))

  fun separatebinders (e : t) : (Variable.t list * t) = 
    (case A.out e of 
      \(var, e') => 
        let 
          val (binders, e'') = separatebinders e'
        in 
          ((var::binders), e'') 
        end
    | _ => ([], e)
    )

  fun toString e =
     case A.out e of
       A.` x => A.Variable.toString x
     | A.$(f, []) => (A.Operator.toString f) 
     | A.$(f, es) => (A.Operator.toString f) ^ "(" ^ (toStrings es) ^ ")"
     | A.\(x, e) => (A.Variable.toString x) ^ ". " ^ (toString e)
  and toStrings [] = ""
    | toStrings [e] = toString e
    | toStrings (e :: es) = (toString e) ^ ", " ^ (toStrings es)
end
