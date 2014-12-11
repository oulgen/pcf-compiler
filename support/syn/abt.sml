functor Abt(O : OPERATOR) :> ABT where type Variable.t = Var.t 
                                 where type Operator.t = O.t
=
struct
   open List_Util

   structure Variable = Var

   structure Operator = O 

   datatype 'a view = 
     ` of Variable.t
   | \ of Variable.t * 'a
   | $ of Operator.t * 'a list

   fun map f (` x)        = ` x
     | map f (\(x, a))   = \(x, f a)
     | map f ($(h, vs)) = $(h, List.map f vs)

   datatype t =
     FV of Var.t
   | BV of int
   | ABS of t
   | OPER of Operator.t * t list

   exception Malformed

   fun valence_ok (n, e) =
     case e of 
       ABS t => if n > 0 then valence_ok (n-1, t) else false
     | _     => if n = 0 then true else false

   fun aequiv (FV x, FV y) = Var.equal(x, y)
     | aequiv (BV n, BV m) = (n = m)
     | aequiv (ABS t, ABS t') = aequiv(t, t')
     | aequiv (OPER(f, ts), OPER(f', ts')) =
         O.equal(f,f') andalso List_Util.zipTest aequiv ts ts'
     | aequiv (_, _) = false

   fun abs x t = 
     let fun bind' i t =
	   case t of
	     FV y        => if Var.equal(x, y) then BV i else FV y
	   | ABS t       => ABS (bind' (i+1) t)
	   | BV n        => BV n
	   | OPER(f, ts) => OPER(f, List.map (bind' i) ts)
     in
       ABS (bind' 0 t)
     end

   fun unabs x t =
     let fun unabs' i t =
	   case t of
	     BV j  	 => if i = j then FV x else BV j 
	   | ABS t 	 => ABS (unabs' (i+1) t)
	   | FV x  	 => FV x
	   | OPER(f, ts) => OPER(f, List.map (unabs' i) ts)
     in
       unabs' 0 t
     end

   fun into (` x)        = FV x
     | into (\(x, t))   = abs x t
     | into ($(f, es)) = 
       if List.all valence_ok (zip_exact Malformed (O.arity f) es)
       then OPER(f, es)
       else raise Malformed

   exception Assertion_failure_name

   fun out t =
     case t of
       BV _        => raise Assertion_failure_name
     | FV x        => ` x
     | OPER(f, ts) => $(f, ts)
     | ABS t       => let val x' = Var.newvar "x"
		      in
			\(x', unabs x' t)
		      end
end
