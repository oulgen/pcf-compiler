structure UncheckedDynamics : DYNAMICS =
struct

  exception Unimplemented
  exception RuntimeError
  exception Malformed
  exception Abort

  datatype d = STEP of Term.t | VAL
  datatype D = Step of Term.t | Val | Err

  fun view d1 = (case d1 of
    STEP t1 => Step t1
    | VAL => Val
  )

  fun trystep e = raise Unimplemented
  fun eval e = raise Unimplemented
end
