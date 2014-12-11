structure TypeOps  =
struct
  datatype t = NAT | PARR | UNIT | PROD | VOID | SUM

  exception Unimplemented
  fun arity NAT = []
    | arity PARR = [0,0]
    | arity UNIT = []
    | arity PROD = [0,0]
    | arity VOID = []
    | arity SUM = [0,0]

  fun equal (x : t, y : t) = x = y

  fun toString NAT = "nat"
    | toString PARR = "parr"
    | toString UNIT = "unit"
    | toString PROD = "prod"
    | toString VOID = "void"
    | toString SUM = "sum"
end
