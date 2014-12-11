signature C0VM =
sig

  type label = int * string

  type var_index = int
  type byte = int (* 8 bits signed *)
  type size = int (* 16 bits unsigned *)
  type field_offset = int (* 8 bits signed *)
  type pool_index = int (* 16 bits unsigned *)
  type branch_offset = label (* 16 bits unsiged *)

  datatype comparison =
    eq | ne | lt | ge | gt | le

  datatype binop =
    iadd | iand | idiv | imul
  | ior | irem | ishl | ishr | isub | ixor

  datatype inst =
   (* stack op *)
     dup
   | pop
   | swap
   (* bin op *)
   | binop of binop
   (* local vars *)
   | vload of var_index
   | vstore of var_index
   (* constants *)
   | aconst_null
   | bipush of byte
   | ildc of pool_index
   | aldc of pool_index
   (* control flow *)
   | nop
   | if_icmp of comparison * branch_offset
   | goto of branch_offset
   | athrow | assert
   (* functions *)
   | invokestatic of pool_index
   | return
   | invokenative of pool_index
   (* memory *)
   | new of size
   | newarray of size
   | arraylength
   | aaddf of field_offset
   | aadds
   | imload
   | amload
   | imstore
   | amstore
   | cmload
   | cmstore

  datatype bcline =
     Inst of inst * string * Mark.ext option
   | Label of label
   | Comment of string

  datatype function_info =
     FI of {name : string,
	    num_args : int,
	    num_vars : int,
	    code_length : int,
	    code : bcline list}

  datatype native_info =
	   NI of {name : string,
		  num_args : int,
		  function_table_index : int}

  datatype program =
     BC0File of { int_pool : int * Word32.word array,
		  string_pool : int * int * string array,
		  function_pool : int * function_info option array,
		  native_pool : native_info list
		}

  val il : inst -> int
  val code_length : bcline list -> int

end (* signature C0VM *)

structure C0VM :> C0VM =
struct

  type label = int * string
  type var_index = int
  type byte = int
  type size = int
  type field_offset = int
  type pool_index = int
  type branch_offset = int * string

  datatype comparison =
    eq | ne | lt | ge | gt | le

  datatype binop =
    iadd | iand | idiv | imul
  | ior | irem | ishl | ishr | isub | ixor

  datatype inst =
   (* stack op *)
     dup
   | pop
   | swap
   (* bin op *)
   | binop of binop
   (* local vars *)
   | vload of var_index
   | vstore of var_index
   (* constants *)
   | aconst_null
   | bipush of byte
   | ildc of pool_index
   | aldc of pool_index
   (* control flow *)
   | nop
   | if_icmp of comparison * branch_offset
   | goto of branch_offset
   | athrow | assert
   (* functions *)
   | invokestatic of pool_index
   | return
   | invokenative of pool_index
   (* memory *)
   | new of size
   | newarray of size
   | arraylength
   | aaddf of field_offset
   | aadds
   | imload
   | amload
   | imstore
   | amstore
   | cmload
   | cmstore

  datatype bcline =
     Inst of inst * string * Mark.ext option
   | Label of label
   | Comment of string

  datatype function_info =
     FI of {name : string,
	    num_args : int,
	    num_vars : int,
	    code_length : int,
	    code : bcline list}

  datatype native_info =
	   NI of {name : string,
		  num_args : int,
		  function_table_index : int}

  datatype program =
     BC0File of { int_pool : int * Word32.word array,
		  string_pool : int * int * string array,
		  function_pool : int * function_info option array,
		  native_pool : native_info list
		}

  fun il (dup) = 1
    | il (pop) = 1
    | il (swap) = 1
    | il (binop(_)) = 1
     (* local vars *)
    | il (vload(i)) = 2
    | il (vstore(i)) = 2
     (* constants *)
    | il (aconst_null) = 1
    | il (bipush(b)) = 2
    | il (ildc(c)) = 3
    | il (aldc(c)) = 3
     (* control flow *)
    | il (nop) = 1
    | il (if_icmp(_,offset)) = 3
    | il (goto(offset)) = 3
    | il athrow = 1
    | il assert = 1
     (* functions *)
    | il (invokestatic(c)) = 3
    | il (return) = 1
    | il (invokenative(c)) = 3
     (* memory *)
    | il (new(s)) = 2
    | il (newarray(s)) = 2
    | il (arraylength) = 1
    | il (aaddf(f)) = 2
    | il (aadds) = 1
    | il (imload) = 1
    | il (amload) = 1
    | il (imstore) = 1
    | il (amstore) = 1
    | il (cmload) = 1
    | il (cmstore) = 1

  fun num_bytes (Comment(s)::bcs) i = num_bytes bcs i
    | num_bytes (Label(lab)::bcs) i = num_bytes bcs i
    | num_bytes (Inst(inst,_,_)::bcs) i = num_bytes bcs (i+il(inst))
    | num_bytes (nil) i = i

  fun code_length bcs = num_bytes bcs 0

end
