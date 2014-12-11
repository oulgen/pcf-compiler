signature C0VM_PRINT=
sig

  (* pp_program c0vm_version bytecode_arch bytecode = bc string *)
  val pp_program : int -> int-> C0VM.program -> string

end (* signature C0VM_PRINT *)

structure C0VMPrint :> C0VM_PRINT =
struct

  structure V = C0VM

  fun pad8(s) = if String.size(s) < 2
		then "0" ^ s else s

  fun u8(i) = (* 0 <= i < 256 *)
      if i < 0 orelse i >= 256 then raise Fail ("Invalid bytecode: u8: " ^ (Int.toString i))
      else pad8(Int.fmt StringCvt.HEX i)
  fun u16(c) = (* 0 <= c < 65536 *)
      if c < 0 orelse c >= 65536 then raise Fail ("Invalid bytecode: u16: " ^ (Int.toString c))
      else u8(c div 256) ^ " " ^ u8(c mod 256)
  fun u32(c) =
      u16(Word32.toInt(Word32.>>(c,Word.fromInt(16)))) ^ " "
      ^ u16(Word32.toInt(Word32.mod(c,Word32.fromInt(65536))))
  fun s8(b) = (* -128 <= b < 127 *)
      if b < ~128 orelse b >= 128 then raise Fail ("Invalid bytecode: s8: " ^ (Int.toString b))
      else u8(if b < 0 then b+256 else b)
  fun s16(c) = (* -2^15 <= c < 2^15-1 *)
      if c < ~32768 orelse c >= 32768 then raise Fail ("Invalid bytecode: s16: " ^ (Int.toString c))
      else u16(if c < 0 then c+65536 else c)
  fun s16dec(c) = (* for jump offsets *)
      if c < 0
      then "-" ^ Int.toString(~c)
      else "+" ^ Int.toString(c)
  fun s8dec(b) = (* for bipush immediate operands *)
      if b < 0
      then "-" ^ Int.toString(~b)
      else Int.toString(b)
  fun pad(s,n) = if String.size(s) < n
		 then pad(s ^ " ",n)
		 else s
  fun pad9(s) = pad(s,9)
  fun pad18(s) = pad(s,18)
  fun pad27(s) = pad(s,27)


  fun pp_inst2 (V.dup) = ("59","dup")
    | pp_inst2 (V.pop) = ("57","pop")
    | pp_inst2 (V.swap) = ("5F","swap")
    | pp_inst2 (V.binop(V.iadd)) = ("60","iadd")
    | pp_inst2 (V.binop(V.iand)) = ("7E","iand")
    | pp_inst2 (V.binop(V.idiv)) = ("6C","idiv")
    | pp_inst2 (V.binop(V.imul)) = ("68","imul")
    | pp_inst2 (V.binop(V.ior)) = ("80","ior")
    | pp_inst2 (V.binop(V.irem)) = ("70","irem")
    | pp_inst2 (V.binop(V.ishl)) = ("78","ishl")
    | pp_inst2 (V.binop(V.ishr)) = ("7A","ishr")
    | pp_inst2 (V.binop(V.isub)) = ("64","isub")
    | pp_inst2 (V.binop(V.ixor)) = ("82","ixor")
    | pp_inst2 (V.vload(i)) = ("15 " ^ u8(i), "vload " ^ Int.toString(i))
    | pp_inst2 (V.vstore(i)) = ("36 " ^ u8(i), "vstore " ^ Int.toString(i))
    | pp_inst2 (V.aconst_null) = ("01", "aconst_null")
    | pp_inst2 (V.bipush(b)) = ("10 " ^ s8(b), "bipush " ^ s8dec(b))
    | pp_inst2 (V.ildc(c)) = ("13 " ^ u16(c), "ildc " ^ Int.toString(c))
    | pp_inst2 (V.aldc(c)) = ("14 " ^ u16(c), "aldc " ^ Int.toString(c))
    | pp_inst2 (V.nop) = ("00", "nop")
    | pp_inst2 (V.if_icmp(V.eq, (offset, _))) = ("9F " ^ s16(offset), "if_cmpeq " ^ s16dec(offset))
    | pp_inst2 (V.if_icmp(V.ne, (offset, _))) = ("A0 " ^ s16(offset), "if_cmpne " ^ s16dec(offset))
    | pp_inst2 (V.if_icmp(V.lt, (offset, _))) = ("A1 " ^ s16(offset), "if_icmplt " ^ s16dec(offset))
    | pp_inst2 (V.if_icmp(V.ge, (offset, _))) = ("A2 " ^ s16(offset), "if_icmpge " ^ s16dec(offset))
    | pp_inst2 (V.if_icmp(V.gt, (offset, _))) = ("A3 " ^ s16(offset), "if_icmpgt " ^ s16dec(offset))
    | pp_inst2 (V.if_icmp(V.le, (offset, _))) = ("A4 " ^ s16(offset), "if_icmple " ^ s16dec(offset))
    | pp_inst2 (V.goto(offset, _)) = ("A7 " ^ s16(offset), "goto " ^ s16dec(offset))
    | pp_inst2 (V.athrow) = ("BF", "athrow")
    | pp_inst2 (V.assert) = ("CF", "assert")
    | pp_inst2 (V.invokestatic(c)) = ("B8 " ^ u16(c), "invokestatic " ^ Int.toString(c))
    | pp_inst2 (V.return) = ("B0","return")
    | pp_inst2 (V.invokenative(c)) = ("B7 " ^ u16(c), "invokenative " ^ Int.toString(c))
    | pp_inst2 (V.new(s)) = ("BB " ^ u8(s), "new " ^ Int.toString(s))
    | pp_inst2 (V.newarray(s)) = ("BC " ^ u8(s), "newarray " ^ Int.toString(s))
    | pp_inst2 (V.arraylength) = ("BE", "arraylength")
    | pp_inst2 (V.aaddf(offset)) = ("62 " ^ u8(offset), "aaddf " ^ Int.toString(offset))
    | pp_inst2 (V.aadds) = ("63", "aadds")
    | pp_inst2 (V.imload) = ("2E", "imload")
    | pp_inst2 (V.amload) = ("2F", "amload")
    | pp_inst2 (V.imstore) = ("4E", "imstore")
    | pp_inst2 (V.amstore) = ("4F", "amstore")
    | pp_inst2 (V.cmload) = ("34", "cmload")
    | pp_inst2 (V.cmstore) = ("55", "cmstore")

  fun pp_inst inst =
      let val (code, mnemonic) = pp_inst2 inst
      in pad9(code) ^ "# " ^ mnemonic end

  fun pp_bclines n (V.Comment(s)::bcs) = "# " ^ s ^ "\n" ^ pp_bclines n bcs
    | pp_bclines n (V.Label(k, s)::bcs) = "# " ^ s ^ "\n" ^ pp_bclines n bcs
    | pp_bclines n (V.Inst(inst, anno, ext)::bcs) =
      (* "/" ^ pad(Int.toString(n),3) ^ "/ " ^ *)
        pad27(pp_inst inst) ^ "# " ^ anno ^ "\n"
	^ pp_bclines (n+V.il(inst)) bcs
    | pp_bclines n (nil) = ""

  fun pp_function_info (SOME(V.FI {name = name,
			     	   num_args = num_args,
			      	   num_vars = num_vars,
			      	   code_length = code_length,
			      	   code = is})) =
      "\n#<" ^ name ^ ">\n"
      ^ pad18(u16(num_args)) ^ "# number of arguments = " ^ Int.toString(num_args) ^ "\n"
      ^ pad18(u16(num_vars)) ^ "# number of local variables = " ^ Int.toString(num_vars) ^ "\n"
      ^ pad18(u16(code_length)) ^ "# code length = " ^ Int.toString(code_length) ^ " bytes\n"
      ^ pp_bclines 0 is
    | pp_function_info (NONE) =
      "\n#<unused function>\n"
      ^ pad18(u16(0)) ^ "# number of arguments = 0\n"
      ^ pad18(u16(0)) ^ "# number of local variables = 0\n"
      ^ pad18(u16(0)) ^ "# code length = 0 bytes\n"

  fun ppfp function_pool_array i n =
      if i < n
      then pp_function_info (Array.sub(function_pool_array, i)) ^ "\n"
           ^ ppfp function_pool_array (i+1) n
      else ""

  fun pp_function_pool (n, function_pool_array) =
      ppfp function_pool_array 0 n

  fun pp_native_info (V.NI {name = name,
			    num_args = num_args,
			    function_table_index = nindex}) =
      pad18(u16(num_args) ^ " " ^ u16(nindex)) ^ "# " ^ name ^ "\n"

  fun pp_magic () = "C0 C0 FF EE"

  fun pp_version c0vm_version bytecode_arch =
      u16(c0vm_version * 2 + (if bytecode_arch = 32 then 0 else 1))

  fun ppip int_pool_array i n =
      if i < n
      then u32(Array.sub(int_pool_array, i)) ^ "\n"
	   ^ ppip int_pool_array (i+1) n
      else "\n"

  fun pp_int_pool (n, int_pool_array) =
      ppip int_pool_array 0 n

  fun charsToHex (nil) = u8(0)
    | charsToHex (c::cs) = u8(Char.ord(c)) ^ " " ^ charsToHex(cs)

  fun ppsp string_pool_array i n =
      if i < n
      then charsToHex(String.explode(Array.sub(string_pool_array, i)))
	   ^ "  # \"" ^ String.toCString(Array.sub(string_pool_array, i)) ^ "\"\n"
	   ^ ppsp string_pool_array (i+1) n
      else "\n"

  fun pp_string_pool (n, string_pool_array) =
      ppsp string_pool_array 0 n

  fun pp_program c0vm_version bytecode_arch
		 (V.BC0File {int_pool = (int_pool_length, int_pool_array),
			     string_pool = (string_pool_length, string_pool_size, string_pool_array),
			     function_pool = (function_pool_length, function_pool_array),
			     native_pool = nilist}) =
      pad18(pp_magic()) ^ "# magic number\n"
      ^ pad18(pp_version c0vm_version bytecode_arch)
      ^ "# version " ^ Int.toString(c0vm_version)
      ^ ", arch = " ^ Int.toString(if bytecode_arch = 32 then 0 else 1)
      ^ " (" ^ Int.toString(bytecode_arch) ^ " bits)\n\n"
      ^ pad18(u16(int_pool_length)) ^ "# int pool count\n"
      ^ "# int pool\n"
      ^ pp_int_pool (int_pool_length, int_pool_array)
      ^ pad18(u16(string_pool_size)) ^ "# string pool total size\n"
      ^ "# string pool\n"
      ^ pp_string_pool (string_pool_length, string_pool_array)
      ^ pad18(u16(function_pool_length)) ^ "# function count\n"
      ^ "# function_pool\n"
      ^ pp_function_pool (function_pool_length, function_pool_array)
      ^ pad18(u16(List.length(nilist))) ^ "# native count\n"
      ^ "# native pool\n"
      ^ String.concat (List.map pp_native_info nilist) ^ "\n"
        
end
