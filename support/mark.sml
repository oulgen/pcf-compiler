(* C0 Compiler
 * Positional Markers
 * Frank Pfenning
 *)

signature MARK =
sig
  (* ((line1, col1), (line2, col2), filename) : ext *)
  type ext = (int * int) * (int * int) * string

  val show : ext -> string     (* print ext as filename:line1.col1-line2.col2 *)
  val show' : ext option -> string (* like show, but given <unknown
location> if NONE *)
  val show_source : ext -> string (* print first line of ext in source file *)

  type 'a marked           (* value of type 'a marked with extent *)

  val mark : 'a * ext -> 'a marked (* mark a value with extent *)
  val mark' : 'a * ext option -> 'a marked (* mark a value with
optional extent *)

  val data : 'a marked -> 'a    (* obtain marked value *)
  val ext : 'a marked -> ext option (* obtain extent, if present *)

  (* currently unused utilities *)
  val wrap : ext option list -> ext option (* union of extents, if in
same file *)
  val map : ('a -> 'b) -> 'a marked -> 'b marked (* retain extents *)
  val map' : ('a marked -> 'b) -> 'a marked -> 'b marked (* retain extents *)
end

structure Mark :> MARK =
struct
  (* ((line1, col1), (line2, col2), filename) : ext *)
  (* inclusive on left, exclusive on right *)
  type ext = (int * int) * (int * int) * string

  (* col 0 means no column info, just show line number *)
  fun pos (line, 0) = Int.toString line
    | pos (line, col) = Int.toString line ^ "." ^ Int.toString col

  fun show (left, right, file) = file ^ ":" ^ pos left ^ "-" ^ pos right

  fun show' (SOME(ext)) = show ext
    | show' (NONE) = "<unknown location>"

  fun theLine (NONE) = NONE
    | theLine (SOME(line)) = SOME(String.substring(line, 0,
String.size(line)-1))

  fun inputLines 0 instream = NONE
    | inputLines 1 instream = theLine (TextIO.inputLine instream)
    | inputLines n instream =
      let val _ = TextIO.inputLine instream (* ignore line *)
      in
          inputLines (n-1) instream
      end

  (* first version: ^-----^ *)
  (*
  fun createLine col1 col2 =
      String.implode (List.tabulate (col2, fn i =>
        let val c = i+1
        in
            if c < col1 then #" "
            else if c = col1 then #"^"
            else if col1 < c andalso c < col2-1 then #"-"
            else if c = col2-1 then #"^"
            else #" "
        end))
  *)
  (* second version: ~~~~~~~ *)
  fun createLine col1 col2 =
      String.implode (List.tabulate (col2, fn i =>
        let val c = i+1
        in
            if c < col1 then #" "
            else if col1 <= c andalso c < col2 then #"~"
            else #" "
        end))

  fun count_whitespace i s =
      if i < String.size(s) andalso Char.isSpace(String.sub(s,i))
      then count_whitespace (i+1) s
      else i

  fun show_source (left as (line1,col1), right as (line2,col2), file) =
      SafeIO.withOpenIn file (fn instream =>
        case inputLines line1 instream
         of NONE => "[location at end of file]\n"
          | SOME(first_line) =>
            if line1 = line2
            then first_line ^ "\n" ^ createLine col1 col2 ^ "\n"
            else let val second_line = case inputLines (line2-line1) instream
                                        of NONE => "<eof>"
                                         | SOME(line) => line
                     val ws_count = count_whitespace 0 second_line
                     val error_line = first_line ^ " ... "
                                      ^ String.extract(second_line,
ws_count, NONE)
                     val indicator = createLine col1 (String.size
first_line + 5 + col2 - ws_count)
                 in
                     error_line ^ "\n" ^ indicator ^ "\n"
                 end)
      handle IO.Io _ => "" (* no source file *)

  type 'a marked = 'a * ext option

  fun mark (x, ext) = (x, SOME ext)
  fun mark' (x, ext_opt) = (x, ext_opt)

  fun data (x, ext_opt) = x
  fun ext (x, ext_opt) = ext_opt

  fun extmin ((line1, col1), (line2, col2)) =
    if line1 < line2 then (line1, col1)
    else if line1 > line2 then (line2, col2)
    else (* line1 = line2 *)
        (line1, Int.min (col1, col2))

  fun extmax ((line1, col1), (line2, col2)) =
    if line1 > line2 then (line1, col1)
    else if line1 > line2 then (line2, col2)
    else (* line1 = line2 *)
        (line1, Int.min (col1, col2))

  fun wrap nil = NONE
    | wrap (ext_opt :: nil) = ext_opt
    | wrap (ext_opt :: ext_opts) =
      (case (ext_opt, wrap ext_opts) of
       (_, NONE) => NONE
     | (SOME (left1, right1, filename1), SOME (left, right, filename)) =>
       if filename1 = filename
       then SOME (extmin (left1, left), extmax (right1, right), filename)
       else NONE
     | (NONE, ext_opt') => ext_opt')

  fun map f (x, ext_opt) = (f x, ext_opt)
  fun map' f (m as (x, ext_opt)) = (f m, ext_opt)
end
