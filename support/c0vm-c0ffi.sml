(* Automatically generated by bin/wrappergen
 * Do not edit this file directly! *)

signature C0VM_NATIVE = 
sig
   val native_index : string -> int
end

structure C0VMNative :> C0VM_NATIVE = 
struct

fun native_index("") =                      ~1

(* 15411 *)
  | native_index("fadd") =                  0
  | native_index("fdiv") =                  1
  | native_index("fless") =                 2
  | native_index("fmul") =                  3
  | native_index("fsub") =                  4
  | native_index("ftoi") =                  5
  | native_index("itof") =                  6
  | native_index("print_fpt") =             7
  | native_index("print_hex") =             8
  | native_index("print_int") =             9

(* args *)
  | native_index("args_flag") =             10
  | native_index("args_int") =              11
  | native_index("args_parse") =            12
  | native_index("args_string") =           13

(* conio *)
  | native_index("eof") =                   14
  | native_index("flush") =                 15
  | native_index("print") =                 16
  | native_index("printbool") =             17
  | native_index("printchar") =             18
  | native_index("printint") =              19
  | native_index("println") =               20
  | native_index("readline") =              21

(* curses *)
  | native_index("c_addch") =               22
  | native_index("c_cbreak") =              23
  | native_index("c_curs_set") =            24
  | native_index("c_delch") =               25
  | native_index("c_endwin") =              26
  | native_index("c_erase") =               27
  | native_index("c_getch") =               28
  | native_index("c_initscr") =             29
  | native_index("c_keypad") =              30
  | native_index("c_move") =                31
  | native_index("c_noecho") =              32
  | native_index("c_refresh") =             33
  | native_index("c_subwin") =              34
  | native_index("c_waddch") =              35
  | native_index("c_waddstr") =             36
  | native_index("c_wclear") =              37
  | native_index("c_werase") =              38
  | native_index("c_wmove") =               39
  | native_index("c_wrefresh") =            40
  | native_index("c_wstandend") =           41
  | native_index("c_wstandout") =           42
  | native_index("cc_getbegx") =            43
  | native_index("cc_getbegy") =            44
  | native_index("cc_getmaxx") =            45
  | native_index("cc_getmaxy") =            46
  | native_index("cc_getx") =               47
  | native_index("cc_gety") =               48
  | native_index("cc_highlight") =          49
  | native_index("cc_key_is_backspace") =   50
  | native_index("cc_key_is_down") =        51
  | native_index("cc_key_is_enter") =       52
  | native_index("cc_key_is_left") =        53
  | native_index("cc_key_is_right") =       54
  | native_index("cc_key_is_up") =          55
  | native_index("cc_wboldoff") =           56
  | native_index("cc_wboldon") =            57
  | native_index("cc_wdimoff") =            58
  | native_index("cc_wdimon") =             59
  | native_index("cc_wreverseoff") =        60
  | native_index("cc_wreverseon") =         61
  | native_index("cc_wunderoff") =          62
  | native_index("cc_wunderon") =           63

(* file *)
  | native_index("file_close") =            64
  | native_index("file_closed") =           65
  | native_index("file_eof") =              66
  | native_index("file_read") =             67
  | native_index("file_readline") =         68

(* img *)
  | native_index("image_clone") =           69
  | native_index("image_create") =          70
  | native_index("image_data") =            71
  | native_index("image_destroy") =         72
  | native_index("image_height") =          73
  | native_index("image_load") =            74
  | native_index("image_save") =            75
  | native_index("image_subimage") =        76
  | native_index("image_width") =           77

(* parse *)
  | native_index("parse_bool") =            78
  | native_index("parse_int") =             79

(* string *)
  | native_index("char_chr") =              80
  | native_index("char_ord") =              81
  | native_index("string_charat") =         82
  | native_index("string_compare") =        83
  | native_index("string_equal") =          84
  | native_index("string_from_chararray") = 85
  | native_index("string_frombool") =       86
  | native_index("string_fromchar") =       87
  | native_index("string_fromint") =        88
  | native_index("string_join") =           89
  | native_index("string_length") =         90
  | native_index("string_sub") =            91
  | native_index("string_terminated") =     92
  | native_index("string_to_chararray") =   93
  | native_index("string_tolower") =        94

(* unknown *)
  | native_index(s) =                       ~1

end
