signature PATTERN = 
sig

    datatype side = L | R                  

    datatype t = Wild | Var 
               | Z | S of t 
               | Triv | Pair of t * t | In of side * t 

    (* Counts the occurances of Var in the pattern *)
    val numvars : t -> int 
    val equals : t * t -> bool	
    val toString :  t -> string
end

