structure TopLevelCommands =
struct
  datatype cmd = Step of Term.t option | Eval of Term.t option  
  datatype res = Next of Term.t  | Val of Term.t 
end
