structure TypeEnv :> sig

  type env

  val empty  : env
  val lookup : env * string -> Type.typ option
  val extend : env * string * Type.typ -> env
	    
end = struct

  type env = (string * Type.typ) list (* todo: replace this *)
	       
  val empty : (string * Type.typ) list = [] (* todo: replace this *)

  fun lookup ([], s) = NONE
    | lookup ((s,t)::e, str) = if s = str then SOME t
                                else lookup(e, str) 

  fun extend (env, s, t) = (s,t) :: env
					  
end
