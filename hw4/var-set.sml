structure VarSet :> sig

  type set

  val empty  : set
  val mem    : string * set -> bool
  val ins    : string * set -> set
  val rem    : string * set -> set
  val union  : set * set -> set

(* the following operations aren't necessary for substitution *)
(* it just seemed sad not to have them for sets *)
(* they also make testing much easier *)

  val sing   : string -> set (* singleton set *)
  val size   : set -> int
  val subset : set * set -> bool
  val equals : set * set -> bool
			      
  val toList : set -> string list

end = struct

  type set = string list
  val empty : string list = [] 

  fun mem (x, []) = false
    | mem (x, y::ys) = x=y orelse mem (x, ys)

  fun ins (x, s) = if mem (x, s) then s else x::s

  fun rem (x, []) = []
    | rem (x, y::ys) = if x=y then ys else y::(rem (x, ys))

  fun union ([], set2) = set2
    | union (x::xs, set2) = union (xs, ins (x, set2))

  fun sing x = [x]

  fun size [] =  0
    | size (x::xs) = 1 + size xs
                    
  fun subset (x, []) = true
    | subset (x, y::ys) = if not (mem (y,x)) then false
                          else subset (x, ys)

  fun equals ([], []) = true
    | equals ([], _) = false
    | equals (_, []) = false
    | equals (x::xs, y::ys) = if x <> y then false
                              else equals(xs, ys)

  fun toList [] = []
    | toList (x :: xs) = x :: toList xs
      				  
end
