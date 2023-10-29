structure VarSet :> sig

  type set

  val empty : set
  val mem   : string * set -> bool
  val ins   : string * set -> set
  val rem   : string * set -> set
  val union : set * set -> set

end = struct

  type set = string list (* <== Change this to something else! *)

  val empty = []  (* <== Change this to something consistent with the new set type. *)

  fun mem (_, []) = false
    | mem (str, (x::xs)) = if str = x then true
                            else mem (str, xs)

  fun ins (str, ls) = str :: ls

  fun rem (_, []) = []
    | rem (str, (x::xs)) = if str = x then xs
                          else x :: rem (str, xs)
  
  fun union ([], ls) = ls
    | union (ls, []) = ls
    | union ((x::xs), ls) = if mem (x,ls) then union(xs, ls)
                            else union(xs, ins(x, ls))
				      
end
