structure Subst : sig

  val subst : string * L23RR.term * L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR

(* note: We will do without VarSet/FV this time. *)
(* This is because free variables are effectively banished by the typechecker. *)
(* That is, we shouldn't have them when we get to evaluation. *)
		  
  fun subst (x, s, t) = 
    (case t of
        L.Int n => L.Int n
      | L.True => L.True
      | L.False => L.False
      | L.Unit => L.Unit
      | L.Var y => if x = y then s else t
      | L.App (t1, t2) => L.App(subst (x, s, t1), subst (x, s, t2))
      | L.Lam (y, typ, t1) => 
          if x = y then L.Lam (y, typ, t1)
          else L.Lam (y, typ, subst (x, s, t1))
      | L.Fix y => L.Fix (subst (x, s, y))
      | L.Let (y, t1, t2) => if x = y then L.Let(y, subst(x,s,t1), t2) 
                              else L.Let(y, subst(x,s,t1), subst(x,s,t2)) 
      | L.Cond (t1,t2,t3) => L.Cond(subst(x, s, t1), subst(x, s, t2), subst(x, s, t3))
      | L.Add (t1, t2) => L.Add (subst(x,s,t1), subst(x,s,t2))
      | L.Sub (t1, t2) => L.Sub (subst(x,s,t1), subst(x,s,t2))
      | L.Mul (t1, t2) => L.Mul (subst(x,s,t1), subst(x,s,t2))
      | L.Eq (t1, t2) => L.Eq (subst(x,s,t1), subst(x,s,t2))
      | L.LessThan (t1, t2) => L.LessThan (subst(x,s,t1), subst(x,s,t2))
      | L.Not t1 => L.Not (subst(x,s,t1))
      | L.Record ls =>
          let
            fun subRec [] = []
              | subRec ((s1,t1) :: rs) = (s1, subst(x,s,t1)) :: subRec rs
          in
            L.Record (subRec ls)
          end
      | L.Select (s1, r) => L.Select(s1, subst(x,s,r))
    )

end
