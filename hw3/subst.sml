structure Subst : sig

  val fv : ULC.term -> VarSet.set
  val subst : string * ULC.term * ULC.term -> ULC.term

end = struct
  
  structure U = ULC
  structure VS = VarSet

  fun fv (U.Var s) = VS.ins (s, VS.empty)
    | fv (U.App (t1,t2)) = VS.union(fv t1, fv t2)
    | fv (U.Lam (s, t)) = VS.rem (s, (fv t))
		  
  fun subst (x, t1, t2) = (case t2 of
                            U.Var s => if x = s then t1
                                        else t2
                          | U.App (a1, a2) => U.App (subst(x,t1,a1), subst(x,t1,a2))
                          | U.Lam (y, a1) => if x = y then t2
                                              else if VS.mem(y, fv t1) then 
                                                  let
                                                    val y' = Fresh.var()
                                                  in 
                                                    subst(x, t1, U.Lam(y', subst(y, U.Var y', a1)))
                                                  end
                                              else U.Lam (y, subst(x, t1, a1))
                          )

end
