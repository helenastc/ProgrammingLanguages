structure Lazy : sig

  val step : ULC.term -> ULC.term option

end = struct

  structure U = ULC
		  
  fun step (U.App (t1, t2)) = (case (t1,t2) of
                                (U.Lam (x,t), _) => SOME(Subst.subst(x, t2, t))
                              | _ => (case step t1 of
                                        SOME(t1') => SOME(U.App(t1',t2))
                                      | NONE => NONE))
    | step _ = NONE	   

end
