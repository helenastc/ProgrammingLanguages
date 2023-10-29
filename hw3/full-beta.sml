structure FullBeta : sig

  val step : ULC.term -> ULC.term option

end = struct

  structure U = ULC

  fun step (U.App (t1,t2)) = (case (t1,t2) of
                                (U.Lam (x,t), _) => SOME(Subst.subst(x, t2, t))
                              | _ => (case step t1 of 
                                        SOME(t1') => SOME(U.App(t1', t2))
                                      | NONE => (case step t2 of
                                                  SOME(t2') => SOME(U.App(t1, t2'))
                                                | NONE => NONE)))
    | step (U.Lam (x,t1)) = (case step t1 of
                              SOME(t1') => SOME(U.Lam(x, t1'))
                            | NONE => NONE)
    | step _ = NONE 

end
