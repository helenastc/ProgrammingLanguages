structure CBV : sig

  val step : ULC.term -> ULC.term option

end = struct

  structure U = ULC

  fun step (U.Var s) = NONE
    | step (U.App (t1, t2)) = (case (t1,t2) of
                                (U.Lam (x,t), U.Lam _) => SOME(Subst.subst(x, t2, t))
                              | (U.Lam _, _) => (case step t2 of
                                                    SOME(t2') => SOME(U.App(t1, t2'))
                                                  | NONE => NONE)
                              | _ => (case step t1 of
                                        SOME (t1') => SOME(U.App(t1', t2))
                                      | NONE => NONE)
                              )                   
    | step (U.Lam (s, t)) = NONE

end
