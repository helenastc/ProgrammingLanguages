structure Eval : sig

  val isNV : Desugared.term -> bool
  val isV  : Desugared.term -> bool
  val step : Desugared.term -> Desugared.term option
  val eval : Desugared.term -> Desugared.term list

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term	    

  val result : Desugared.term -> norm
      
end = struct

  datatype norm
    = Value of Desugared.term
    | Stuck of Desugared.term

  structure D = Desugared

  fun isNV term = (case term of
      D.Zero => true
    | D.Succ(t) => isNV t
    | _ => false)

  fun isV term = if isNV term then true
                 else (case term of
                        D.Pair(v1, v2) => if (isV v1 andalso isV v2) then true
                                    else false
                        | _ => false )

		 
  fun step (D.Zero) = NONE
    | step (D.Succ(t)) = (case step t of
                            SOME(t1) => SOME(D.Succ(t1))
                          | NONE => NONE)    
    | step (D.Add(t1,t2)) = if isNV t1 then
                              (case t1 of
                                D.Zero => SOME(t2)
                              | D.Succ(t1') => SOME(D.Add(t1', D.Succ(t2)))
                              | _ => raise Fail "isNV error in addition")
                            else (case step t1 of
                                  SOME(t1') => SOME(D.Add(t1',t2))
                                  | NONE => NONE)
    | step (D.Subtract(t1,t2)) = if (isV t1 andalso isV t2) then
                                  (case (t1,t2) of
                                    (D.Zero, _ ) => SOME(D.Zero)
                                  | ( _ , D.Zero) => SOME(t1)
                                  | (D.Succ(t1'), D.Succ(t2')) => SOME(D.Subtract(t1',t2'))
                                  | _ => raise Fail "isV error in subtraction")
                                else if isV t1 then
                                  (case step t2 of
                                      SOME(t2') => SOME(D.Subtract(t1,t2'))
                                    | NONE => NONE)
                                else 
                                  (case step t1 of 
                                      SOME(t1') => SOME(D.Subtract(t1',t2))
                                    | NONE => NONE)
    | step (D.Less(t1,t2)) = if (isV t1 andalso isV t2) then
                              (case (t1,t2) of
                                (D.Zero, D.Zero) => SOME(D.Zero)
                              | (D.Zero, D.Succ(t2')) => SOME(D.Succ D.Zero)
                              | (_, D.Zero) => SOME(D.Zero)
                              | (D.Succ(t1'), D.Succ(t2')) => SOME(D.Less(t1',t2'))
                              | _ => raise Fail "something wrong with <")
                            else if isV t1 then
                              (case step t2 of
                                  SOME(t2') => SOME(D.Less(t1,t2'))
                                | NONE => NONE)
                            else
                              (case step t1 of 
                                  SOME(t1') => SOME(D.Less(t1',t2))
                                | NONE => NONE)
    | step (D.Eq(t1,t2)) = if (isV t1 andalso isV t2) then
                            (case (t1,t2) of
                                (D.Zero, D.Zero) => SOME(D.Succ D.Zero)
                              | (D.Zero, D.Succ(t2')) => SOME(D.Zero)
                              | (D.Succ(t1'), D.Zero) => SOME(D.Zero)
                              | (D.Succ(t1'), D.Succ(t2')) => SOME(D.Eq(t1',t2'))
                              | (D.Pair(v1, v2), D.Pair(v3, v4)) => SOME(D.Cond(D.Eq(v1,v3), D.Eq(v2,v4), D.Zero))
                              | _ => raise Fail "something wrong with ==")
                          else if isV t1 then
                            (case step t2 of
                                SOME(t2') => SOME(D.Eq(t1,t2'))
                              | NONE => NONE)
                          else 
                            (case step t1 of
                                SOME(t1') => SOME(D.Eq(t1', t2))
                              | NONE => NONE)    
    | step (D.Cond(t1,t2,t3)) = if isV t1 then
                                  (case t1 of
                                    D.Succ(D.Zero) => SOME(t2)
                                  | D.Zero => SOME(t3)
                                  | _ => raise Fail "error identifying TF in cond")      
                                else (case step t1 of
                                        SOME(t1') => SOME(D.Cond(t1',t2,t3))
                                      | NONE => NONE)
		| step (D.Pair(t1,t2)) = if isV t1 then
                              (case step t2 of 
                                  SOME(t2') => SOME(D.Pair(t1, t2'))
                                | NONE => NONE)
                            else 
                              (case step t1 of
                                  SOME(t1') => SOME(D.Pair(t1',t2))
                                | NONE => NONE)
    | step (D.First t) = (case t of 
                            D.Pair(v1, v2) => SOME(v1)
                          | _ => (case step t of
                                    SOME(t') => SOME(D.First t')
                                  | NONE => NONE)) 
    | step (D.Second t) = (case t of 
                            D.Pair(v1, v2) => SOME(v2)
                          | _ => (case step t of
                                    SOME(t') => SOME(D.Second t')
                                  | NONE => NONE)) 

  fun eval t =
    let
      fun lp t =
	(case step t
	   of SOME t' => t :: lp t'
	    | NONE => [t])
    in
      lp t
    end		    

  fun result term = 
    let val t = eval term
    in
      if isV (List.last t) then Value(List.last t)
      else Stuck(List.last t)
    end

end
