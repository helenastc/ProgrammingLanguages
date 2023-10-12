structure Eval : sig

  val isV  : AST.term -> bool
  val isNV : AST.term -> bool
  val step : AST.term -> AST.term option
  val eval : AST.term -> AST.term list
				  
end = struct

  structure A = AST

  fun isNV ast = (case ast of
    A.Zero => true
    | A.Succ(term) => isNV term
    | _ => false)

  fun isV ast = if isNV ast then true
    else (case ast
    of A.True => true
    | A.False => true
    | _ => false)
		 
  fun step (A.Zero) = NONE
    | step (A.True) = NONE
    | step (A.False) = NONE
    | step (A.Pred(A.Zero)) = SOME(A.Zero)
    | step (A.Pred(A.Succ(t))) = SOME(t)
    | step (A.Pred(t)) = (case t of
                            A.Zero => SOME(A.Zero)
                          | A.Succ(t) => SOME(t)
                          | _ => (case step t of
                                    SOME(t1) => SOME(A.Pred(t1))
                                  | NONE => NONE))
    | step (A.Succ(t)) = (case step t of
                            SOME(t1) => SOME(A.Succ(t1))
                          | NONE => NONE)      
    | step (A.Add(t1,t2)) = if isNV t1 then
                              (case t1 of
                                A.Zero => SOME(t2)
                              | A.Succ(t1') => SOME(A.Add(t1', A.Succ(t2)))
                              | _ => raise Fail "isNV error in addition")
                            else (case step t1 of
                                  SOME(t1') => SOME(A.Add(t1',t2))
                                  | NONE => NONE)
    | step (A.Subtract(t1,t2)) = if (isNV t1 andalso isNV t2) then
                                  (case (t1,t2) of
                                    (A.Zero, _ ) => SOME(A.Zero)
                                  | ( _ , A.Zero) => SOME(t1)
                                  | (A.Succ(t1'), A.Succ(t2')) => SOME(A.Subtract(t1',t2'))
                                  | _ => raise Fail "isNV error in subtraction")
                                else if isV t1 then
                                  (case step t2 of
                                      SOME(t2') => SOME(A.Subtract(t1,t2'))
                                    | NONE => NONE)
                                else 
                                  (case step t1 of 
                                      SOME(t1') => SOME(A.Subtract(t1',t2))
                                    | NONE => NONE)
    | step (A.Less(t1,t2)) = if (isNV t1 andalso isNV t2) then
                              (case (t1,t2) of
                                (A.Zero, A.Zero) => SOME(A.False)
                              | (A.Zero, A.Succ(t2')) => SOME(A.True)
                              | (_, A.Zero) => SOME(A.False)
                              | (A.Succ(t1'), A.Succ(t2')) => SOME(A.Less(t1',t2'))
                              | _ => raise Fail "something wrong with <")
                            else if isV t1 then
                              (case step t2 of
                                  SOME(t2') => SOME(A.Less(t1,t2'))
                                | NONE => NONE)
                            else
                              (case step t1 of 
                                  SOME(t1') => SOME(A.Less(t1',t2))
                                | NONE => NONE)
    | step (A.Greater(t1,t2)) = if (isNV t1 andalso isNV t2) then
                              (case (t1,t2) of
                                (A.Zero, _ ) => SOME(A.False)
                              | (A.Succ(t1'), A.Zero) => SOME(A.True)
                              | (A.Succ(t1'), A.Succ(t2')) => SOME(A.Greater(t1',t2'))
                              | _ => raise Fail "something wrong with >")
                            else if isV t1 then
                              (case step t2 of
                                  SOME(t2') => SOME(A.Greater(t1,t2'))
                                | NONE => NONE)
                            else
                              (case step t1 of 
                                  SOME(t1') => SOME(A.Greater(t1',t2))
                                | NONE => NONE)
    | step (A.And(t1,t2)) = if (isV t1 andalso (not (isNV t1))) then
                              (case t1 of
                                A.True => SOME(t2)
                              | A.False => SOME(A.False)
                              | _ => raise Fail "error identifying TF in &&")
                            else (case step t1 of
                                    SOME(t1') => SOME(A.And(t1', t2))
                                  | NONE => NONE)
    | step (A.Or(t1,t2)) = if (isV t1 andalso (not (isNV t1))) then
                              (case t1 of
                                A.True => SOME(A.True)
                              | A.False => SOME(t2)
                              | _ => raise Fail "error identifying TF in ||")
                            else (case step t1 of
                                    SOME(t1') => SOME(A.Or(t1', t2))
                                  | NONE => NONE)     
    | step (A.Cond(t1,t2,t3)) = if (isV t1 andalso (not (isNV t1))) then
                                  (case t1 of
                                    A.True => SOME(t2)
                                  | A.False => SOME(t3)
                                  | _ => raise Fail "error identifying TF in cond")      
                                else (case step t1 of
                                        SOME(t1') => SOME(A.Cond(t1',t2,t3))
                                      | NONE => NONE)

  fun eval terms =
    let
      fun lp ast =
          (case step ast of
            NONE => []
          | SOME(t) => t :: lp t)
    in 
      terms :: lp terms
    end
    
end
