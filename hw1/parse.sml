structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST
  
  fun next [] = NONE
    | next(T.Z :: toks) = SOME(A.Zero,toks)
    | next(T.T :: toks) = SOME(A.True,toks)
    | next(T.F :: toks) = SOME(A.False,toks) 
    | next(T.S :: toks) = (case next toks
                            of SOME(Aterm,toks') => SOME(A.Succ(Aterm), toks')
                            | _ => raise Fail "No argument after succ")
    | next(T.P :: toks) = (case next toks
                            of SOME(Aterm,toks') => SOME(A.Pred(Aterm), toks')
                            | _ => raise Fail "No argument after pred")
    | next(T.LBrack :: toks) = 
      (case next toks
        of SOME(t1,T.Plus :: toks') => (case next toks'
                                        of SOME(t2, T.RBrack :: toks'') => SOME(A.Add(t1,t2), toks'')
                                        | _ => raise Fail "Invalid addition, missing arg or bracket")
        | SOME(t1,T.Minus :: toks') => (case next toks'
                                        of SOME(t2, T.RBrack :: toks'') => SOME(A.Subtract(t1,t2), toks'')
                                        | _ => raise Fail "Invalid subtraction, missing arg or bracket")
        | SOME(t1,T.LessThan :: toks') => (case next toks'
                                        of SOME(t2, T.RBrack :: toks'') => SOME(A.Less(t1,t2), toks'')
                                        | _ => raise Fail "Invalid comparison, missing arg or bracket")
        | SOME(t1,T.GreaterThan :: toks') => (case next toks'
                                        of SOME(t2, T.RBrack :: toks'') => SOME(A.Greater(t1,t2), toks'')
                                        | _ => raise Fail "Invalid comparison, missing arg or bracket")
        | SOME(t1,T.DoubleAmpersand :: toks') => (case next toks'
                                        of SOME(t2, T.RBrack :: toks'') => SOME(A.And(t1,t2), toks'')
                                        | _ => raise Fail "Invalid &&, missing arg or bracket")                                                                     
        | SOME(t1,T.DoublePipe :: toks') => (case next toks'
                                        of SOME(t2, T.RBrack :: toks'') => SOME(A.Or(t1,t2), toks'')
                                        | _ => raise Fail "Invalid ||, missing arg or bracket")
        | SOME(t1,T.QuestionMark :: toks') => (case next toks'
                                                of SOME(t2, T.Colon :: toks'')
                                                 => (case next toks'' 
                                                      of SOME(t3, T.RBrack :: ltoks)=> SOME(A.Cond(t1,t2,t3), ltoks)
                                                      | _ => raise Fail "Missing third conditional term or bracket")   
                                                | _ => raise Fail "Missing second conditional term or :")
        | _ => raise Fail "Incomplete expression in brackets")
    | next _ = raise Fail "todo"

  fun parse toks =
    (case next toks
      of SOME(ast, []) => ast
      | SOME(ast, _ ) => raise Fail "Did not finish parsing toks"
      | NONE => raise Fail "No ast term")


end
