structure TypeCheck : sig
	    
  val typeof : TypeEnv.env * Sugary.term -> Type.typ
	    
end = struct

  structure S = Sugary
  structure T = Type

  fun typeof (env, S.Nat n) = T.Nat
    | typeof (env, S.True) = T.Bool
    | typeof (env, S.False) = T.Bool
    | typeof (env, S.Unit) = T.Unit
    | typeof (env, S.Add(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Nat
                              else raise Fail "Program not well-typed"
    | typeof (env, S.Subtract(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Nat
                              else raise Fail "Program not well-typed"
    | typeof (env, S.Mul(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Nat
                              else raise Fail "Program not well-typed"
    | typeof (env, S.Pow(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Nat
                              else raise Fail "Program not well-typed"
    | typeof (env, S.Less(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (env, S.Greater(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (env, S.LessEq(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (env, S.GreaterEq(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (env, S.Not t) = if (typeof(env, t) = T.Bool) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (env, S.And(t1,t2)) = if (typeof(env, t1) = T.Bool andalso typeof(env, t2) = T.Bool) then T.Bool
                                else raise Fail "Program not well-typed"
    | typeof (env, S.Or(t1,t2)) = if (typeof(env, t1) = T.Bool andalso typeof(env, t2) = T.Bool) then T.Bool
                                else raise Fail "Program not well-typed"
    | typeof (env, S.Xor(t1,t2)) = if (typeof(env, t1) = T.Bool andalso typeof(env, t2) = T.Bool) then T.Bool
                                else raise Fail "Program not well-typed"
    | typeof (env, S.Cond(t1,t2,t3)) = if ((typeof (env,t1) = T.Bool) andalso (typeof (env,t2) = typeof (env,t3))) then typeof (env,t2)
                              else raise Fail "Program not well-typed"
    | typeof (env, S.Eq(t1,t2)) = if (typeof(env,t1) = T.Nat andalso typeof(env,t2) = T.Nat) then T.Bool
                          else raise Fail "Program not well-typed" 
    | typeof (env, S.Pair(t1,t2)) = T.Product(typeof (env,t1), typeof (env,t2))
    | typeof (env, S.First(t)) = (case t of
                                    S.Pair(t1,t2) => typeof (env,t1)
                                  | _ => raise Fail "Program not well-typed")
    | typeof (env, S.Second(t)) = (case t of
                                    S.Pair(t1,t2) => typeof (env,t2)
                                  | _ => raise Fail "Program not well-typed")
    | typeof (env, S.Var s) = (case TypeEnv.lookup(env, s) of
                                SOME t => t
                              | NONE => raise Fail "Undefined variable")
    | typeof(env, S.Let (s, t1, t2)) = (case typeof (env, t1) of
                                         t1' => (case typeof (TypeEnv.extend(env, s, t1'), t2) of
                                                  t2' => t2')
                                        )

end
