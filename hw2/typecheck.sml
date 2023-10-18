structure TypeCheck : sig

  val typeof : Sugary.term -> Type.typ

end = struct

  structure S = Sugary
  structure T = Type
  
  fun typeof (S.Nat n) = T.Nat
    | typeof (S.True) = T.Bool
    | typeof (S.False) = T.Bool
    | typeof (S.Unit) = T.Unit
    | typeof (S.Add(t1,t2)) = if (typeof(t1) = T.Nat andalso typeof(t2) = T.Nat) then T.Nat
                              else raise Fail "Program not well-typed"
    | typeof (S.Subtract(t1,t2)) = if (typeof(t1) = T.Nat andalso typeof(t2) = T.Nat) then T.Nat
                                    else raise Fail "Program not well-typed"
    | typeof (S.Less(t1,t2)) = if (typeof(t1) = T.Nat andalso typeof(t2) = T.Nat) then T.Bool
                                else raise Fail "Program not well-typed"
    | typeof (S.Greater(t1,t2)) = if (typeof(t1) = T.Nat andalso typeof(t2) = T.Nat) then T.Bool
                            else raise Fail "Program not well-typed"
    | typeof (S.LessEq(t1,t2)) = if (typeof(t1) = T.Nat andalso typeof(t2) = T.Nat) then T.Bool
                            else raise Fail "Program not well-typed"
    | typeof (S.GreaterEq(t1,t2)) = if (typeof(t1) = T.Nat andalso typeof(t2) = T.Nat) then T.Bool
                                else raise Fail "Program not well-typed"
    | typeof (S.Not(t)) = (case typeof t of
                            T.Bool => T.Bool
                            | _ => raise Fail "Program not well-typed")
    | typeof (S.And(t1,t2)) = if (typeof(t1) = T.Bool andalso typeof(t2) = T.Bool) then T.Bool
                                else raise Fail "Program not well-typed"
    | typeof (S.Or(t1,t2)) = if (typeof(t1) = T.Bool andalso typeof(t2) = T.Bool) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (S.Xor(t1,t2)) = if (typeof(t1) = T.Bool andalso typeof(t2) = T.Bool) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (S.Cond(t1,t2,t3)) = if ((typeof t1 = T.Bool) andalso (typeof t2 = typeof t3)) then typeof t2
                                  else raise Fail "Program not well-typed"
    | typeof (S.Eq(t1,t2)) = if (typeof t1 = typeof t2) then T.Bool
                              else raise Fail "Program not well-typed"
    | typeof (S.Pair(t1,t2)) = T.Product(typeof t1, typeof t2)
    | typeof (S.First(t)) = (case t of
                              S.Pair(t1,t2) => typeof t1
                              | _ => raise Fail "Program not well-typed")
    | typeof (S.Second(t)) = (case t of
                              S.Pair(t1,t2) => typeof t2
                              | _ => raise Fail "Program not well-typed")

end
