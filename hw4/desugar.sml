structure Desugar : sig

  val desugar : Sugary.term -> ULC.term

end = struct

  structure S = Sugary
  structure U = ULC

  fun makeNat 0 = U.Var "z"
    | makeNat n = if (n > 0) then U.App (U.Var "s", makeNat(n-1))
                else raise Fail "Negative number not allowed"
  
  val cZero = U.Lam("s", U.Lam("z", U.Var "z"))
  val c1 = U.Lam("s", U.Lam("z", U.App(U.Var "s", U.Var "z")))
  val cTru : ULC.term = U.Lam ("t", U.Lam ("f", U.Var "t"))
  val cFls : ULC.term = U.Lam ("t", U.Lam ("f", U.Var "f"))
  val plus : ULC.term = U.Lam ("m", U.Lam ("n", U.Lam ("s", U.Lam ("z", U.App(U.App(U.Var "m", U.Var "s"), U.App(U.App(U.Var "n", U.Var "s"), U.Var "z"))))))
  val times = U.Lam("m", U.Lam ("n", U.App(U.App(U.Var "m", U.App(plus, U.Var "n")), cZero)))
  val pair = U.Lam("f",U.Lam("s",U.Lam("b", U.App(U.App(U.Var("b"),U.Var("f")),U.Var("s")))))
  val fst = U.Lam("p", U.App(U.Var "p", cTru))
  val snd = U.Lam("p", U.App(U.Var "p", cFls))
  val zz = U.App(U.App(pair, cZero), cZero)
  val ss = U.Lam("p", U.App(U.App(pair, U.App(snd, U.Var "p")), U.App(U.App(plus, c1), U.App(snd, U.Var "p"))))
  val prd = U.Lam("m", U.App(fst, U.App(U.App(U.Var "m", ss), zz)))
  val subtract = U.Lam("m", U.Lam("n", U.App(U.App(U.Var "n", prd), U.Var "m")))
  val power = U.Lam("m", U.Lam("n", U.App(U.App(U.Var "n", U.App(times, U.Var "m")), c1)))
  val an = U.Lam("b", U.Lam("c", U.App(U.App(U.Var "b", U.Var "c"), cFls)))
  val cOr = U.Lam("b", U.Lam("c", U.App(U.App(U.Var "b", cTru), U.Var "c")))
  val iszro = U.Lam("m", U.App(U.App(U.Var "m", U.Lam("x", cFls)), cTru))
  val equal = U.Lam("m", U.Lam("n", U.App(U.App(an, U.App(iszro, U.App(U.App(U.Var "m", prd), U.Var "n"))), U.App(iszro, U.App(U.App(U.Var "n", prd), U.Var "m")))))
  val cNot = U.Lam("b", U.App(U.App(U.Var "b", cFls), cTru))
  val xor = U.Lam("a", U.Lam("b", U.App(U.App(U.Var "a", U.App(cNot, U.Var "b")), U.Var "b")))
  val test = U.Lam("l", U.Lam("m", U.Lam("n", U.App(U.App(U.Var "l", U.Var "m"), U.Var "n"))))


  fun desugar (S.Nat n) = U.Lam ("s", U.Lam ("z", makeNat n))
    | desugar (S.True) = cTru
    | desugar (S.False) = cFls
    | desugar (S.Unit) = cZero
    | desugar (S.Add(t1,t2)) = U.App(U.App(plus, desugar t1), desugar t2)
    | desugar (S.Subtract(t1,t2)) = U.App(U.App (subtract, desugar t1), desugar t2)
    | desugar (S.Mul(t1,t2)) = U.App(U.App(times, desugar t1), desugar t2)
    | desugar (S.Pow(t1,t2)) = U.App(U.App(power, desugar t1), desugar t2)
    | desugar (S.Less(t1,t2)) = U.App(iszro, U.App(U.App (subtract, U.App(U.App(plus, c1), desugar t1)), desugar t2))
    | desugar (S.Greater(t1,t2)) = U.App(iszro, U.App(U.App (subtract, U.App(U.App(plus, c1), desugar t2)), desugar t1))
    | desugar (S.LessEq(t1, t2)) = U.App(iszro, U.App(U.App (subtract, desugar t1), desugar t2))
    | desugar (S.GreaterEq(t1,t2)) = U.App(iszro, U.App(U.App (subtract, desugar t2), desugar t1))
    | desugar (S.Not t) = U.App(cNot, desugar t)
    | desugar (S.And(t1,t2)) = U.App(U.App(an, desugar t1), desugar t2)
    | desugar (S.Or(t1,t2)) = U.App(U.App(cOr, desugar t1), desugar t2)
    | desugar (S.Xor(t1,t2)) = U.App(U.App(xor, desugar t1), desugar t2)
    | desugar (S.Cond(t1, t2, t3)) = U.App(U.App(U.App(test, desugar t1), desugar t2), desugar t3)
    | desugar (S.Eq(t1, t2)) = U.App(U.App(equal, desugar t1), desugar t2)
    | desugar (S.Pair(t1,t2)) = U.App(U.App(pair, desugar t1), desugar t2)
    | desugar (S.First t) = U.App(fst, desugar t)
    | desugar (S.Second t) = U.App(snd, desugar t)
    | desugar (S.Var str) = (U.Var str)
    | desugar (S.Let(s, t1, t2)) = U.App(U.Lam(s, desugar t2), desugar t1)

end

