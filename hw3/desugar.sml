structure Desugar : sig

  val desugar : Sweetl.term -> ULC.term
  val makeNat : int -> ULC.term

end = struct

  structure S = Sweetl
  structure U = ULC

  fun makeNat 0 = U.Var "z"
  | makeNat n = if (n > 0) then U.App (U.Var "s", makeNat(n-1))
                else raise Fail "Negative number not allowed"
                
  fun desugar (S.Var str) = (U.Var str)
    | desugar (S.Nat n) = U.Lam ("s", U.Lam ("z", makeNat n))
    | desugar (S.Tru) = U.Lam ("t", U.Lam ("f", U.Var "t"))
    | desugar (S.Fls) = U.Lam ("t", U.Lam ("f", U.Var "f"))
    | desugar (S.ID str) = U.Lam (str, U.Var str)
    | desugar (S.Lam (str, t)) = U.Lam (str, desugar t)
    | desugar (S.App (t1, t2)) = U.App (desugar t1, desugar t2)
    | desugar (S.Abbr _ ) = raise Fail "Undefined abbreviation" 


end
