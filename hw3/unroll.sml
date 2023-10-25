structure Unroll : sig

  val unroll : Sweetl.prog -> Sweetl.term

end = struct

  structure S = Sweetl

  fun unroll (S.Prog (abbs, S.Var str)) = S.Var str
    | unroll (S.Prog (abbs, S.Nat n)) = S.Nat n
    | unroll (S.Prog (abbs, S.Tru)) = S.Tru
    | unroll (S.Prog (abbs, S.Fls)) = S.Fls
    | unroll (S.Prog (abbs, S.ID str)) = S.ID str 
    | unroll (S.Prog (abbs, S.Abbr str)) = (case (List.find (fn (abbName, _) => abbName = str) abbs) of
                                        SOME (_, term) => unroll (S.Prog (abbs, term))
                                      | NONE => raise Fail "Couldn't find abbreviation in main term")
    | unroll (S.Prog (abbs, S.Lam (str, t))) = S.Lam (str, unroll (S.Prog (abbs, t)))
    | unroll (S.Prog (abbs, S.App (t1, t2))) = S.App (unroll (S.Prog (abbs, t1)), unroll (S.Prog (abbs, t2)))

end
