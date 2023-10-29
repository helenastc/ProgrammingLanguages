structure Unroll : sig

  val unroll : Sweetl.prog -> Sweetl.term

end = struct

  structure S = Sweetl

  fun replace (S.Prog (abbs, S.Var str)) = S.Var str
    | replace (S.Prog (abbs, S.Nat n)) = S.Nat n
    | replace (S.Prog (abbs, S.Tru)) = S.Tru
    | replace (S.Prog (abbs, S.Fls)) = S.Fls
    | replace (S.Prog (abbs, S.ID str)) = S.ID str 
    | replace (S.Prog (abbs, S.Abbr str)) = (case (List.find (fn (abbName, _) => abbName = str) abbs) of
                                        SOME (_, term) => replace (S.Prog (abbs, term))
                                      | NONE => raise Fail "Couldn't find abbreviation")
    | replace (S.Prog (abbs, S.Lam (str, t))) = S.Lam (str, replace (S.Prog (abbs, t)))
    | replace (S.Prog (abbs, S.App (t1, t2))) = S.App (replace (S.Prog (abbs, t1)), replace (S.Prog (abbs, t2)))


  fun validList [] = []
    | validList ((str, t) :: rest) = (str, replace (S.Prog (rest, t))) :: validList rest

  fun unroll (S.Prog (abbs, mainT)) = 
    replace (S.Prog (validList (rev abbs), mainT))


end
