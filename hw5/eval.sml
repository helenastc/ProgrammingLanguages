structure Eval : sig

  val eval : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR
  structure S = Subst

  fun isV L.True = true
    | isV L.False = true
    | isV L.Unit = true
    | isV (L.Int n) = true
    | isV (L.Lam (s, tau, t)) = true
    | isV (L.Record ls) = true
    | isV _ = false

  fun eval (L.Int n) = L.Int n
    | eval L.True = L.True
    | eval L.False = L.False
    | eval L.Unit = L.Unit
    | eval (L.Var s) = raise Fail "Can't evaluate, extra var"
    | eval (L.Lam (s, tau, t)) = L.Lam (s, tau, t)
    | eval (L.App (t1, t2)) = (case eval t1 of 
                                  L.Lam(x,tau,t1') => if isV (eval t2) then eval(S.subst(x, eval t2, t1'))
                                                  else raise Fail "Can't evaluate app"
                                | _ => raise Fail "Can't evaluate app")
		| eval (L.Fix t) = (case eval t of
                          L.Lam(f,tau,t1') => (case eval (S.subst(f, L.Fix(L.Lam(f,tau,t1')), t1')) of
                                                v => if isV v then v
                                                      else raise Fail "Can't evaluate fix"))
    | eval (L.Let (x, t1, t2)) = (case eval t1 of
                                    v => if isV v then (case eval(S.subst(x, v, t2)) of
                                                          v2 => if isV v2 then v2
                                                                else raise Fail "Can't evaluate let")
                                          else raise Fail "Can't evaluate let")
    | eval (L.Cond (t1, t2, t3)) = (case eval t1 of
                                      L.True => if isV (eval t2) then (eval t2)
                                                else raise Fail "Can't evaluate cond"
                                    | L.False => if isV (eval t3) then (eval t3)
                                                else raise Fail "Can't evaluate cond"
                                    | _ => raise Fail "Can't evaluate cond")
    | eval (L.Add (t1, t2)) = (case (eval t1, eval t2) of
                                (L.Int n1, L.Int n2) => L.Int(n1 + n2)
                              | _ => raise Fail "Can't evaluate add")
    | eval (L.Sub (t1, t2)) = (case (eval t1, eval t2) of
                                (L.Int n1, L.Int n2) => L.Int(n1 - n2)
                              | _ => raise Fail "Can't evaluate sub")
    | eval (L.Mul (t1, t2)) = (case (eval t1, eval t2) of
                                (L.Int n1, L.Int n2) => L.Int(n1 * n2)
                              | _ => raise Fail "Can't evaluate mul")
    | eval (L.Eq (t1, t2)) = (case (eval t1, eval t2) of
                                (L.Int n1, L.Int n2) => if n1 = n2 then L.True else L.False
                              | _ => raise Fail "Can't evaluate eq")
    | eval (L.LessThan (t1,t2)) = (case (eval t1, eval t2) of
                                    (L.Int n1, L.Int n2) => if n1 < n2 then L.True else L.False
                                   | _ => raise Fail "Can't evaluate less than")      
    | eval (L.Not t) = (case eval t of
                          L.True => L.False
                        | L.False => L.True
                        | _ => raise Fail "Can't evaluate not")      
    | eval (L.Record ls) =
        let
          fun evalRec [] = []
            | evalRec ((s,t)::rest) = if isV (eval t) then (s, eval t)::evalRec rest
                                    else raise Fail "Can't evaluate record"
        in
          L.Record(evalRec ls)
        end
    | eval (L.Select(s,t)) = (case eval t of
                                L.Record ls => (case (List.find (fn (str, _) => str = s) ls) of
                                                                  SOME (s, v1) => v1
                                                                | NONE => raise Fail "Record doesn't exist")
                              | _ => raise Fail "Can't evaluate select")


end
