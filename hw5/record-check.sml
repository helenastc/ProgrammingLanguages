structure RecordCheck : sig

(* check for pairwise distinct labels at all levels of record expressions and record types *)
(* also, reject empty records if you encounter them *)

(* raise an exception if the term doesn't pass the check *)

(* otherwise, return the term as is *)

  val check : L23RR.term -> L23RR.term
	    
end = struct

  structure L = L23RR
  structure T = Type

  fun repRec _ [] = false
    | repRec str ((s,t)::rs) = if str = s then true else (repRec str rs)

  fun checkType (T.Record ls) =  
        let
          fun checkRec [] = []
            | checkRec ((s,t)::rs) = if repRec s rs then raise Fail "Repeated record type"
                                    else (s, checkType t)::checkRec rs
        in
          (case ls of 
             [] => raise Fail "Empty record type"
            | _ => T.Record (checkRec ls))
        end
    | checkType t = t

  fun check (L.Lam (str, t, term)) = L.Lam (str, checkType t, check term)
    | check (L.App (t1, t2)) = L.App (check t1, check t2)
    | check (L.Fix t) = L.Fix (check t)
    | check (L.Let (s,t1,t2)) = L.Let(s, check t1, check t2)
    | check (L.Cond (t1,t2,t3)) = L.Cond(check t1, check t2, check t3)
    | check (L.Add(t1,t2)) = L.Add(check t1, check t2)
    | check (L.Sub(t1,t2)) = L.Sub(check t1, check t2)
    | check (L.Mul(t1,t2)) = L.Mul(check t1, check t2)
    | check (L.Eq(t1,t2)) = L.Eq(check t1, check t2)
    | check (L.LessThan(t1,t2)) = L.LessThan(check t1, check t2)
    | check (L.Not t) = L.Not(check t)
    | check (L.Record ls) = 
        let
          fun checkRec [] = []
            | checkRec ((s,t)::rs) = if repRec s rs then raise Fail "Repeated record"
                                    else (s, check t)::checkRec rs
        in
          (case ls of 
             [] => raise Fail "Empty record"
            | _ => L.Record (checkRec ls))
        end
    | check (L.Select (str, recs)) = L.Select (str, check recs)
    | check t = t

end
