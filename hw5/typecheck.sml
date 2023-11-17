 structure TypeCheck : sig

(* return true if the first type is a subtype of the second *)
  val subty : Type.typ * Type.typ -> bool

(* for commonSupertype, use the s function from the PDF *)
(* if there isn't a common supertype, return NONE *)
  val commonSupertype : Type.typ * Type.typ -> Type.typ option

  val typeHelper : TypeEnv.env * L23RR.term -> Type.typ 

  val typeof : L23RR.term -> Type.typ
							
end = struct

  structure L = L23RR
  structure T = Type
  structure E = TypeEnv

  (* check if every element in the first list exists in the second *)  
  fun recPerm ([], _) = true
    | recPerm ((s,t)::rest,ls) = if (List.exists (fn (str, typ) => (s = str andalso typ = t)) ls) then recPerm (rest,ls)
                                else false
  
  fun recWidth ([],[]) = true
    | recWidth ([], _) = false
    | recWidth (_, []) = true
    | recWidth ((s1,t1)::rest1, (s2,t2)::rest2) = if (s1 = s2 andalso t1 = t2) then recWidth (rest1,rest2)
                                else false

  fun subty (T.Int, T.Int) = true
    | subty (T.Bool, T.Bool) = true
    | subty (T.Unit, T.Unit) = true
    | subty (T.Record ls1, T.Record ls2) = 
        let
          fun checkAll (_, []) = true
            | checkAll (ls1, (s2,t2)::rest2) =
                if (List.exists (fn (str, typ) => (s2 = str andalso subty(typ,t2))) ls1) then checkAll (ls1, rest2)
                else false
        in
          if length ls1 < length ls2 then false
          else checkAll (ls1, ls2)
        end
    (*
    | subty (T.Record ls1, T.Record ls2) = 
        let
          fun recDepth ([],[]) = true
            | recDepth ((s1,t1)::rest1, (s2,t2)::rest2) = if (s1 = s2 andalso subty(t1,t2)) then recDepth (rest1,rest2)
                                                          else false
        in
          if length ls1 < length ls2 then false
          else if (length ls1 = length ls2) then (recDepth(ls1,ls2) orelse recPerm(ls1,ls2))
          else recWidth (ls1, ls2) 
        end
    *)
    | subty (T.Function (in1, out1), T.Function (in2, out2)) = subty(in2, in1) andalso subty(out1,out2)
    | subty _ = false

  fun commonSupertype (t1, t2) = 
    if subty(t1, t2) then SOME t2
    else if subty(t2,t1) then SOME t1
    else NONE
  
  fun typeHelper (env, L.Int n) = T.Int
    | typeHelper (env, L.True) = T.Bool
    | typeHelper (env, L.False) = T.Bool
    | typeHelper (env, L.Unit) = T.Unit
    | typeHelper (env, L.Var s) = (case TypeEnv.lookup(env, s) of
                                SOME t => t
                              | NONE => raise Fail "Undefined variable")
    | typeHelper (env, L.Lam (s, typ, t)) = T.Function (typ, typeHelper(TypeEnv.extend(env, s, typ), t))
    | typeHelper (env, L.App (t1, t2)) = (case typeHelper(env, t1) of
                                            T.Function(f1, f2) => if subty(typeHelper(env,t2),f1) then f2
                                                                  else raise Fail "Application type error"
                                          | _ => raise Fail "First term in application isn't function")
    | typeHelper (env, L.Fix t) = (case typeHelper(env, t) of
                                    T.Function(t1,t2) => if t1 = t2 then t1
                                                          else raise Fail "Fix type error"
                                    | _ => raise Fail "Fix type error")
    | typeHelper (env, L.Let (s, t1, t2)) = (case typeHelper(env, t1) of
                                              t1' => (case typeHelper (TypeEnv.extend(env, s, t1'), t2) of
                                                        t2' => t2')
                                        )
    | typeHelper (env, L.Cond (t1, t2, t3)) = (case typeHelper(env,t1) of
                                                T.Bool => (case commonSupertype(typeHelper(env,t2), typeHelper(env,t3)) of
                                                              SOME t4 => t4
                                                            | NONE => raise Fail "Cond type error") (*check if this is right*)
                                                | _ => raise Fail "Cond type error - t1 not Bool")
    | typeHelper (env, L.Add (t1, t2)) = if (typeHelper(env, t1) = T.Int andalso typeHelper(env, t2) = T.Int) then T.Int
                                          else raise Fail "Addition type error"
    | typeHelper (env, L.Sub (t1, t2)) = if (typeHelper(env, t1) = T.Int andalso typeHelper(env, t2) = T.Int) then T.Int
                                          else raise Fail "Subtraction type error"   
    | typeHelper (env, L.Mul (t1, t2)) = if (typeHelper(env, t1) = T.Int andalso typeHelper(env, t2) = T.Int) then T.Int
                                          else raise Fail "Multiplication type error"   
    | typeHelper (env, L.Eq (t1, t2)) = if (typeHelper(env, t1) = T.Int andalso typeHelper(env, t2) = T.Int) then T.Bool
                                          else raise Fail "Eq type error"      
    | typeHelper (env, L.LessThan (t1, t2)) = if (typeHelper(env, t1) = T.Int andalso typeHelper(env, t2) = T.Int) then T.Bool
                                          else raise Fail "Less than type error"       
    | typeHelper (env, L.Not t) = if typeHelper(env,t) = T.Bool then T.Bool
                                  else raise Fail "Not type error"      
    | typeHelper (env, L.Record ls) = 
        let
          fun recType [] = []
            | recType ((s,term)::rest) = (s, typeHelper(env,term)) :: recType rest
        in
          T.Record (recType ls)  
        end
    | typeHelper (env, L.Select (s, t)) = (case typeHelper(env,t) of
                                              T.Record ls =>  (case (List.find (fn (str, _) => str = s) ls) of
                                                                  SOME (s, typ) => typ
                                                                | NONE => raise Fail "Record doesn't exist")
                                            | _ => raise Fail "Select type error")

  fun typeof t = typeHelper(E.empty, t) 
	    
end
