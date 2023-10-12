structure Test = struct

  structure T = Token
  structure A = AST

  fun scan () =
    let
      val _ = Check.expect (Scan.scan "Z", [T.Z], "scan0")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      (* write more scan tests here *)
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Z], A.Zero, "parse0")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
    in
      TextIO.print "parse tests done\n"
    end

  fun eval () =
    let
      val _ = Check.expect (Eval.eval A.Zero, [A.Zero], "eval0")
      val _ = Check.expect(Eval.eval (A.Add (A.Zero,A.Zero)), [A.Add (A.Zero,A.Zero), A.Zero], "eval1")
      val _ = Check.expect(Eval.eval (A.Succ (A.And (A.True, A.False)))
              , [A.Succ (A.And (A.True, A.False)), A.Succ (A.False)], "eval2")
      val _ = Check.expect(Eval.eval (A.Pred A.Zero), [A.Pred A.Zero, A.Zero], "eval3")
      val _ = Check.expect(Eval.eval (A.Pred (A.Succ (A.Zero)))
              , [(A.Pred (A.Succ (A.Zero))), A.Zero], "eval4")
      val _ = Check.expect(Eval.eval (A.Pred (A.Pred (A.Zero)))
              ,[A.Pred (A.Pred A.Zero),A.Pred A.Zero,A.Zero], "eval5")
      val _ = Check.expect(Eval.eval (A.Add (A.Succ (A.Succ (A.Zero)), A.Zero))
              , [A.Add (A.Succ (A.Succ A.Zero),A.Zero),A.Add (A.Succ A.Zero,A.Succ A.Zero),
                A.Add (A.Zero,A.Succ (A.Succ A.Zero)),A.Succ (A.Succ A.Zero)]
              , "eval6")
      val _ = Check.expect(Eval.eval (A.Add ((A.Succ A.Zero), A.True))
              , [(A.Add ((A.Succ A.Zero), A.True)),A.Add (A.Zero,A.Succ A.True),
                (A.Succ A.True)]
              , "eval7")
      val _ = Check.expect(Eval.eval (A.Subtract (A.Succ (A.Succ (A.Zero)), A.Zero))
              , [(A.Subtract (A.Succ (A.Succ (A.Zero)), A.Zero)), A.Succ (A.Succ A.Zero)]
              , "eval8")
      val _ = Check.expect(Eval.eval (A.Subtract (A.Zero, A.Succ (A.Succ (A.Zero))))
              , [(A.Subtract (A.Zero, A.Succ (A.Succ (A.Zero)))), A.Zero]
              , "eval9")
      val _ = Check.expect(Eval.eval (A.Subtract (A.Succ A.Zero, A.Succ A.Zero))
              , [(A.Subtract (A.Succ A.Zero, A.Succ A.Zero)), A.Subtract (A.Zero, A.Zero), A.Zero]
              , "eval10")
      val _ = Check.expect(Eval.eval (A.Less (A.Zero, A.Zero))
              , [(A.Less (A.Zero, A.Zero)), A.False]
              , "eval11")
      val _ = Check.expect(Eval.eval (A.Less (A.Zero, A.Succ (A.Succ A.Zero)))
              , [(A.Less (A.Zero, A.Succ (A.Succ A.Zero))), A.True]
              , "eval12")
      val _ = Check.expect(Eval.eval (A.Less (A.Succ A.Zero, A.Zero))
              , [(A.Less (A.Succ A.Zero, A.Zero)), A.False]
              , "eval13")
      val _ = Check.expect(Eval.eval (A.Less (A.Succ A.Zero, A.Succ A.Zero))
              , [(A.Less (A.Succ A.Zero, A.Succ A.Zero)), A.Less (A.Zero, A.Zero), A.False]
              , "eval14")
      val _ = Check.expect(Eval.eval (A.Less (A.Succ A.Zero, A.Succ A.True))
              , [(A.Less (A.Succ A.Zero, A.Succ A.True))]
              , "eval15")
      val _ = Check.expect(Eval.eval (A.Less (A.And (A.True, A.False), A.Succ A.Zero))
              , [(A.Less (A.And (A.True, A.False), A.Succ A.Zero)), (A.Less (A.False, A.Succ A.Zero))]
              , "eval16")   
      val _ = Check.expect(Eval.eval (A.Greater (A.Zero, A.Succ A.Zero))
              , [A.Greater (A.Zero,A.Succ A.Zero), A.False]
              , "eval17")  
      val _ = Check.expect(Eval.eval (A.Greater (A.Succ A.Zero, A.Zero))
              , [(A.Greater (A.Succ A.Zero, A.Zero)), A.True]
              , "eval18") 
      val _ = Check.expect(Eval.eval (A.Greater (A.Succ A.Zero, A.And(A.True, A.True)))
              , [(A.Greater (A.Succ A.Zero, A.And(A.True, A.True))), (A.Greater (A.Succ A.Zero, A.True))]
              , "eval19")   
      val _ = Check.expect(Eval.eval (A.And (A.True, A.And(A.True, A.False)))
              , [(A.And (A.True, A.And(A.True, A.False))), A.And(A.True, A.False), A.False]
              , "eval20")
      val _ = Check.expect(Eval.eval (A.And (A.False, A.And(A.True, A.False)))
              , [(A.And (A.False, A.And(A.True, A.False))), A.False]
              , "eval20") 
      val _ = Check.expect(Eval.eval (A.And (A.And (A.True, A.True), A.And (A.True, A.Zero)))
              , [A.And (A.And (A.True,A.True),A.And (A.True,A.Zero)),A.And (A.True,A.And (A.True,A.Zero)),
                A.And (A.True,A.Zero),A.Zero]
              , "eval21") 
      val _ = Check.expect(Eval.eval (A.Or (A.And (A.True, A.True), A.Or (A.True, A.Zero)))
              , [A.Or (A.And (A.True,A.True),A.Or (A.True,A.Zero)),A.Or (A.True,A.Or (A.True,A.Zero)), A.True]
              , "eval22")
      val _ = Check.expect(Eval.eval (A.Cond (A.True, A.Or (A.False, A.False), A.Zero))
              , [(A.Cond (A.True, A.Or (A.False, A.False), A.Zero)), A.Or (A.False, A.False), A.False]
              , "eval23")
      val _ = Check.expect(Eval.eval (A.Cond (A.False, A.Or (A.False, A.False), A.And (A.True, A.True)))
              , [(A.Cond (A.False, A.Or (A.False, A.False), A.And (A.True, A.True))), A.And (A.True, A.True), A.True]
              , "eval24")
      val _ = Check.expect(Eval.eval (A.Cond ((A.And (A.False, A.True)), A.Or (A.False, A.False), A.And (A.True, A.True)))
              , [(A.Cond ((A.And (A.False, A.True)), A.Or (A.False, A.False), A.And (A.True, A.True)))
                , (A.Cond (A.False, A.Or (A.False, A.False), A.And (A.True, A.True)))
                , A.And (A.True, A.True)
                , A.True]
              , "eval25")
      val _ = Check.expect(Eval.eval (A.Cond(A.Greater(A.Succ A.Zero, A.Succ A.Zero), A.Zero, A.Or(A.True, A.False))),
              [(A.Cond(A.Greater(A.Succ A.Zero, A.Succ A.Zero), A.Zero, A.Or(A.True, A.False))), 
              (A.Cond(A.Greater( A.Zero, A.Zero), A.Zero, A.Or(A.True, A.False))),
              (A.Cond(A.False, A.Zero, A.Or(A.True, A.False))),
              A.Or(A.True, A.False),
              A.True],
              "eval26")
      val _ = Check.expect(Eval.eval(Parse.parse(Scan.scan"[ [SZ > SZ] ? Z : [T || F]]")),
              [(A.Cond(A.Greater(A.Succ A.Zero, A.Succ A.Zero), A.Zero, A.Or(A.True, A.False))), 
              (A.Cond(A.Greater( A.Zero, A.Zero), A.Zero, A.Or(A.True, A.False))),
              (A.Cond(A.False, A.Zero, A.Or(A.True, A.False))),
              A.Or(A.True, A.False),
              A.True],
              "eval27")
      val _ = Check.expect(Eval.eval(A.Pred(A.False)), [A.Pred(A.False)], "eval28")
      val _ = Check.expect(Eval.eval(A.Succ(A.False)), [A.Pred(A.False)], "eval28")
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      val _ = Check.expect (Compile.code "SZ", [A.Succ A.Zero], "compile0")
      (* write more eval tests here *)
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end
