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
      val _ = Check.expect (Eval.eval(A.Zero), [A.Zero], "eval0")
      val _ = Check.expect (Eval.eval(A.False), [A.False], "eval1")
      val _ = Check.expect (Eval.eval(A.Succ(A.Succ(A.Succ(A.Zero)))), [A.Succ(A.Succ(A.Succ(A.Zero)))], "eval2")
      val _ = Check.expect (Eval.eval(A.Succ(A.Succ(A.Pred(A.Zero)))), [A.Succ(A.Succ(A.Pred(A.Zero))), A.Succ(A.Succ(A.Zero))], "eval3")
      val _ = Check.expect(Eval.eval (A.Add (A.Zero,A.Zero)), [A.Add (A.Zero,A.Zero), A.Zero], "eval4")
      val _ = Check.expect(Eval.eval (A.Add (A.Zero,A.Succ(A.Zero))), [A.Add (A.Zero,A.Succ(A.Zero)), A.Succ(A.Zero)], "eval5")
      val _ = Check.expect(Eval.eval (A.Add (A.Succ(A.Succ(A.Zero)),A.Succ(A.Zero))), 
              [(A.Add (A.Succ(A.Succ(A.Zero)),A.Succ(A.Zero))), (A.Add(A.Succ(A.Zero), A.Succ(A.Succ(A.Zero)))),
              (A.Add(A.Zero, A.Succ(A.Succ(A.Succ(A.Zero))))),
              A.Succ(A.Succ(A.Succ(A.Zero)))], "eval6")
      val _ = Check.expect(Eval.eval (A.Succ (A.And (A.True, A.True)))
              , [A.Succ (A.And (A.True, A.True)), A.Succ (A.True)], "eval7")
      val _ = Check.expect(Eval.eval(A.Pred (A.Zero)), [A.Pred (A.Zero), A.Zero], "eval8")
      val _ = Check.expect(Eval.eval(A.Pred (A.Pred (A.Zero))),[(A.Pred (A.Pred (A.Zero))), A.Pred(A.Zero), A.Zero], "eval9")
      val _ = Check.expect(Eval.eval(A.Succ(A.Pred(A.True))), [(A.Succ(A.Pred(A.True)))], "eval10")
      val _ = Check.expect(Eval.eval (A.Cond(A.Greater(A.Succ A.Zero, A.Succ A.Zero), A.Zero, A.Or(A.True, A.False))),
              [(A.Cond(A.Greater(A.Succ A.Zero, A.Succ A.Zero), A.Zero, A.Or(A.True, A.False))), 
              (A.Cond(A.Greater( A.Zero, A.Zero), A.Zero, A.Or(A.True, A.False))),
              (A.Cond(A.False, A.Zero, A.Or(A.True, A.False))), A.Or(A.True, A.False), A.True], "eval11")
      val _ = Check.expect(Eval.eval(A.Pred(A.False)), [A.Pred(A.False)], "eval12")
      val _ = Check.expect(Eval.eval(A.Succ(A.False)), [A.Succ(A.False)], "eval13")
      val _ = Check.expect(Eval.eval(A.Subtract(A.Or(A.Zero,A.True),A.Succ(A.Pred(A.Zero)))), [(A.Subtract(A.Or(A.Zero,A.True),A.Succ(A.Pred(A.Zero))))], "eval14")
      val _ = Check.expect(Eval.eval(A.And(A.False,A.Zero)), [(A.And(A.False,A.Zero)), A.False], "eval15")
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
