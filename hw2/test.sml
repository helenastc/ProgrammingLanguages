structure Test = struct

  structure T = Token
  structure S = Sugary
  structure D = Desugared
  structure E = Eval
		  
  fun scan () =
    let
      val _ = Check.expect (Scan.scan "12", [T.Nat 12], "scan12")
      val _ = Check.exn (fn () => Scan.scan "~", "badScan00")
      val _ = Check.expect (Scan.scan "1   2", [T.Nat 1, T.Nat 2], "scan0")
    in
      TextIO.print "scan tests done\n"
    end

  fun parse () =
    let
      val _ = Check.expect (Parse.parse [T.Nat 12], S.Nat 12, "parse12")
      val _ = Check.exn (fn () => Parse.parse [T.LBrack], "badParse0")
      (* write more parse tests here *)
    in
      TextIO.print "parse tests done\n"
    end

  fun typ () =
    let
      val _ = Check.expect (TypeCheck.typeof (S.Nat 12), Type.Nat, "type12") 
    in
      TextIO.print "type tests done\n"
    end

  fun desugar () =
    let
      val desugar = Desugar.desugar
      val _ = Check.expect (desugar (S.Nat 0), D.Zero, "desugar0")
    in
      TextIO.print "desugar tests done\n"
    end
			            
  fun eval () =
    let
      val _ = Check.expect (Eval.result D.Zero, Eval.Value D.Zero, "eval0")
      val _ = Check.expect (Eval.result(Desugar.desugar(Parse.parse(Scan.scan"[[105 - 104] > 1]"))), Eval.Value D.Zero, "eval1")
      val _ = Check.expect (Eval.result (D.First (D.Add (D.Succ D.Zero, D.Zero))), Eval.Stuck (D.First (D.Succ D.Zero)), "eval2")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"[[5+10] == [20-5]]"))), Eval.Value (D.Succ D.Zero), "eval3")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"[[5>10] ^^ [20>5]]"))), Eval.Value (D.Succ D.Zero), "eval4")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"[[[10<=10] && [5==5]] ? 1 : 2]"))), Eval.Value (D.Succ D.Zero), "eval5")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"[[[T^^T] || [5>5]] ? 1 : 2]"))), Eval.Value (D.Succ(D.Succ D.Zero)), "eval6")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"1#(   [5 >= 6], [5>=5])"))), Eval.Value D.Zero, "eval7")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"2#(  1, [1+1 ])"))), Eval.Value (D.Succ(D.Succ D.Zero)), "eval8")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"[([1+2],5)==(3,5)]"))), Eval.Value (D.Succ D.Zero), "eval9")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"[([1+2],6)==(3,5)]"))), Eval.Value (D.Zero), "eval10")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"[([1+2],5)==(3,5)]"))), Eval.Value (D.Succ D.Zero), "eval9")
      val _ = Check.expect (Eval.result (Desugar.desugar(Parse.parse(Scan.scan"1#[1+1]"))), Eval.Stuck (D.First(D.Succ (D.Succ D.Zero))), "eval10")
    in
      TextIO.print "eval tests done\n"
    end

  fun compile () =
    let
      fun value typ program result =
	Check.expect (Compile.code program, (E.Value result, typ), "compile"^program)
      val natval = value Type.Nat
      val boolval = value Type.Bool
      val _ = natval "0" D.Zero 
      (* write more compile tests here *)
    in
      TextIO.print ("compile tests done\n")
    end
      
  fun all () =
    let
      val _ = scan ()
      val _ = parse ()
      val _ = typ ()
      val _ = desugar ()
      val _ = eval ()
      val _ = compile ()
    in
      TextIO.print "all tests done\n"
    end
      
end
