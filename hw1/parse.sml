structure Parse : sig

  val next  : Token.token list -> (AST.term * Token.token list) option
  val parse : Token.token list -> AST.term

end = struct

  structure T = Token
  structure A = AST
  
  fun next [] = NONE
    | next(T.Z :: toks) = SOME(A.Zero,toks)
    | next(T.T :: toks) = SOME(A.True,toks)
    | next(T.F :: toks) = SOME(A.False,toks) 
    | next _ = raise Fail "todo"

  fun parse _ = raise Fail "todo"
		     
end
