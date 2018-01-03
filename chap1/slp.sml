(* Defines SLP language elements as a self-contained structure *)

structure Slp =
struct
  type id = string

  datatype binop = Plus | Minus | Times | Div

  datatype stm = CompoundStm of stm * stm
        	     | AssignStm of id * exp
      	       | PrintStm of exp list

       and exp = IdExp of id
      	       | NumExp of int
               | OpExp of exp * binop * exp
               | EseqExp of stm * exp

  val prog = 
   CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
   CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
     PrintStm[IdExp "b"]))

  (* Maxargs{Stm,Exp} process the AST keeping track of the maximum 
   * PrintStm list length seen.
   *)
  fun maxargs st = let
    fun maxargsStm (CompoundStm (stm1, stm2)) curmax = let
      val c1 = maxargsStm stm1 curmax
    in
      maxargsStm stm2 c1
    end
      | maxargsStm (AssignStm (_, e1)) curmax = maxargsExp e1 curmax
      | maxargsStm (PrintStm (nil)) curmax = curmax
      | maxargsStm (PrintStm (e::rest)) curmax = let
      val thislen = length (e::rest)
      val maxE = maxargsExp e curmax
      val maxrest = maxargsStm (PrintStm(rest)) maxE
    in
      if thislen > maxrest then 
        thislen
      else maxrest
    end
    and maxargsExp (IdExp s) curmax = curmax
      | maxargsExp (NumExp n) curmax = curmax
      | maxargsExp (OpExp (e1, oper, e2)) curmax = let
      val c1 = maxargsExp e1 curmax
    in
      maxargsExp e2 c1
    end
      | maxargsExp (EseqExp (st, ex)) curmax = let
      val c1 = maxargsStm st curmax
    in
      maxargsExp ex c1
    end
  in
    maxargsStm st 0
  end
  
  (* the environment type *)
  type env = (id * int) list

  val emptyEnv = []

  fun update (t:env, sym, v) = ((sym, v)::t)

  (* returns the first value seen for the given environment or error *)
  fun lookup (nil:env, key) = raise Empty
    | lookup ((sym, v)::rest, key) = if sym = key then
                                      v
                                     else lookup(rest, key)
  (* Interprets the language, receiving an empty environment at start *)
  fun interp prog = let
    fun interpStm (CompoundStm(s1, s2), tab) = let
      val t1 = interpStm (s1, tab)
    in
      interpStm (s2, t1)
    end
      | interpStm (AssignStm(sym, e), tab) = let
      val (v, tab1) = interpExp(e, tab)
    in
      update(tab1, sym, v)
    end
      | interpStm (PrintStm(nil), tab) = tab
      | interpStm (PrintStm(e::rest), tab) = let
      val (eres, tab1) = interpExp (e, tab)
    in
      print ((Int.toString eres) ^ "\n");
      interpStm(PrintStm(rest), tab1)
    end
    and interpExp (IdExp sym, tab) = (lookup(tab, sym), tab)
      | interpExp (NumExp v, tab) = (v, tab)
      | interpExp (OpExp (e1, oper, e2), tab) = let
      val (v1, tab1) = interpExp (e1, tab)
      val (v2, tab2) = interpExp (e2, tab1)
      val res = case oper of 
                  Plus => v1 + v2
                | Minus => v1 - v2
                | Times => v1 * v2
                | Div => v1 div v2
    in
      (res, tab2)
    end
      | interpExp (EseqExp (s1, e1), tab) = let
      val tab1 = interpStm (s1, tab)
    in
      interpExp (e1, tab1)
    end
  in
    interpStm (prog, emptyEnv)
  end
end
