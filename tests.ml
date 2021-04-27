let free_vars_tests : (exp * name list) list = [ 
  (parse_exp valid_program_1, []);
  (parse_exp valid_program_2, []);
  (parse_exp valid_program_3, []);
  (parse_exp valid_program_4, []);
  (parse_exp valid_program_5, []);
  (parse_exp valid_program_6, []);
  (parse_exp valid_program_7, []);
  (parse_exp valid_program_8, []);
  (parse_exp valid_program_9, []);
  (parse_exp valid_program_10, []);
  (parse_exp "100;", []);
  (parse_exp "true;", []); 
  (parse_exp "x + y * ~2;", ["x"; "y"]);
  (parse_exp "if x then 10 else y;", ["x"; "y"]);
  (parse_exp "x + 10 * 20 / y;", ["x"; "y"]);
  (parse_exp "let val (x, y) = (10, x) in ~x + y * z end;", ["x"; "z"]);
  (parse_exp "let name z = true in x || z && y end;", ["x"; "y"]);
  (parse_exp "fn x => x * y;", ["y"]);
  (parse_exp "let val f = fn x => fn y => x y in f x 10 end;", ["x"]); 
  (parse_exp "let fun g f : int = let name f1 = fn x => f x + y in f 70 end in g f end;", ["y"; "f"]);
  (parse_exp "let fun test (x : int): int = 50 in test 1 end;", []);
  (parse_exp "(if true then 3 else 5) : int;", []);
  (parse_exp "let val (x, y) = (x, 10) val (u, v) = (x + y, y) in u + v end;", ["x"]);
]

let unused_vars_tests : (exp * name list) list = [
  (parse_exp valid_program_1, []);
  (parse_exp valid_program_2, []);
  (parse_exp valid_program_3, []);
  (parse_exp valid_program_4, []);
  (parse_exp valid_program_5, []);
  (parse_exp valid_program_6, ["x"]);
  (parse_exp valid_program_7, []);
  (parse_exp valid_program_8, []);
  (parse_exp valid_program_9, []);
  (parse_exp valid_program_10, ["y"]);
  (parse_exp "let val x = 3 in 4 end;", ["x"]);
  (parse_exp "let val x = true in let val y = 4 in x + 5 end end;", ["y"]);
  (parse_exp "let val x = 3 in let val x = 4 in x + x end end;", ["x"]);
  (parse_exp "let val x = true name y = z + 1 in x end;", ["y"]);
  (parse_exp "let val x = 3 in 4 end;", ["x"]);
  (parse_exp "let val x = 3 in let val y = x in 5 end end;", ["y"]);
  (parse_exp "let val x = true in let val y = 4 in x + 5 end end;", ["y"]);
  (parse_exp "let val x = 3 in (let val x = 4 in x + x end) end;", ["x"]); (* first occurence of x is unused *)
  (parse_exp "let val x = 2 name y = 3 in x + 2 end;", ["y"]); 
  (parse_exp "let val x = 1 val x = 1 in x + 1 end;", ["x"]);
  (parse_exp "let val x = 5 val (x,y) = (3,4) in x end;", ["x";"y"]);
  (parse_exp "let name x = true name x = 1 in x end;", ["x"]);
  (parse_exp "(fn y => 2) true;", ["y"]);
  (parse_exp "let fun fact (x : int) : int = if x = 0 then 1 else x * fact(x - 1) in fact 5 end;", []);
  (parse_exp "let fun test ( x : int ) : int = 3 in 4 end;", ["test"; "x"]); 
  (parse_exp "(fn y => let val y = 2 in 3 + y end) true;", ["y"]);
  (parse_exp "10 * 10 + 33;", []); 
  (parse_exp "let val f = let val ten = 10 in (fn y => ten) : int -> int end in f 55 end;", ["y"]);
  (parse_exp "let val (x,y,z) = (1,2,3) 
              in let val (x,y,z) = (3,4,5) in x+y+z end 
              end;", ["x"; "y"; "z"]);
  (parse_exp "let val (x, y) = (x, y) val (u, v) = (x + y, y) in u + v end;", []);
  (parse_exp "let val x = 3 val y = x val z = 1 in 4 end;", ["y";"z"]);
]

let subst_tests : (((exp * name) * exp) * exp) list = [
  (((Int 5, "x"), If(Bool(true), Var "x", Var "y")), If (Bool true, Int 5, Var "y"));
  (((Int 5, "y"), parse_exp "fn y => x + y;"), parse_exp "fn y => x + y;");
  (((Int 5, "x"), parse_exp "fn y => x + y;"), parse_exp "fn y => 5 + y;");
  (((Int 5, "x"), parse_exp "(fn x => x + 1) x;"), parse_exp "(fn x => x + 1) 5;");
  (((Int 5, "x"), parse_exp "(fn y => y + 1) x;"), parse_exp "(fn y => y + 1) 5;");
  (((Int 6, "y"), parse_exp "let fun fact (x : int) : int = if x = 0 then 1 else x * fact(x - 1) * y in fact 5 end;"), parse_exp "let fun fact (x : int) : int = if x = 0 then 1 else x * fact(x - 1) * 6 in fact 5 end;");
  (((Var "y","x"), parse_exp "let val x = 3 + x in x end;"), parse_exp "let val x = 3 + y in x end;");
  (((Int 69, "x"), parse_exp "let val y = x + 3 in x end;"), parse_exp "let val y = 69 + 3 in 69 end;");
  (((Var "y","x"), parse_exp "let name x = 3 + x in x end;"), parse_exp "let name x = 3 + y in x end;");
  (((Int 69, "x"), parse_exp "let name y = x + 3 in x end;"), parse_exp "let name y = 69 + 3 in 69 end;");
  (((Int 69, "y"), parse_exp "let val x = 5 + y val (x,y) = (3,4) in x end;"), parse_exp "let val x = 5 + 69 val (x,y) = (3,4) in x end;");
  (((parse_exp "x + 1;", "y"), parse_exp "let val (x,y) = (3,y) in x + y end;"), parse_exp "let val (x,y) = (3,x + 1) in x + y end;");
  (* Var rename tests *)
  (((parse_exp "y + 1;", "x"), parse_exp "fn y => x + y;"), parse_exp "fn Y => y + 1 + Y;");
  (((parse_exp "x + 1;", "y"), parse_exp "let name x = y val (x,y) = (3,y) in x + y end;"), parse_exp "let name X = x + 1 val (x,y) = (3,x + 1) in x + y end;");
  (((parse_exp "2 * w;", "x"), parse_exp "let val w = 3 name y = 2 * x in fn x => w * y * x end;"), parse_exp "let val W = 3 name y = 2 * 2 * w in fn x => W * y * x end;");
  (((parse_exp "y + 3;", "x"), parse_exp "let name w = x in fn y => x + 2 end;"), parse_exp "let name w = y + 3 in fn Y => y + 3 + 2 end;");
  (((parse_exp "y * 2;", "w"), parse_exp "let val x = 3 val y = w * 3 in fn x => w * y * x end;"), parse_exp "let val x = 3 val Y = y * 2 * 3 in fn x => y * 2 * Y * x end;")
]

let eval_tests : (exp * exp) list = [
  (parse_exp "true && true && true;", parse_exp "true;");
  (parse_exp "true && false;", parse_exp "false;");
  (parse_exp "false && true;", parse_exp "false;");
  (parse_exp "false || false || true;", parse_exp "true;"); 
  (parse_exp "false || false;", parse_exp "false;");
  (parse_exp "true || false;", parse_exp "true;"); 
  (parse_exp "fn x => 3;", parse_exp "fn x => 3;");
  (parse_exp "(fn x => if x then 69 else 4) true;", parse_exp "69;"); 
  (parse_exp "let val (x,y) = (3,2) in x + y end;", parse_exp "5;");
  (parse_exp "let name x = 10 in let name x = x + 1 val (x, y) = (3, x + 1) in x + y end end;", 
   parse_exp "14;"); 
  (parse_exp "let val x = 10 in let name x = x + 1 val (x, y) = (3, x + 1) in x + y end end;", 
   parse_exp "14;"); 
  (parse_exp valid_program_1, parse_exp "300;"); 
  (parse_exp valid_program_2, parse_exp "133;");
  (parse_exp valid_program_3, parse_exp "120;");
  (parse_exp valid_program_4, parse_exp "3;");
  (parse_exp valid_program_5, parse_exp "6;");
  (parse_exp valid_program_6, parse_exp "6;");
  (parse_exp valid_program_7, parse_exp "4;");
  (parse_exp valid_program_8, parse_exp "900;");
  (parse_exp valid_program_9, parse_exp "1600;");
  (parse_exp valid_program_10, parse_exp "10;");
]

let infer_tests : ((context * exp) * typ) list = [
  ((Ctx([]), parse_exp "2 + 3;"), TInt);
  ((Ctx([]), parse_exp "2 < 3;"), TBool);
  ((Ctx([]), parse_exp "69 + 3 < 44;"), TBool);
  ((Ctx([]), parse_exp "if 69 + 3 < 44 then 32 else 69;"), TInt);
  ((Ctx([]), parse_exp "(fn x : bool => x) true;"), TBool);
  ((Ctx([]), parse_exp "(fn x : int => x + 2) 3;"), TInt);
  ((Ctx([]), parse_exp "(fn x : bool => if x then 54 else 69) true;"), TInt); 
  ((Ctx([]), parse_exp valid_program_2), TInt);
  ((Ctx([]), parse_exp valid_program_3), TInt);
  ((Ctx([]), parse_exp valid_program_4), TInt);
  ((Ctx([]), parse_exp valid_program_5), TInt);
  ((Ctx([]), parse_exp valid_program_6), TInt);
  ((Ctx([]), parse_exp valid_program_7), TInt);
  ((Ctx([]), parse_exp valid_program_8), TInt);
  ((Ctx([]), parse_exp valid_program_9), TInt);
  ((Ctx([]), parse_exp "let val (x,y) = (3 + 2, true) in if y then x else x + 2 end;"), TInt);
  ((Ctx([]), parse_exp "let val x = 4 name x = true val y = x in y end;"), TBool);
  ((Ctx([]), parse_exp "let val x = 4 name x = true val y = x in x || false end;"), TBool);
  ((Ctx([]), parse_exp "(fn x : int => if x = 0 then true else false);"), TArrow(TInt, TBool));
  ((Ctx([]), parse_exp "let val a = 4 val a = true in a end;"), TBool);
  (* Q7 Tests *)
  ((Ctx([]), parse_exp "(fn x => x) true;"), TBool);
  ((Ctx([]), parse_exp "(fn x => x + 2) 3;"), TInt);
  ((Ctx([]), parse_exp "(fn x => if x then 54 else 69) true;"), TInt); 
  ((Ctx([]), parse_exp "(fn x => if x = 0 then true else false);"), TArrow(TInt, TBool));
  ((Ctx([]), parse_exp "let val f = fn x : int => x in f 5 end;"), TInt); 
]

let unify_tests : ((typ * typ) * unit) list = [
  (* Not Unifiable *)
  ((TBool, TVar(ref (Some TInt))), ()); 
  ((TInt, TArrow(TBool, TInt)), ());
  ((TProduct [TBool; fresh_tvar ()], TProduct [TBool; TInt; TInt]), ());
  ((TProduct [TBool; fresh_tvar ()], TProduct [TInt; fresh_tvar ()]), ());
  (* Unifiable *)
  ((TInt, TInt), ());
  ((TBool, TBool), ());
  ((fresh_tvar (), TArrow(TBool, TInt)), ());
  ((TArrow (fresh_tvar (), TBool), TArrow (TInt, TBool)), ());
  ((TProduct [TArrow(fresh_tvar (), TBool); fresh_tvar ()], TProduct [fresh_tvar (); TInt]), ());
  ((TArrow(TBool, TProduct [TInt; TInt]), TArrow(fresh_tvar (), fresh_tvar ())), ());
]