exception NotImplemented

(* Variables *)
type name = string

(* Primitive operations *)
type primop =
  | Equals        (* v1 = v2 *)
  | NotEquals     (* v1 != v2 *)
  | LessThan      (* i1 < i2 *)
  | LessEqual     (* i1 <= i2 *)
  | GreaterThan   (* i1 > i2 *)
  | GreaterEqual  (* i1 >= i2 *)
  | And           (* b1 && b2 *)
  | Or            (* b1 || b2 *)
  | Plus          (* i1 + i2 *)
  | Minus         (* i1 - i2 *)
  | Times         (* i1 * i2 *)
  | Div           (* i1 / i2 *)
  | Negate        (* ~ i *)

(* type exception *)
exception TypeError of string

let type_fail message = raise (TypeError message)

type typ =
  | TArrow   of typ * typ         (* a -> b *)
  | TProduct of typ list          (* a * b *)
  | TInt                          (* int *)
  | TBool                         (* bool *)
  | TVar     of (typ option) ref  (* Only used for Q6 and Q7 *)

let fresh_tvar () = TVar (ref None)

(* type equality ignoring TVar *)
let rec typ_eq t1 e2 =
  match (t1, e2) with
  | (TArrow (domain1, range1), TArrow (domain2, range2)) ->
     typ_eq domain1 domain2 && typ_eq range1 range2
  | (TProduct ts1, TProduct ts2) ->
     List.length ts1 = List.length ts2 && List.for_all2 typ_eq ts1 ts2
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | _ -> false

(* general exception *)
exception Stuck of string

let stuck message = raise (Stuck message)

type exp =
  | Int    of int                        (* 0 | 1 | 2 | ... *)
  | Bool   of bool                       (* true | false *)
  | If     of exp * exp * exp            (* if e then e1 else e2 *)
  | Primop of primop * exp list          (* e1 <op> e2  or  <op> e *)
  | Tuple  of exp list                   (* (e1, ..., eN) *)
  | Fn     of (name * typ option * exp)  (* fn x => e *)
  | Rec    of name * typ * exp           (* rec f => e *)
  | Let    of dec list * exp             (* let decs in e end *)
  | Apply  of exp * exp                  (* e1 e2 *)
  | Var    of name                       (* x *)
  | Anno   of exp * typ                  (* e : t *)

and dec =
  | Val      of exp * name               (* val x = e *)
  | Valtuple of exp * name list          (* val (x1,...,xN) = e *)
  | ByName   of exp * name               (* name x = e1 *)

let eval_op op args =
  match (op, args) with
  | (Equals,       [Int i1; Int i2])   -> Some (Bool (i1 = i2))
  | (NotEquals,    [Int i1; Int i2])   -> Some (Bool (i1 <> i2))
  | (LessThan,     [Int i1; Int i2])   -> Some (Bool (i1 < i2))
  | (LessEqual,    [Int i1; Int i2])   -> Some (Bool (i1 <= i2))
  | (GreaterThan,  [Int i1; Int i2])   -> Some (Bool (i1 > i2))
  | (GreaterEqual, [Int i1; Int i2])   -> Some (Bool (i1 >= i2))
  | (Plus,         [Int i1; Int i2])   -> Some (Int (i1 + i2))
  | (Minus,        [Int i1; Int i2])   -> Some (Int (i1 - i2))
  | (Times,        [Int i1; Int i2])   -> Some (Int (i1 * i2))
  | (Div,          [Int i1; Int i2])   -> Some (Int (i1 / i2))
  | (Negate,       [Int i])            -> Some (Int (-i))
  | _                                  -> None

type context = Ctx of (name * typ) list

(* Context manipulation helpers *)
exception NotFound

let ctx_lookup ctx x =
  let rec assoc x y =
    match y with
    | [] -> raise NotFound
    | (y, r) :: rest ->
       if x = y then
         r
       else
         assoc x rest
  in
  let Ctx list = ctx in assoc x list

let extend ctx (x, v) = let Ctx list = ctx in Ctx ((x,v)::list)

let rec extend_list ctx l =
  match l with
  | [] -> ctx
  | (x, y) :: pairs -> extend_list (extend ctx (x, y)) pairs

(* Replacement for the standard "result" type *)
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

(* Set helper functions. You might find them useful *)
let member = List.mem

let rec union xs ys =
  match xs with
  | [] -> ys
  | x :: xs ->
     if member x ys then
       union xs ys
     else
       x :: union xs ys

let union_list sets = List.fold_right union sets []

let rec delete ds set =
  match set with
  | [] -> []
  | h :: t ->
     if member h ds then
       delete ds t
     else
       h :: delete ds t


(* free name generator *)
let (fresh_var, reset_ctr) =
  let counter = ref 0 in
  ((fun x ->
    counter := !counter+1;
    string_of_int (!counter) ^ x),
   fun () ->
   counter := 0)

(* Update this to 1 or higher to get debug messages *)
let debug = ref 0

(* example valid MiniML programs *)

let valid_program_1 = "
let fun apply (f : int -> int) : int -> int =
          fn x : int => f(x)
in
  apply (fn x => x * 3) 100
end;
"

let valid_program_2 = "10 * 10 + 33;"

let valid_program_3 = "
let fun fact (x : int) : int =
  if x = 0 then 1
  else x * fact(x - 1)
in
  fact 5
end;
"

let valid_program_4 = "(if true then 3 else 5) : int;"

let valid_program_5 = "
let val x = 1
in
  x + 5
end;
"

let valid_program_6 = "
let val x = true
in
  let val x = 1
  in
    x + 5
  end
end;
"

let valid_program_7 = "
let name x = 3
in
  x + 1
end;
"

let valid_program_8 = "
let val (x,y) = (2 + 1, 2 * 50) in x * x * y end;
"

let valid_program_9 = "
let fun repeat (n : int) : (int -> int) -> int -> int =
          fn f : (int -> int) => fn x : int =>
            if n = 0 then x
            else repeat (n - 1) f (f(x))
in
 repeat 4 (fn z : int => z * 2) 100
 (* expected result: 100 * 2 * 2 * 2 * 2 = 1600 *)
end;
"

let valid_program_10 = "
let val f = let val ten = 10 in (fn y => ten) : int -> int end
in
  f 55
end;
"

(* ============================================================ *)

(* Helper method to parse the expression from a string *)
let parse_exp s = 
  match P.parse s with
  | Right e -> e
  | Left x -> stuck x

(* Q0  : Get familiar with the external syntax of MiniML *)
let parse_tests : (string * (string, exp) either) list = [ 
  (* Provide your tests for the parser *)
  ( valid_program_2, 
    Right (
      Primop (Plus, [Primop (Times, [Int 10; Int 10]); Int 33])
    ) 
  );
]

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
]

(* Q1  : Find the free variables in an expression *)
let rec free_vars (e : exp) : name list = 
  match e with 
  | If (e1, e2, e3) -> union (union (free_vars e1) (free_vars e2)) (free_vars e3)
  | Primop (_, l) -> union_list (List.map free_vars l)
  | Tuple l -> union_list (List.map free_vars l)
  | Fn (x, _, e) 
  | Rec (x, _, e) -> delete [x] (free_vars e)
  | Apply (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var x -> [x]
  | Anno (e, _) -> free_vars e
  | Let (ds, e) -> 
      begin match ds with
        | [] -> free_vars e 
        | d::ds' -> 
            let bv = find_bounded_vars d
            and e' = get_exp d in
            union (free_vars e') (delete bv (free_vars (Let (ds', e))))
      end
  | _ -> []
and find_bounded_vars dec = 
  match dec with 
  | Val (e, x) | ByName (e, x) -> [x]
  | Valtuple (e, xs) -> xs 
and get_exp d = 
  let Val (e, _) | Valtuple (e, _) | ByName (e, _) = d 
  in e

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
]

(* Q2  : Check variables are in use *)
let rec unused_vars (e : exp) : name list = 
  match e with 
  | If (e1, e2, e3) -> union (unused_vars e1) (union (unused_vars e2) (unused_vars e3))
  | Primop (_, l) -> union_list (List.map unused_vars l)
  | Tuple l -> union_list (List.map unused_vars l)
  | Apply (e1, e2) -> union (unused_vars e1) (unused_vars e2)
  | Anno (e, _) -> unused_vars e
  | Fn (x, _, e) -> union (delete (free_vars e) [x]) (unused_vars e)
  | Rec (_, _, e) -> unused_vars e 
  | Let (ds, e) -> 
      begin match ds with
        | [] -> unused_vars e 
        | d::ds' -> 
            let bv = find_bounded_vars d
            and fv = free_vars (Let (ds', e))
            and e' = get_exp d in
            union (union (delete fv bv) (unused_vars (Let (ds', e)))) (unused_vars e')
      end
  | _ -> []


let subst_tests : (((exp * name) * exp) * exp) list = [
  
]
  
let rec subst ((e', x) : exp * name) (e : exp) : exp = 
  let subst_let ds e2 = 
    let get_let_parts e = 
      match e with 
      | Let (ds, e) -> (ds, e)
      | _ -> stuck "Expected Let expression" (* Will never get here *)
    in
    let subst_dec ds' e1 y f = 
      let e1' = subst (e', x) e1 in
      if x = y then 
        Let ((f (e1', y))::ds', e2)
      else if not (member y (free_vars e')) then 
        let (ds, e) = get_let_parts (subst (e', x) (Let(ds', e2)))in 
        Let ((f (e1', y))::ds, e)
      else 
        let y' = fresh_var y in
        let exp = subst (Var y', y) (Let(ds', e2)) in
        let (ds, e) = get_let_parts (subst (e', x) exp) in 
        Let ((f (e1', y'))::ds, e)
    in 
    match ds with 
    | [] -> Let ([], subst (e', x) e2)
    | d::ds' -> 
        match d with 
        | Val (e1, y) -> subst_dec ds' e1 y (fun (e, n) -> Val (e, n)) 
        | ByName (e1, y) -> subst_dec ds' e1 y (fun (e, n) -> ByName (e, n)) 
        | Valtuple (e1, ns) -> 
            let e1' = subst (e', x) e1 in
            if member x ns then 
              Let ((Valtuple (e1', ns))::ds', e2)
            else 
              let fv = free_vars e' in
              let rec rename ns ns' exp = 
                match ns with 
                | [] -> 
                    let (ds, e) = get_let_parts (subst (e', x) exp) in
                    Let ((Valtuple (e1', List.rev ns'))::ds, e)
                | y::t -> 
                    if not (member y fv) then 
                      rename t (y::ns') exp
                    else 
                      let y' = fresh_var y in
                      let exp' = subst (Var y', y) exp in
                      rename t (y'::ns') exp'
              in
              rename ns [] (Let(ds', e2))
  and subst_fun y t e f = 
    if x = y then f (y, t, e)
    else if not (member y (free_vars e')) then 
      f (y, t, subst (e', x) e)
    else 
      let y' = fresh_var y in
      let new_e = subst (Var y', y) e in
      f (y', t,  subst (e', x) new_e)
  in 
  match e with
  | Var y ->
      if x = y then
        e'
      else
        Var y

  | Int _ | Bool _ -> e
  | Primop (po, es) -> Primop (po, List.map (subst (e', x)) es)
  | If (e1, e2, e3) -> If (subst (e', x) e1, subst (e', x) e2, subst (e', x) e3)
  | Tuple es -> Tuple (List.map (subst (e', x)) es)
  | Anno (e, t) -> Anno (subst (e', x) e, t)

  | Let (ds, e2) -> subst_let ds e2
  | Apply (e1, e2) -> Apply (subst (e', x) e1, subst (e', x) e2)
  | Fn (y, t, e) -> subst_fun y t e (fun (y, t, e) -> Fn (y, t, e)) 
  | Rec (y, t, e) -> subst_fun y t e (fun (y, t, e) -> Rec (y, t, e)) 

    
let eval_tests : (exp * exp) list = [ 
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

(* Q4  : Evaluate an expression in big-step *)
let rec eval : exp -> exp =
  (* do not change the code from here *)
  let bigstep_depth = ref 0 in
  fun e ->
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "eval (" ^ Print.exp_to_string e ^ ")\n");
    incr bigstep_depth;
  (* to here *)
    let result =
      match e with
      | Int _ | Bool _ -> e
      | Tuple es -> Tuple (List.map eval es)
      | If (e1, e2, e3) ->
          begin match eval e1 with
            | Bool b ->
                if b then
                  eval e2
                else
                  eval e3
            | _ -> stuck "Condition for if expression should be of the type bool"
          end
      | Anno (e, _) -> eval e     (* types are ignored in evaluation *)
      | Var x -> stuck ("Free variable \"" ^ x ^ "\" during evaluation")

      | Fn (x, t, e) -> Fn (x, t, e)
      | Apply (e1, e2) -> 
          begin match eval e1 with
            | Fn (x, t, e) -> 
                let v = eval e2 in
                eval (subst (v, x) e)
            | _ -> stuck "A function must be passed as the first argument to apply" 
          end
      | Rec (f, t, e) -> eval (subst (Rec (f, t, e), f) e)

      | Primop (And, es) ->
          begin match es with 
            | [e1; e2] -> 
                begin match eval e1 with 
                  | Bool (true) -> eval e2
                  | Bool (false) -> Bool (false)
                  | _ -> stuck "Arguments to 'and' operation must be of type bool"
                end
            | _ -> stuck "Bad arguments to 'and' operation"
          end    
      | Primop (Or, es) ->
          begin match es with 
            | [e1; e2] -> 
                begin match eval e1 with 
                  | Bool (true) -> Bool (true) 
                  | Bool (false) -> eval e2
                  | _ -> stuck "Arguments to 'or' operation must be of type bool"
                end
            | _ -> stuck "Bad arguments to 'or' operation"
          end  
      | Primop (op, es) ->
          let vs = List.map eval es in
          begin match eval_op op vs with
            | None -> stuck "Bad arguments to primitive operation"
            | Some v -> v
          end

      | Let (ds, e) -> 
          match ds with
          | [] -> eval e
          | h::t -> 
              let exp = Let(t, e) in
              match h with
              | Val (e', x) -> eval (subst (eval e', x) exp)
              | ByName (e', x) -> eval (subst (eval e', x) exp)
              | Valtuple (e', xs) -> 
                  match eval e' with
                  | Tuple vs -> 
                      let val_name_pair = List.combine vs xs in
                      eval (List.fold_right subst val_name_pair exp)
                  | _ -> stuck "Valtuple expression should be of type tuple"
    in
  (* do not change the code from here *)
    decr bigstep_depth;
    if !debug >= 1 then
      print_endline
        (String.make (!bigstep_depth) ' '
         ^ "result of eval (" ^ Print.exp_to_string e ^ ") = "
         ^ Print.exp_to_string result ^ "\n");
  (* to here *)
    result 


let unify_tests : ((typ * typ) * unit) list = [
  (* Not Unifiable *)
  (*
   ((TBool, TVar(ref (Some TInt))), ()); 
   ((TInt, TArrow(TBool, TInt)), ());
   ((TProduct [TBool; fresh_tvar ()], TProduct [TBool; TInt; TInt]), ());
   ((TProduct [TBool; fresh_tvar ()], TProduct [TInt; fresh_tvar ()]), ());
  *)
  (* Unifiable *)
  ((TInt, TInt), ());
  ((TBool, TBool), ());
  ((fresh_tvar (), TArrow(TBool, TInt)), ());
  ((TArrow (fresh_tvar (), TBool), TArrow (TInt, TBool)), ());
  ((TProduct [TArrow(fresh_tvar (), TBool); fresh_tvar ()], TProduct [fresh_tvar (); TInt]), ());
  ((TArrow(TBool, TProduct [TInt; TInt]), TArrow(fresh_tvar (), fresh_tvar ())), ());
]

(* find the next function for Q5 *)
(* Q6  : Unify two types *)
let rec unify (ty1 : typ) (ty2 : typ) : unit =
  let rec check_occ a t = 
    match t with
    | TInt | TBool -> false
    | TArrow (t1, t2) -> (check_occ a t1) || ((check_occ a t2))
    | TProduct ts -> List.exists (check_occ a) ts
    | TVar b -> (a == b)
  in
  let unify_tvar x ty = 
    if check_occ x ty then 
      type_fail "Type variable occurs in the given type"
    else 
      match !x with 
      | Some x_ty -> unify x_ty ty
      | None -> x := Some ty 
  in
  match ty1, ty2 with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TArrow (t1, t2), TArrow (t1', t2') -> unify t1 t1'; unify t2 t2'
  | TProduct ts, TProduct ts' ->  
      begin try List.iter2 unify ts ts' with 
          Invalid_argument _ -> type_fail "TProducts are not of the same size" 
      end
  | TVar x, TVar x' -> 
      begin match !x, !x' with 
        | Some t, Some t' -> unify t t'
        | None, Some t -> x := Some t
        | Some t, None -> x' := Some t
        | None, None -> ()
      end
  | TVar x, _ -> unify_tvar x ty2
  | _, TVar x -> unify_tvar x ty1
  | _, _ -> type_fail "Types are not unifiable"
  

let infer_tests : ((context * exp) * typ) list = [
  ((Ctx([]), parse_exp valid_program_1), TInt);
  ((Ctx([]), parse_exp valid_program_2), TInt);
  ((Ctx([]), parse_exp valid_program_3), TInt);
  ((Ctx([]), parse_exp valid_program_4), TInt);
  ((Ctx([]), parse_exp valid_program_5), TInt);
  ((Ctx([]), parse_exp valid_program_6), TInt);
  ((Ctx([]), parse_exp valid_program_7), TInt);
  ((Ctx([]), parse_exp valid_program_8), TInt);
  ((Ctx([]), parse_exp valid_program_9), TInt);
  ((Ctx([]), parse_exp valid_program_10), TInt);
  (* Q7 Tests *)
  ((Ctx([]), parse_exp "(fn x => x) true;"), TBool);
  ((Ctx([]), parse_exp "(fn x => x + 2) 3;"), TInt);
  ((Ctx([]), parse_exp "(fn x => if x then 54 else 69) true;"), TInt); 
  ((Ctx([]), parse_exp "(fn x => if x = 0 then true else false);"), TArrow(TInt, TBool));
  ((Ctx([]), parse_exp "let val f = fn x => x in f 5 end;"), TInt); 
]

(* Q5  : Type an expression *)
(* Q7* : Implement the argument type inference
         For this question, move this function below the "unify". *)
let rec infer (ctx : context) (e : exp) : typ = 
  let rec remove_tvars t = 
    match t with 
    | TInt -> TInt
    | TBool -> TBool
    | TArrow (t1, t2) -> TArrow (remove_tvars t1, remove_tvars t2)
    | TProduct ts -> TProduct (List.map remove_tvars ts) 
    | TVar x -> 
        match !x with       
        | Some t -> remove_tvars t
        | None -> t 
  in 

  match e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var x -> 
      begin try 
          let ty = ctx_lookup ctx x in remove_tvars ty
        with
          NotFound -> type_fail ("Variable \"" ^ x ^ "\" not found.")
      end
  | If (e, e1, e2) ->
      begin try unify (infer ctx e) TBool; 
          let t1 = infer ctx e1 
          and t2 = infer ctx e2 in
          begin try unify t1 t2; t1 with 
              TypeError _ -> type_fail "The consequent and alternative of the If statment have diffrent types"
          end 
        with
          TypeError _ -> type_fail "Expected TBool"
      end 
  | Primop (op, es) -> 
      let (t1, t2) = 
        match es with 
        | e1::e2::t -> (infer ctx e1, infer ctx e2)
        | [e] -> let t = infer ctx e in (t, t)
        | [] -> type_fail "Primitive operation has no expressions" 
      in
      begin match op with
        | Equals | NotEquals | LessThan | LessEqual | GreaterThan | GreaterEqual ->
            begin try unify t1 TInt; unify t2 TInt; TBool with 
                TypeError _ -> type_fail "Expressions in comparision operations must be of type TInt"
            end
        | Plus | Minus | Times | Div ->
            begin try unify t1 TInt; unify t2 TInt; TInt with 
                TypeError _ -> type_fail "Expressions in math operations must be of type TInt"
            end

        | And | Or -> 
            begin try unify t1 TBool; unify t2 TBool; TBool with 
                TypeError _ -> type_fail "Expressions in boolean operations must be of type TBool"
            end
        | Negate -> 
            begin try unify t1 TInt; TInt with 
                TypeError _ -> type_fail "Expression in the negate operation must be of type TInt"  
            end
      end  
  | Tuple es -> TProduct (List.map (infer ctx) es)
  | Fn (x, t, e) -> 
      begin match t with 
        | Some t -> TArrow (t, infer (extend ctx (x, t)) e)
        | None -> 
            let tv = 
              try ctx_lookup ctx x with
                NotFound -> fresh_tvar ()
            in
            remove_tvars (TArrow (tv, infer (extend ctx (x, tv)) e))
      end
  | Rec (f, t, e) -> infer (extend ctx (f, t)) e
  | Apply (f, e) -> 
      begin try 
          let ft = infer ctx f in 
          unify ft (TArrow (infer ctx e, fresh_tvar ()));
          try match ft with 
            | TArrow (_, t') -> remove_tvars t' 
            | _ -> raise (Invalid_argument "Illegal type for function") (* Should never get here *)
          with 
            TypeError _ -> type_fail "Function and expression have different types in Apply" 
        with
          TypeError _ -> type_fail "Invalid type for function" 
      end                
  | Anno (e, t) -> infer ctx e
  | Let (ds, e) -> 
      let rec infer_decs ds ctx =
        match ds with
        | [] -> ctx
        | d::ds' ->
            match d with 
            | Val (e, x) | ByName (e, x) -> 
                let t = infer ctx e in
                infer_decs ds' (extend ctx (x, t))
            | Valtuple (e, xs) -> 
                match infer ctx e with 
                | TProduct ts ->
                    let xt_pairs = List.combine xs ts in 
                    infer_decs ds' (extend_list ctx xt_pairs)
                | _ -> type_fail "Valtuple expression must be of type TProduct"        
      in
      infer (infer_decs ds ctx) e


(* Now you can play with the language that you've implemented! *)
let execute (s: string) : unit =
  match P.parse s with
  | Left s -> print_endline ("parsing failed: " ^ s)
  | Right e ->
      try
       (* first we type check the program *)
        ignore (infer (Ctx []) e);
        let result = eval e in
        print_endline ("program is evaluated to: " ^ Print.exp_to_string result)
      with
      | NotImplemented -> print_endline "code is not fully implemented"
      | Stuck s -> print_endline ("evaluation got stuck: " ^ s)
      | NotFound -> print_endline "variable lookup failed"
      | TypeError s -> print_endline ("type error: " ^ s)
      | e -> print_endline ("unknown failure: " ^ Printexc.to_string e)


(************************************************************
 *                     Tester template:                     *
 *         Codes to test your interpreter by yourself.      *
 *         You can change these to whatever you want.       *
 *                We won't grade these codes                *
 ************************************************************)
let list_to_string el_to_string l : string =
  List.fold_left
    begin fun acc el ->
      if acc = "" then
        el_to_string el
      else
        acc ^ "; " ^ el_to_string el
    end
    ""
    l
  |> fun str -> "[" ^ str ^ "]"

let run_test name f ts stringify : unit =
  List.iteri
    begin fun idx (input, expected_output) ->
      try
        let output = f input in
        if output <> expected_output then
          begin
            print_string (name ^ " test #" ^ string_of_int idx ^ " failed\n");
            print_string (stringify output ^ " <> " ^ stringify expected_output);
            print_newline ()
          end 
      with
      | exn ->
          print_string (name ^ " test #" ^ string_of_int idx ^ " raised an exception:\n");
          print_string (Printexc.to_string exn);
          print_newline ()
    end
    ts

let run_free_vars_tests () : unit =
  run_test "free_vars" free_vars free_vars_tests (list_to_string (fun x -> x))

let run_unused_vars_tests () : unit =
  run_test "unused_vars" unused_vars unused_vars_tests (list_to_string (fun x -> x))

let run_subst_tests () : unit =
  run_test "subst" (fun (s, e) -> subst s e) subst_tests Print.exp_to_string

let run_eval_tests () : unit =
  run_test "eval" eval eval_tests Print.exp_to_string

(* You may want to change this to use the unification (unify) instead of equality (<>) *)
let run_infer_tests () : unit =
  run_test "infer" (fun (ctx, e) -> infer ctx e) infer_tests Print.typ_to_string

let run_unify_tests () : unit =
  run_test "unify" (fun (ty1, ty2) -> unify ty1 ty2) unify_tests (fun () -> "()")

let run_all_tests () : unit =
  run_free_vars_tests ();
  run_unused_vars_tests ();
  run_subst_tests ();
  run_eval_tests ();
  run_infer_tests ();
  run_unify_tests ()