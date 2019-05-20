(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions, structs) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
                               ("printc", Char);
                               ("prints", String);
			                         ("printb", Bool);
			                         ("printf", Float);
                               ("printbig", Int);
                               ("raw", Int) ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    check_binds "local" func.locals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
  let rec check_assign lvaluet rvaluet err = match rvaluet with
       Array(t1,_) -> (match t1 with
            Int -> check_assign t1 Int err
            | _ -> raise (Failure err))
       | _ -> if lvaluet = rvaluet then lvaluet else raise (Failure err)
   in

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
	                StringMap.empty (globals @ func.formals @ func.locals )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let access_type = function
         Array(t, _) -> t
        | _ -> raise (Failure("illegal array access"))
    in

    let get_struct_name (s : string) = match (type_of_identifier s) with
           Struct n -> n
          | _ -> raise (Failure ("Invalid access(.) operation for " ^ s))
    in

    let find_struct (id : string) = (
        let rec find_svar = function
            [] -> raise (Failure ("Cannot find struct " ^ id))
          | sdecl :: _ when sdecl.sname = id -> sdecl.svar
          | _ :: tl -> find_svar tl
        in find_svar structs)
    in

    let get_smember_type (s : string) (e : string) = (
        let rec get_member_type = function
            [] -> raise (Failure ("Struct " ^ s ^ " does not have member " ^ e))
          | (ty, name) :: _ when name = e -> ty
          | _ :: tl -> get_member_type tl
        in get_member_type (find_struct s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal  l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Char_literal c -> (Char, SChar_literal c)
      | String_literal s -> (String, SString_literal s)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
      | Unop(op, e) as ex ->
          let (t, e') = expr e in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | Dollar when t = Int -> t
          | _ -> raise (Failure ("illegal unary operator " ^
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
      | Binop(e1, op, e2) as e ->
          let (t1, e1') = expr e1
          and (t2, e2') = expr e2 in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal | Neq            when same               -> Bool
          | Less | Leq | Greater | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))
      | Call(fname, args) as call ->
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e =
            let (et, e') = expr e in
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))

      | ArrayLiteral l -> check_array_types l
      | ArrayAccess(a, e) -> check_int_expr e; (type_of_identifier a, SArrayAccess(a, expr e, access_type (type_of_identifier a)))
      | ArrayAssign(var, idx, num) -> check_int_expr num; check_int_expr idx; (type_of_identifier var, SArrayAssign(var, expr idx, expr num))
      | Dereference (s, m) ->
        let n = get_struct_name s in
        let ty = get_smember_type n m in (ty, SDereference(s, m))

      | MemAssign (s, m, e) as ex ->
        let n = get_struct_name s in
        let lt = get_smember_type n m in
        let (rt, e') = expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
        in (check_assign lt rt err, SMemAssign(s, m, (rt, e')))

    and check_int_expr e =
      let (t', e') = expr e
      and err = "expected Int expression in " ^ string_of_expr e
      in if t' != Int then raise (Failure err) else ignore e'

    and get_arr_type e = match e with
        Literal(_) :: ss -> get_arr_type ss
      | [] -> Int
      | _ -> raise (Failure("Unsupported Array Type"))

    and check_array_types e =
      let t = get_arr_type e in
      let check_arr_el e = match e with
        Literal(i) -> if t == Int then expr(Literal(i)) else expr(Fliteral(string_of_int i))        | _ -> raise (Failure("arrays only ints"))
    in (Array (t, Literal(List.length e)), SArrayLiteral(List.map check_arr_el e, Array(t, Literal(List.length e))))

    in

    let check_bool_expr e =
      let (t', e') = expr e
        and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e')
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | For(e1, e2, e3, st) ->
	  SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise (
	  Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))

	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      slocals  = func.locals;
      sbody = match check_stmt (Block func.body) with
	SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  in

 (**** Check structs ****)
 let check_structs (st_list : struct_decl list) =
      (* check struct name *)
      let rec struct_dups = function
          [] -> ()
        | (s1 :: s2 :: _) when s1.sname = s2.sname ->
          raise (Failure ("duplicate struct name " ^ s1.sname))
        | _ :: tl -> struct_dups tl
      in struct_dups st_list;

      (* check struct body *)
      let struct_binds (st : struct_decl) =
        if List.length st.svar = 0 then raise (Failure ("Empty struct body"))
        else check_binds "struct" st.svar
      in List.iter struct_binds st_list;

      let check_struct (st : struct_decl) = {
        ssname = st.sname;
        ssvar = st.svar;
      }
  in List.map check_struct st_list

in (globals, List.map check_function functions, check_structs structs)

