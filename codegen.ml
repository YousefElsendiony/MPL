(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions, structs) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context (* 32 bit for integer *)
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context (* 1 bit for Bool *)
  and array_t    = L.array_type          (* returns the array type containing n elements *)
  and float_t    = L.double_type context
  and void_t     = L.void_type   context in

  let literal_to_val = function
    A.Literal(i) -> i
    | _ -> raise(Failure("Array type mismatched"))
  in

  (* Return the LLVM type for a MicroC type. AST to llvm type. *)
  let rec ltype_of_typ = function
      A.Int         -> i32_t
    | A.Byte        -> i32_t
    | A.Bool        -> i1_t
    | A.Float       -> float_t
    | A.Void        -> void_t
    | A.Char        -> i8_t
    | A.String      -> L.pointer_type i8_t
    | A.Array(typ, size) -> (match typ with
                                A.Int -> array_t i32_t (literal_to_val size)
                              | _ -> raise(Failure("Arrays working for int only")))
    | A.Pointer(t)  -> L.pointer_type (ltype_of_typ t)
    | A.Struct e    -> L.pointer_type (L.named_struct_type context e)
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let struct_decls : (L.lltype * sstruct_decl) StringMap.t =
      let define_struct m sdecl = 
        let get_sbody_ty ((ty, _) : A.bind) = ltype_of_typ ty in
        let sname = sdecl.ssname
        and svar_type = Array.of_list (List.map get_sbody_ty sdecl.ssvar) in
        let stype = L.named_struct_type context sname in
        L.struct_set_body stype svar_type false;
        StringMap.add sname (stype, sdecl) m in
      List.fold_left define_struct StringMap.empty structs
  in

  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
      L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
      L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
    and char_format_str = L.build_global_stringptr "%c\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    and raw_format_str = L.build_global_stringptr "0x%x\n" "fmt" builder
    in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
  let local_var = match t with
          A.Struct e -> 
            let (sty, _) = StringMap.find e struct_decls in
            let ptr = L.build_alloca (L.pointer_type sty) n builder in
            let sbody = L.build_malloc sty (n ^ "_body") builder in
            ignore(L.build_store sbody ptr builder); ptr
        | _ -> L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty (fdecl.sformals @ fdecl.slocals)
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
     SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SChar_literal c -> L.const_int i8_t (Char.code c)
      | SString_literal s -> L.build_global_stringptr s "tmp" builder
      | SNoexpr     -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SArrayLiteral (l, t) -> L.const_array (ltype_of_typ t) (Array.of_list (List.map (expr builder) l))
      | SAssign (s, e) -> let e' = expr builder e in
                          ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
            let e1' = expr builder e1
            and e2' = expr builder e2 in
                  (match op with
                    A.Add     -> L.build_fadd
                  | A.Sub     -> L.build_fsub
                  | A.Mult    -> L.build_fmul
                  | A.Div     -> L.build_fdiv
                  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
                  | A.Neq     -> L.build_fcmp L.Fcmp.One
                  | A.Less    -> L.build_fcmp L.Fcmp.Olt
                  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
                  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
                  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
                  | A.And | A.Or ->
                      raise (Failure "internal error: semant should have rejected and/or on float")
                  ) e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
            let e1' = expr builder e1
            and e2' = expr builder e2 in
              (match op with
                A.Add     -> L.build_add
              | A.Sub     -> L.build_sub
              | A.Mult    -> L.build_mul
              | A.Div     -> L.build_sdiv
              | A.And     -> L.build_and
              | A.Or      -> L.build_or
              | A.Equal   -> L.build_icmp L.Icmp.Eq
              | A.Neq     -> L.build_icmp L.Icmp.Ne
              | A.Less    -> L.build_icmp L.Icmp.Slt
              | A.Leq     -> L.build_icmp L.Icmp.Sle
              | A.Greater -> L.build_icmp L.Icmp.Sgt
              | A.Geq     -> L.build_icmp L.Icmp.Sge
              ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
            let e' = expr builder e in
                (match op with
                  A.Neg when t = A.Float -> L.build_fneg
                | A.Dollar               -> L.build_neg
                | A.Neg                  -> L.build_neg
                | A.Not                  -> L.build_not) e' "tmp" builder
      | SArrayAccess (s, e, _) -> L.build_load (get_array_acc_address s e builder) s builder
      | SArrayAssign (s, ea, eb) ->
            let lsb = get_array_acc_address s ea builder in
            let msb = expr builder eb in
                  ignore (L.build_store msb lsb builder); msb
      | SDereference (s, m) -> let mem_p = get_smem_ptr builder s m in
        L.build_load mem_p (s ^ m) builder
      | SMemAssign (s, m, e) ->
        let e' = expr builder e in
        let mem_p = get_smem_ptr builder s m in
        ignore(L.build_store e' mem_p builder); e'
      | SCall ("raw", [e]) ->
        L.build_call printf_func [| raw_format_str ; (expr builder e) |]
          "printf" builder
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
	  L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
      | SCall ("printc", [e]) ->
    L.build_call printf_func [| char_format_str; (expr builder e)|] "printf" builder
      | SCall ("prints", [e]) ->
    L.build_call printf_func [| string_format_str; (expr builder e)|] "printf" builder
      | SCall ("printbig", [e]) ->
	  L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) ->
	  L.build_call printf_func [| float_format_str ; (expr builder e) |]
	    "printf" builder
      | SCall (f, args) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder and 
         get_array_acc_address s e1 builder = L.build_gep (lookup s)
          [| (L.const_int i32_t 0); (expr builder e1) |] s builder

    (* get the member m's pointer in struct *)
    and get_smem_ptr builder s m =
          let sname = match (type_of_identifier s) with
                A.Struct n -> n
              | _ -> raise (Failure ("Invalid access(.) operation for " ^ s)) in
          let (_, sdecl) = StringMap.find sname struct_decls in
          let idx =
            let rec find_idx = function
                [] -> raise (Failure ("Struct " ^ sname ^ " does not have member " ^ m))
              | (_, var) :: _ when var = m -> 0
              | _ :: tl -> 1 + find_idx tl
            in find_idx sdecl.ssvar in
          let struct_p = L.build_load (lookup s) s builder in
          L.build_struct_gep struct_p idx (sname ^ m) builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
	SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
