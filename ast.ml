(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not | Dollar



type expr =
    Literal of int
  | Fliteral of string
  | BoolLit of bool
  | ArrayLiteral of expr list
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr
  | Char_literal of char
  | String_literal of string
  | ArrayAssign of string * expr * expr
  | ArrayAccess of string * expr
  | Dereference of string * string
  | MemAssign of string * string * expr


type typ = Int | Bool | Float | Void | Char | String | Pointer of typ | Struct of string | Byte | Array of typ * expr

type bind = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type struct_decl = {
    sname: string;
    svar: bind list;
 }


type program = bind list * func_decl list * struct_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"
  | Dollar -> "$"

  let convert_array l conversion joiner =
    let glob_item original data = original ^ (conversion data) ^ joiner in
    let full = (List.fold_left glob_item "" l) in
    "[" ^ String.sub full 0 ((String.length full) - 2) ^ "]"


let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLiteral(el) -> "[" ^ String.concat ", " (List.map (fun e -> string_of_expr e) el) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> 
      let converted_string = match o with 
        Dollar  ->  Printf.sprintf "%X" (int_of_string (string_of_expr e))
        | _     ->  string_of_expr e
      in  string_of_uop o ^ converted_string
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | Char_literal(l) -> Char.escaped l
  | String_literal(l) -> l
  | ArrayAccess(a, e) -> a ^ "[" ^ string_of_expr e ^ "]"
  | ArrayAssign(a, e1, e2) -> a ^ "[" ^ string_of_expr e1 ^ "] = " ^ string_of_expr e2
  | Dereference (s, e) -> s ^ "." ^ e
  | MemAssign (s, m, e) -> s ^ "." ^ m ^ " = " ^ (string_of_expr e)

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec repeat c = function
  0 -> ""
| n -> c ^ (repeat c (n - 1))

let rec string_of_typ = function
    Int   -> "int"
  | Bool  -> "bool"
  | Float -> "float"
  | Void  -> "void"
  | Char  -> "char"
  | Byte  -> "byte"
  | String-> "string"
  | Pointer(t) -> string_of_typ t ^ " *"
  | Struct(id) -> "struct" ^ id
  | Array(t, e) -> string_of_typ t ^ "[" ^ string_of_expr e ^ "]"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

  let string_of_sdecl sdecl =
    "struct" ^ " " ^ sdecl.sname ^ "{\n" ^
    String.concat "" (List.map string_of_vdecl sdecl.svar) ^ "};\n"

let string_of_program (vars, funcs, structs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_sdecl structs)
