(* Yousef Elsendiony - YE2194 *)
{
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
}

let digit = ['0' - '9']
let digits = digit+

rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip whitespace *)
  | "/*"           { comment lexbuf }   (* comments *)


  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ';'            { SEMI }
  | ','            { COMMA }

  | ['\n' ]        { EOL }



  | digit+ as lxm { INT(int_of_string lxm) }

  (* Basic Arithmetic Operators*)
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }

  (* Relationships *)
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | "&&"     { AND }
  | "||"     { OR }
  | "!"      { NOT }


  | eof            { raise Eof }
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
  
  and comment = parse
    "*/" { token lexbuf }
    | _   { comment lexbuf }
