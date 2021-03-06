/* Ocamlyacc parser for MicroC */

%{
open Ast

let fst (a,_,_) = a;;
let snd (_,b,_) = b;;
let trd (_,_,c) = c;;

%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LARRAY RARRAY COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN DOLLAR PERIOD
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE INT BOOL CHAR STRING FLOAT VOID STRUCT PACKET MESSAGE BYTE
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID FLIT
%token EOF
%token <char> CHAR_LITERAL
%token <string> STRING_LITERAL

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%right DOLLAR
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1, trd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1), trd $1 }
 | decls sdecl { fst $1, snd $1, ($2 :: trd $1) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT     { Int      }
  | BOOL    { Bool     }
  | FLOAT   { Float    }
  | VOID    { Void     }
  | CHAR    { Char     }
  | BYTE    { Byte     }
  | STRING  { String   }
  | array_t { $1       }
  | STRUCT ID { Struct ($2) }

sdecl:
    STRUCT ID LBRACE vdecl_list RBRACE SEMI
      {
        { sname = $2;
          svar = $4;
      }
    }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

array_t:
  typ LARRAY expr RARRAY { Array($1, $3) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
   literals          { $1                     }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID LARRAY expr RARRAY ASSIGN expr  { ArrayAssign($1, $3, $6) }
  | ID LARRAY expr RARRAY { ArrayAccess($1, $3) }
  | ID PERIOD ID     { Dereference($1, $3)    }
  | ID PERIOD ID ASSIGN expr { MemAssign($1, $3, $5) }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | DOLLAR expr      { Unop(Dollar, $2)       }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }

primitive_literals:
    LITERAL          { Literal($1)            }
  | FLIT             { Fliteral($1)           }
  | CHAR_LITERAL     { Char_literal($1)       }
  | STRING_LITERAL   { String_literal($1)     }
  | BLIT             { BoolLit($1)            }

literals:
  primitive_literals { $1 }
  | LARRAY array_literal RARRAY { ArrayLiteral(List.rev $2) }

array_literal:
  primitive_literals { [$1] }
  | array_literal COMMA primitive_literals { $3 :: $1}

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
