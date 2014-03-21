%{
    open ParseTree
%}
%token INCR_PTR_T DECR_PTR_T INCR_DATA_T DECR_DATA_T
%token PUT_CHR_T GET_CHR_T OPEN_LOOP_T CLOSE_LOOP_T
%token EOF_T
%start main
%type <ParseTree.stmt list> main
%%
main: stmts EOF_T { $1 };

stmts: stmt { [$1] } | stmt stmts { $1 :: $2 };

stmt:
    | INCR_PTR_T { INCR_PTR(1) }
    | DECR_PTR_T { INCR_PTR(-1) }
    | INCR_DATA_T { INCR_DATA(0, 1) }
    | DECR_DATA_T { INCR_DATA(0, -1) }
    | PUT_CHR_T { PUT_CHR }
    | GET_CHR_T { GET_CHR }
    | loop { $1 }
;

loop: OPEN_LOOP_T stmts CLOSE_LOOP_T { LOOP($2) };