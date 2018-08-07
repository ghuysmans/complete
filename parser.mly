%token PLUS
%left PLUS
%token <int> LINT
%token LPAR
%token RPAR
%token EOF
%type <int> e
%type <int> l
%start l
%%
e: a=LINT { a }
 | a=e PLUS b=e { a+b }
 | LPAR e=e RPAR { e }
l: e=e EOF { e }
