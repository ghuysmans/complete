let id = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let num = '0' | '-'? ['1'-'9']['0'-'9']*
let white = [' ' '\t']

rule top = parse
| white { top lexbuf }
| "+" { Parser.PLUS }
| num as n { Parser.LINT (int_of_string n) }
| "(" { Parser.LPAR }
| ")" { Parser.RPAR }
| eof { Parser.EOF }
