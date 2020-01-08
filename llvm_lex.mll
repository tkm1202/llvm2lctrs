(* File lexer.mll *)
{
open Llvm_yacc        (* The type token is defined in parser.mli *)
exception Eof
let debug s = (*print_string s*)();;
(*let debug s;;*)
}
rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n']    		{ let ln = lexbuf.lex_curr_p.pos_lnum
                     and off = lexbuf.lex_curr_p.pos_cnum
                     in
                     lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                                            pos_lnum = ln+1; pos_bol = off};
                     debug("new line\n");token lexbuf
		   }
  | ['0'-'9']+ as lxm { debug("INT\n");INT(int_of_string lxm) }
  | "fneg"	     { debug("FNEG\n");FNEG }
  | "add"            { debug("ADD\n");ADD }
  | "fadd"           { debug("FADD\n");FADD }
  | "sub"            { debug("SUB\n");SUB }
  | "fsub"           { debug("FSUB\n");FSUB }
  | "mul"            { debug("MUL\n");MUL }
  | "fmul"           { debug("FMUL\n");FMUL }
  | "udiv"           { debug("UDIV\n");UDIV }
  | "sdiv"           { debug("SDIV\n");SDIV }
  | "fdiv"           { debug("FDIV\n");FDIV }
  | "urem"           { debug("UREM\n");UREM }
  | "srem"           { debug("SREM\n");SREM }
  | "frem"           { debug("FREM\n");FREM }
  | "exact"	     { debug("EXACT\n");EXACT }
  | "to"	     { debug("TO\n");TO }
  | '('            { debug("LPAREN\n");LPAREN }
  | ')'            { debug("RPAREN\n");RPAREN }
  | '{'	  	   { debug("LBRACE\n");LBRACE }
  | '}'	  	   { debug("RBRACE\n");RBRACE }
  | '['		   { debug("LBRACKET\n");LBRACKET }
  | ']'		   { debug("RBRACKET\n");RBRACKET }
  | ','		   { debug("COMMA\n");COMMA }
  | '*'		   { debug("ASTERISK\n");ASTERISK }
  | eof            { EOF }
  | "define"	   { debug("DEFINE\n");DEFINE }
  | "label"	   { debug("LABEL\n");LABEL }
  | "void"	   { debug("EMPTY\n");EMPTY }
  | ['@'] ['a'-'z' 'A'-'Z']+	{ debug("FID\n");FID (Lexing.lexeme lexbuf)}
  | ['%'] ['1'-'9'] ['0'-'9']*	{ debug("VID\n");VID (Lexing.lexeme lexbuf)}
  | ['%'] ['0']	{ debug("VID\n");VID (Lexing.lexeme lexbuf)}
  | ['i'] ['1'-'9'] ['0'-'9']*		{ debug("INTBIT\n");INTBIT }
  | ['#'] ['0'-'9']+		{ debug("ATTRNUM\n");ATTRNUM }
  | '='	  { debug("EQUAL\n");EQUAL }
  | "phi" { debug("PHI\n");PHI }
  | "br"  { debug("BRANCH\n");BRANCH }
  | "ret" { debug("RET\n");RET }
  | "switch"		       {debug("SWITCH\n");SWITCH}
  | [';'] [^'\n']* ['\n']	{token lexbuf} (* comment out *) 
  | "nuw" { debug("NUW\n");NUW }
  | "nsw" { debug("NSW\n");NSW }
  | "and" { debug("AND\n");AND }
  | "or"  { debug("OR\n");OR }
  | "xor" { debug("XOR\n");XOR }
  | "shl" { debug("SHL\n");SHL }
  | "lshr" { debug("LSHR\n");LSHR }
  | "ashr" { debug("ASHR\n");ASHR }
  | "alloca" { debug("ALLOCA\n");ALLOCA }
  | "align"  { debug("ALIGN\n");ALIGN }
  | "load"   { debug("LOAD\n");LOAD }
  | "store"  { debug("STORE\n");STORE }
  | "trunc"  { debug("TRUNC\n");TRUNC }
  | "zext"   { debug("ZEXT\n");ZEXT }
  | "sext"   { debug("SEXT\n");SEXT }
  | "fptrunc"  { debug("FPTRUNC\n");FPTRUNC }
  | "fpext"    { debug("FPEXT\n");FPEXT }
  | "fptoui"   { debug("FPTOUI\n");FPTOUI }
  | "fptosi"   { debug("FPTOSI\n");FPTOSI }
  | "uitofp"   { debug("UITOFP\n");UITOFP }
  | "sitofp"   { debug("SITOFP\n");SITOFP }
  | "icmp"     { debug("ICMP\n");ICMP }
  | "fcmp"     { debug("FCMP\n");FCMP }
  | "call"     { debug("CALL\n");CALL }
  | "eq"       { debug("EQ\n");EQ }
  | "ne"       { debug("NE\n");NE }
  | "ugt"      { debug("UGT\n");UGT }
  | "ult"      { debug("ULT\n");ULT }
  | "ule"      { debug("ULE\n");ULE }
  | "sgt"      { debug("SGT\n");SGT }
  | "sle"      { debug("SGE\n");SGE }
  | "slt"      { debug("SLT\n");SLT }
  | "sle"      { debug("SLE\n");SLE }
  | "false"    { debug("FALSE\n");FALSE }
  | "oeq"      { debug("OEQ\n");OEQ }
  | "ogt"      { debug("OGT\n");OGT }
  | "oge"      { debug("OGE\n");OGE }
  | "olt"      { debug("OLT\n");OLT }
  | "ole"      { debug("OLE\n");OLE }
  | "one"      { debug("ONE\n");ONE }
  | "ord"      { debug("ORD\n");ORD }
  | "ueq"      { debug("UEQ\n");UEQ }
  | "uge"      { debug("UGE\n");UGE }
  | "une"      { debug("UNE\n");UNE }
  | "uno"      { debug("UNO\n");UNO }
  | "true"     { debug("TRUE\n");TRUE }
  | "local_unnamed_addr"	      { debug("LUA\n");LUA }
  | "unreachable"		      { debug("UNREACHABLE\n");UNREACHABLE }
  | ['a'-'z' 'A'-'Z' '0'-'9']+	{ debug("LABELID\n");LABELID (Lexing.lexeme lexbuf)}