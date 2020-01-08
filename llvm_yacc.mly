/* File parser.mly */
%{
open Ast ;;
let debug s = (*print_string s*)();;
let table = ref [];;(*table := 'a::!table;;*)
%}

%token <int> EOL
%token <int> INT
%token ADD SUB MUL UDIV SDIV UREM SREM
%token FNEG FADD FSUB FMUL FDIV FREM
%token EXACT ALIGN TO
%token NUW NSW
%token AND OR XOR
%token SHL LSHR ASHR
%token ALLOCA LOAD STORE
%token TRUNC ZEXT SEXT FPTRUNC FPEXT FPTOUI FPTOSI UITOFP SITOFP
%token ICMP FCMP CALL
%token EQ NE UGT ULT ULE SGT SGE SLT SLE
%token TRUE FALSE
%token OEQ OGT OGE OLT OLE ONE ORD UEQ UGE UNE UNO
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DEFINE LABEL
%token ATTRNUM INTBIT
%token <string> FID
%token <string> VID
%token <string> LABELID
%token EQUAL COMMA ASTERISK
%token PHI
%token EMPTY
%token BRANCH RET SWITCH
%token LUA UNREACHABLE
%token EOF
%start program             /* the entry point */
%type <Ast.fundecl list> program
%%

linecounter:
	EOL		{table := $1::!table}
;

program:
    deflist                { $1 }
;
deflist:
	{[]}
	| def deflist	{$1::$2}
;

def:
	DEFINE typ FID LPAREN typelist RPAREN ATTRNUM LBRACE blocks RBRACE	{debug("def\n");($2,$3,$5,$9)}
	|DEFINE typ FID LPAREN typelist RPAREN LUA ATTRNUM LBRACE blocks RBRACE	{debug("def\n");($2,$3,$5,$10)}

;

typ:	INTBIT {debug("typ\n");I 32}
	|EMPTY {debug("typ\n");I 0}
;

typelist: {debug("typelist\n");[]}
	  |typ typelist {debug("typelist\n"); $1::$2}
;

blocks:
	{[]}
	|block blocks{$1::$2};
block:
/*	| phi inst terminate block	{debug("block\n");0}*/
	 VID EQUAL PHI typ phinode block {debug("phi\n");
	 let Block(ps,is,t) = $6
	 in Block((Phi($4,$5))::ps,is,t)
	 }
	 | inst terminate {debug("block\n");Block([],$1,$2)}
	| UNREACHABLE	     		{debug("block\n");raise (Failure "unreachable")}
;

/*phi:
	;{ debug("phi\n");0}
	;| VID EQUAL PHI typ phinode {debug("phi\n");0}
;
*/

phinode:
	LBRACKET expr COMMA label RBRACKET	{debug("phinode\n");[Phinode($2,$4)]}
	|LBRACKET expr COMMA label RBRACKET COMMA phinode	{debug("phinode\n");(Phinode($2,$4))::$7}
;

label:
	VID	{Labelint 0(*Labelint(int_of_string($1))*)}
	|LABELID	{Labelvar($1)}
expr:
	VID	{debug("expr\n");Var($1)}
	|INT	{debug("expr\n");Int($1)}
;

terminate:
	branch {debug("terminate\n");$1}
	|return {debug("terminate\n");$1}
	|switch {debug("terminate\n");$1}
;

branch:
	BRANCH typ VID COMMA LABEL label COMMA LABEL label {debug("branch\n");Branch($2,$3,$6,$9)}
	|BRANCH LABEL label {debug("branch\n");Goto($3)}
;

return:
	RET typ expr { debug("ret\n");Return (Some($2,$3)) }
	|RET EMPTY { debug("ret\n");Return None }
;

switch:
	SWITCH typ VID COMMA LABEL label LBRACKET switchbr RBRACKET { debug("switch\n");Switch($2,$3,$6,$8) }
;

switchbr:
	typ VID COMMA LABEL label { debug("switchbr\n");[($1,$2,$5)] }
	|typ VID COMMA LABEL label switchbr { debug("switchbr\n");(($1,$2,$5))::$6}
;

inst:
	{ debug("inst\n");[] }
	|unaryop inst		   { debug("inst\n");$1::$2 }
	|binaryop inst		   { debug("inst\n");$1::$2 }
	|bitop inst	  	   { debug("inst\n");$1::$2 }
	|memoryop inst	   	   { debug("inst\n");$1::$2 }
	|convop inst		   { debug("inst\n");$1::$2 }
	|otherop inst		   { debug("inst\n");$1::$2 }
;

unaryop:
	VID EQUAL FNEG typ expr	{ debug("unaryop\n");Fneg($1,$4,$5) }
;

binaryop:
	add	{ debug("binaryop\n");$1 }
	|sub	{ debug("binaryop\n");$1 }
	|mul	{ debug("binaryop\n");$1 }
	|div	{ debug("binaryop\n");$1 }
	|rem	{ debug("binaryop\n");$1 }
;

add:
	VID EQUAL ADD wrap typ expr COMMA expr	{ debug("add\n");Add($1,$4,$5,$6,$8) }
	|VID EQUAL FADD typ expr COMMA expr	{ debug("fadd\n");Fadd($1,$4,$5,$7) }
;

sub:
	VID EQUAL SUB wrap typ expr COMMA expr	{ debug("sub\n");Sub($1,$4,$5,$6,$8) }
	|VID EQUAL FSUB typ expr COMMA expr	{ debug("fsub\n");Fsub($1,$4,$5,$7) }
;

mul:
	VID EQUAL MUL wrap typ expr COMMA expr	{ debug("mul\n");Mul($1,$4,$5,$6,$8) }
	|VID EQUAL FMUL typ expr COMMA expr	{ debug("fmul\n");Fmul($1,$4,$5,$7) }
;

div:
	VID EQUAL UDIV typ expr COMMA expr	{ debug("udiv\n");Udiv($1,false,$4,$5,$7) }
	|VID EQUAL UDIV EXACT typ expr COMMA expr { debug("udiv\n");Udiv($1,true,$5,$6,$8) }
	|VID EQUAL SDIV typ expr COMMA expr	{ debug("sdiv\n");Sdiv($1,false,$4,$5,$7) }
	|VID EQUAL SDIV EXACT typ expr COMMA expr { debug("sdiv\n");Sdiv($1,true,$5,$6,$8) }
	|VID EQUAL FDIV typ expr COMMA expr	{ debug("fdiv\n");Fdiv($1,$4,$5,$7) }
;

rem:
	VID EQUAL UREM typ expr COMMA expr	{ debug("urem\n");Urem($1,$4,$5,$7) }
	|VID EQUAL SREM typ expr COMMA expr	{ debug("srem\n");Srem($1,$4,$5,$7) }
	|VID EQUAL FREM typ expr COMMA expr	{ debug("frem\n");Frem($1,$4,$5,$7) }
;

wrap:
	{ debug("wrap\n");Nothing }
	|NUW		    { debug("wrap\n");Nuw }
	|NSW		    { debug("wrap\n");Nsw }
	|NUW NSW	    { debug("wrap\n");NuwNsw }
;

bitop:
	shl	{ debug("bitop\n");$1 }
	|shr	{ debug("bitop\n");$1 }
	|VID EQUAL AND typ expr COMMA expr	{ debug("bitop\n");And($1,$4,$5,$7) }
	|VID EQUAL OR typ expr COMMA expr	{ debug("bitop\n");Or($1,$4,$5,$7) }
	|VID EQUAL XOR typ expr COMMA expr	{ debug("bitop\n");Xor($1,$4,$5,$7) }
;

shl:
	VID EQUAL SHL wrap typ expr COMMA expr	{ debug("shl\n");Shl($1,$4,$5,$6,$8) }
;

shr:
	VID EQUAL LSHR typ expr COMMA expr		{ debug("lshr\n");Lshr($1,false,$4,$5,$7) }
	|VID EQUAL LSHR EXACT typ expr COMMA expr	{ debug("lshr\n");Lshr($1,true,$5,$6,$8) }
	|VID EQUAL ASHR typ expr COMMA expr		{ debug("ashr\n");Ashr($1,false,$4,$5,$7) }
	|VID EQUAL ASHR EXACT typ expr COMMA expr	{ debug("ashr\n");Ashr($1,true,$5,$6,$8) }
;

memoryop:
	alloca	{ debug("memoryop\n");$1 }
	|load	{ debug("memoryop\n");$1 }
	|store	{ debug("memoryop\n");$1 }
;

alloca:
	VID EQUAL ALLOCA typ					{ debug("alloca\n");Alloca($1,$4,None,None) }
	|VID EQUAL ALLOCA typ COMMA typ INT			{ debug("alloca\n");Alloca($1,$4,(Some($6,$7)),None) }
	|VID EQUAL ALLOCA typ COMMA ALIGN INT			{ debug("alloca\n");Alloca($1,$4,None,(Some $7)) }
	|VID EQUAL ALLOCA typ COMMA typ INT COMMA ALIGN INT	{ debug("alloca\n");Alloca($1,$4,(Some($6,$7)),(Some $10)) }
;

load:
	VID EQUAL LOAD typ COMMA typ ASTERISK VID	{ debug("load\n");Load($1,$4,$6,$8,None) }
	|VID EQUAL LOAD typ COMMA typ ASTERISK VID COMMA ALIGN INT	    { debug("load\n");Load($1,$4,$6,$8,(Some $11)) }
;

store:
	STORE typ expr COMMA typ ASTERISK VID	{ debug("store\n");Store($2,$3,$5,$7,None) }
	|STORE typ expr COMMA typ ASTERISK VID COMMA ALIGN INT	{ debug("store\n");Store($2,$3,$5,$7,(Some $10)) }
;

convop:
	VID EQUAL TRUNC typ VID TO typ			{ debug("trunc\n");Trunc($1,$4,$5,$7) }
	|VID EQUAL ZEXT typ VID TO typ			{ debug("zext\n");Zext($1,$4,$5,$7) }
	|VID EQUAL SEXT typ VID TO typ			{ debug("sext\n");Sext($1,$4,$5,$7) }
	|VID EQUAL FPTRUNC typ VID TO typ		{ debug("fptrunc\n");Fptrunc($1,$4,$5,$7) }
	|VID EQUAL FPEXT typ VID TO typ			{ debug("fpext\n");Fpext($1,$4,$5,$7) }
	|VID EQUAL FPTOUI typ VID TO typ		{ debug("fptoui\n");Fptoui($1,$4,$5,$7) }
	|VID EQUAL FPTOSI typ VID TO typ		{ debug("fptosi\n");Fptosi($1,$4,$5,$7) }
	|VID EQUAL UITOFP typ VID TO typ		{ debug("uitofp\n");Uitofp($1,$4,$5,$7) }
	|VID EQUAL SITOFP typ VID TO typ		{ debug("sitofp\n");Sitofp($1,$4,$5,$7) }
;

otherop:
	VID EQUAL ICMP icmpcond typ expr COMMA expr	{ debug("otherop\n");Icmp($1,$4,$5,$6,$8) }
	|VID EQUAL FCMP fcmpcond typ expr COMMA expr	{ debug("otherop\n");Fcmp($1,$4,$5,$6,$8) }
	|VID EQUAL CALL typ FID LPAREN arglist RPAREN	{ debug("otherop\n");Call($1,$4,$5,$7) }
;

arglist:
	{ debug("arglist\n");[] }
	|arg arglist	{ debug("arglist\n");$1::$2 }
;

arg:
	typ expr	{ debug("arg\n");($1,$2) }
	|typ expr COMMA	{ debug("arg\n");($1,$2) }
;

icmpcond:
	EQ	{ debug("icmpcond\n");Eq }
	|NE	{ debug("icmpcond\n");Ne }
	|UGT	{ debug("icmpcond\n");Ugt }
	|ULT	{ debug("icmpcond\n");Ult }
	|ULE	{ debug("icmpcond\n");Ule }
	|SGT	{ debug("icmpcond\n");Sgt }
	|SGE	{ debug("icmpcond\n");Sge }
	|SLT	{ debug("icmpcond\n");Slt }
	|SLE	{ debug("icmpcond\n");Sle }
;

fcmpcond:
	FALSE	{ debug("fcmpcond\n");False }
	|OEQ	{ debug("fcmpcond\n");Oeq }
	|OGT	{ debug("fcmpcond\n");Ogt }
	|OGE	{ debug("fcmpcond\n");Oge }
	|OLT	{ debug("fcmpcond\n");Olt }
	|OLE	{ debug("fcmpcond\n");Ole }
	|ONE	{ debug("fcmpcond\n");One }
	|ORD	{ debug("fcmpcond\n");Ord }
	|UEQ	{ debug("fcmpcond\n");Ueq }
	|UGT	{ debug("fcmpcond\n");Ugt }
	|UGE	{ debug("fcmpcond\n");Uge }
	|ULT	{ debug("fcmpcond\n");Ult }
	|ULE	{ debug("fcmpcond\n");Ule }
	|UNE	{ debug("fcmpcond\n");Une }
	|UNO	{ debug("fcmpcond\n");Uno }
	|TRUE	{ debug("fcmpcond\n");True }
;
	