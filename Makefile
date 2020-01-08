test: ast.ml llvm_lex.mll llvm_yacc.mly main.ml
	ocamlc -c ast.ml
	ocamllex llvm_lex.mll       # generates lexer.ml
	ocamlyacc llvm_yacc.mly     # generates parser.ml and parser.mli
	ocamlc -c llvm_yacc.mli
	ocamlc -c llvm_lex.ml
	ocamlc -c llvm_yacc.ml
	ocamlc -c main.ml
	ocamlc -o test ast.cmo llvm_lex.cmo llvm_yacc.cmo main.cmo
