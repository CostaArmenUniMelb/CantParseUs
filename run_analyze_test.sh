ocamlc -c snick_ast.ml
ocamlc -c snick_analyze.ml
ocamlc -c snick_analyze_test.ml
ocamlc -o snick_test_analyze snick_ast.cmo snick_analyze.cmo snick_analyze_test.cmo
./snick_test_analyze
