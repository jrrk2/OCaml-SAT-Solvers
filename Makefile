all: test test.top test.opt

test: *.ml
	ocamlc -o $@ cnf.ml dpll.ml generator.ml test.ml

test.top: *.ml
	ocamlmktop -o $@ cnf.ml dpll.ml generator.ml

test.opt: *.ml
	ocamlopt.opt -o $@ cnf.ml dpll.ml generator.ml test.ml

clean:
	@echo "[CLEAN]"
	-rm -f test test.top
	-find . -name "*.cm[oix]" -exec rm {} \;
	-find . -name "*.cm[t]" -exec rm {} \;
	-find . -name "*.cmt[i]" -exec rm {} \;
