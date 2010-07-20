CMX= lexer_conf.cmx parser_conf.cmx configuration.cmx gen_liste_fichiers.cmx sql.cmx main.cmx
GENERATED = lexer_conf.ml parser_conf.ml parser_conf.mli
BIN=repwatcher
FLAGS=-linkpkg -package mysql# -dtypes

all: $(BIN)

$(BIN):$(CMX)
	ocamlfind ocamlopt $(FLAGS) str.cmxa unix.cmxa -o $(BIN) $(CMX)

.SUFFIXES: .mli .ml .cmi .cmx .mll .mly  

.mli.cmi:
	ocamlfind ocamlopt $(FLAGS) -c $<

.ml.cmx:
	ocamlfind ocamlopt $(FLAGS) -c $<

.mll.ml:
	ocamllex.opt $<

.mly.ml:
	ocamlyacc -v $<

.mly.mli:
	ocamlyacc -v $<

clean:
	rm -f *.cm[ix] *.annot *.o *~ $(BIN) $(GENERATED) parser_conf.output .depend

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



