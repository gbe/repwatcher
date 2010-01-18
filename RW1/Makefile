CMO= lexer_conf.cmo parser_conf.cmo configuration.cmo gen_liste_fichiers.cmo sql.cmo main.cmo
GENERATED = lexer_conf.ml parser_conf.ml parser_conf.mli
BIN=repwatcher
FLAGS=# -dtypes
VERSION=`ls /usr/lib/ocaml/`

all: $(BIN)

$(BIN):$(CMO)
	ocamlc.opt $(FLAGS) -o $(BIN) -I /usr/lib/ocaml/$(VERSION)/mysql/ mysql.cma unix.cma str.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly  

.mli.cmi:
	ocamlc.opt $(FLAGS) -I /usr/lib/ocaml/$(VERSION)/mysql/ -c $<

.ml.cmo:
	ocamlc.opt $(FLAGS) -I /usr/lib/ocaml/$(VERSION)/mysql/ -c $<

.mll.ml:
	ocamllex.opt $<

.mly.ml:
	ocamlyacc -v $<

.mly.mli:
	ocamlyacc -v $<

clean:
	rm -f *.cm[io] *.annot *.o *~ $(BIN) $(GENERATED) parser_conf.output

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend



