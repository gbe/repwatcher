CMX= lexer_conf.cmx parser_conf.cmx configuration.cmx file_list.cmx ssl_server.cmx main.cmx
GENERATED = lexer_conf.ml parser_conf.ml parser_conf.mli
BIN=ino
FLAGS=-dtypes -linkpkg -package mysql,ssl,inotify

all: $(BIN)

$(BIN):$(CMX)
	ocamlfind ocamlopt $(FLAGS) str.cmxa -I +threads threads.cmxa -I +ssl ssl_threads.cmxa -o $(BIN) $(CMX)

.SUFFIXES: .mli .ml .cmi .cmx .mll .mly  

.mli.cmi:
	ocamlfind ocamlopt $(FLAGS) -c $<

.ml.cmx:
	ocamlfind ocamlopt $(FLAGS) -I +threads threads.cmxa -c $<

.mll.ml:
	ocamllex.opt $<

.mly.ml:
	ocamlyacc -v $<

.mly.mli:
	ocamlyacc -v $<

clean:
	rm -f main.cmi *.cm[ix] *.annot *.o *~ $(BIN) $(GENERATED) parser_conf.output .depend log.txt

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep.opt *.ml *.mli > .depend

include .depend



