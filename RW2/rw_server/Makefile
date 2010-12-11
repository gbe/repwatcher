CMX= lexer.cmx parser.cmx config.cmx file_list.cmx date.cmx mysqldb.cmx txt_operations.cmx pipe.cmx report.cmx files_progress.cmx ssl_server.cmx dirs.cmx core.cmx pipe_listening.cmx main.cmx
GENERATED= lexer.ml parser.ml parser.mli
BIN=rw_server
FLAGS=-dtypes -linkpkg -package mysql,ssl,inotify,dbus

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
	rm -f *.cm[ix] *.annot *.o *~ $(BIN) $(GENERATED) parser.output parser_conf.output .depend rw.log rw.log.err

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep.opt *.ml *.mli > .depend

include .depend



