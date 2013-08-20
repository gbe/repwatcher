NO_DBUS ?= 0
CMX = lexer.cmx parser.cmx log.cmx config.cmx date.cmx txt_operations.cmx abstract_sql.cmx postgresqldb.cmx mysqldb.cmx sqldb.cmx pipe.cmx sendmail.cmx mail.cmx report.cmx files_progress.cmx ssl_server.cmx dirs.cmx files.cmx core.cmx check_conf.cmx events.cmx offset_thread.cmx pipe_from_server_thread.cmx main.cmx
GENERATED = lexer.ml parser.ml parser.mli
BIN = rw_server
PACKAGES = str,mysql,postgresql,bigarray,ssl,inotify,fdinfo,threads,smtp,syslog,dbus
FLAGS =-annot -thread -linkpkg -package $(PACKAGES)

all: $(BIN)

$(BIN):$(CMX)
	ocamlfind ocamlopt $(FLAGS) -o $(BIN) $(CMX)

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
	rm -f *.cm[ix] *.annot *.o *~ $(BIN) $(GENERATED) parser.output parser_conf.output .depend rw.log rw.log.err

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep.opt *.ml *.mli > .depend

include .depend



