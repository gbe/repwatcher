CMX = lexer.cmx parser.cmx date.cmx log.cmx config.cmx txt_operations.cmx abstract_sql.cmx postgresqldb.cmx mysqldb.cmx sqldb.cmx pipe.cmx sendmail.cmx abstract_mail.cmx instantMail.cmx report.cmx files_progress.cmx ssl_connected_client.cmx ssl_handle_clients.cmx ssl_server.cmx dirs.cmx files.cmx inotifyCaller.cmx fileEvents.cmx directoryEvents.cmx eventsDispatcher.cmx offset_thread.cmx pipe_from_server_thread.cmx main.cmx
GENERATED = lexer.ml parser.ml parser.mli
BIN = rw_server
PACKAGES = str,mysql,postgresql,bigarray,ssl,inotify,fdinfo,threads,smtp,syslog,dbus,calendar
FLAGS =-thread -linkpkg -package $(PACKAGES)

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
	rm -f *.cm[ix] *.annot *.o *~ $(BIN) $(GENERATED) parser.output .depend

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep.opt *.ml *.mli > .depend

include .depend



