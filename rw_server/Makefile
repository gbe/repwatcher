NO_DBUS ?= 0
CMX = lexer.cmx parser.cmx config.cmx date.cmx log.cmx mysqldb.cmx txt_operations.cmx pipe.cmx sendmail.cmx mail.cmx report.cmx files_progress.cmx ssl_server.cmx dirs.cmx files.cmx core.cmx check_conf.cmx events.cmx offset_thread.cmx pipe_from_server_thread.cmx main.cmx
GENERATED = lexer.ml parser.ml parser.mli
BIN = rw_server
PACKAGES = str,mysql,ssl,inotify,fdinfo,threads,smtp,syslog
FLAGS = -thread -linkpkg -package $(PACKAGES)

ifeq ($(NO_DBUS), 0)
	PACKAGES +=-package dbus
endif

all: $(BIN)

$(BIN):$(CMX)
	ocamlfind ocamlopt $(FLAGS) -o $(BIN) $(CMX)

.SUFFIXES: .mli .ml .cmi .cmx .mll .mly  

.mli.cmi:
	ocamlfind ocamlopt $(FLAGS) -c $<

.ml.cmx:
ifeq ($(NO_DBUS), 1)
	ocamlfind ocamlopt $(FLAGS) -pp "camlp4of.opt -DNO_DBUS" -c $<
else
	ocamlfind ocamlopt $(FLAGS) -pp "camlp4of.opt" -c $<
endif

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
ifeq ($(NO_DBUS), 1)
	ocamldep.opt -pp "camlp4of.opt -DNO_DBUS" *.ml *.mli > .depend
else
	ocamldep.opt -pp "camlp4of.opt" *.ml *.mli > .depend
endif

include .depend



