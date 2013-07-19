NO_DBUS ?= 0
CMX = main.cmx
GENERATED = 
BIN = postgres
PACKAGES = str,mysql,ssl,inotify,fdinfo,threads,smtp,syslog,postgresql,bigarray
FLAGS =-annot -thread -linkpkg -package $(PACKAGES)

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



