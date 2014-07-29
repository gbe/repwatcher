CMX= lexer.cmx parser.cmx date.cmx config.cmx dbus_call.cmx rw_client.cmx
GENERATED = lexer.ml parser.ml parser.mli 
BIN= rw_client
FLAGS=-thread -linkpkg -package str,ssl,threads,dbus,fdinfo,calendar

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
	rm -f a.out main.cmi *.cm[ix] *.annot *.o *~ .depend parser.output $(BIN) $(GENERATED)

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep.opt *.ml *.mli > .depend

include .depend



