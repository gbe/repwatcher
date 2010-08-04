CMX= rw_client.cmx
GENERATED = 
BIN= rw_client
FLAGS=-dtypes -linkpkg -package ssl,dbus

all: $(BIN)

$(BIN):$(CMX)
	ocamlfind ocamlopt $(FLAGS) str.cmxa -I +threads threads.cmxa -I +ssl ssl_threads.cmxa -o $(BIN) $(CMX)

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
	rm -f a.out main.cmi *.cm[ix] *.annot *.o *~ .depend $(BIN) $(GENERATED)

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep.opt *.ml *.mli > .depend

include .depend



