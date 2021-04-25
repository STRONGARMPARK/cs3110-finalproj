MODULES=evolution graphs testing
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
#TEST=test.byte
GRAPH=graphs.byte
MAIN=userint.byte
TESTING=testing.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

graph:
	$(OCAMLBUILD) -tag 'debug' $(GRAPH) && OCAMLRUNPARAM=b ./$(GRAPH)

go:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

try: 
	$(OCAMLBUILD) -tag 'debug' $(TESTING) && OCAMLRUNPARAM=b ./$(TESTING)

zip:
	zip schrodinger.zip *.ml* *.txt* _tags .merlin .ocamlformat Makefile	

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private schrodinger.zip
