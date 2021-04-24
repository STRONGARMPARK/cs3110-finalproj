MODULES=evolution1d graphs
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
#TEST=test.byte
GRAPH=graphs.byte
MAIN=userint.byte
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

zip:
	zip schrodinger.zip *.ml* *.txt* _tags .merlin .ocamlformat Makefile	

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private schrodinger.zip
