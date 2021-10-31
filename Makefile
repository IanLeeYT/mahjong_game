# credits to CS3110 Professor MRC

MODULES= command roundState gameState main players tiles tutorial
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
NATIVE=main.native
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

byte:
	$(OCAMLBUILD) -tag 'debug' $(MAIN)

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

native:
	$(OCAMLBUILD) -tag 'debug' $(NATIVE) && OCAMLRUNPARAM=b ./$(NATIVE)

check:
	@bash check.sh

js:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" main.js

zip:
	zip mahjong.zip *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit *.md LICENSE Makefile	
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private mahjong.zip main.js *.cmx
