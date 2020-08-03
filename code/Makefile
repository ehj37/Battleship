MODULES=gui
UNITS=state command cpu authors
OBJECTS=$(MODULES:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
MLS_WITHOUT_MLIS= main gui items test 
MLIS=$(UNITS:=.mli)
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
PKGS=oUnit,str,ANSITerminal

default: build
	utop

build:
		ocamlbuild -use-ocamlfind gui.cmo main.cmo state.cmo command.cmo cpu.cmo items.cmo authors.cmo

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private