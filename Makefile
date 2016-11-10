OCAMLBUILD=ocamlbuild

LANGS = \
	src/check \

SRCDIR = src

BYTETARGETS = $(LANGS:=.byte)
NATIVETARGETS = $(LANGS:=.native)

.PHONY: native byte

default: native

native: $(NATIVETARGETS)

byte: $(BYTETARGETS)

$(NATIVETARGETS): %.native : %.ml
	$(OCAMLBUILD) -use-ocamlfind -pkg ocamlgraph -pkg Z3 -tag thread -I $(SRCDIR) $@

$(BYTETARGETS): %.byte : %.ml
	$(OCAMLBUILD) -use-ocamlfind -pkg ocamlgraph -pkg Z3 -tag thread -use-menhir -libs unix -I (SRCDIR) $@
clean:
	$(OCAMLBUILD) -clean

#all:
#	ocamlbuild src/* -use-ocamlfind -pkg ocamlgraph -pkg Z3 -tag thread check.native

#clean:
#	ocamlbuild -clean
#	rm -f *.dot
