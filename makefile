RESULT = vecna
SOURCES = util.ml poly.ml
PACKS = unix hex zarith
PP = camlp4find $(PACKS)
export PP
CREATE_LIB = yes
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
