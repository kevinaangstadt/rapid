# AP Programming Language
# Kevin Angstadt
# University of Virginia
# Department of Computer Science

OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all: language

LANG_OBJS = \
	language.cmx \
	compiler.cmx \
	parse.cmx \
	lex.cmx \
	main.cmx

clean: 
	$(RM) -f *.cmi *.cmx *.o *.cmo lex.ml parse.ml parse.mli language language.exe

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $<
	
language: $(LANG_OBJS)
	$(OCAMLOPT) -o language $(LANG_OBJS)

parse.cmx: parse.cmi parse.ml
main.cmx: parse.cmi
