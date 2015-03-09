# AP Programming Language
# Kevin Angstadt
# University of Virginia
# Department of Computer Science

OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all: language simulator

LANG_OBJS = \
	util.cmx \
	automata.cmx \
	language.cmx \
	tc.cmx \
	id.cmx \
	compiler.cmx \
	parse.cmx \
	lex.cmx

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
	
language: $(LANG_OBJS) main.cmx
	$(OCAMLOPT) -o language str.cmxa $(LANG_OBJS) main.cmx
	
simulator: $(LANG_OBJS) simulate.cmx
	$(OCAMLOPT) -o rapidsim str.cmxa $(LANG_OBJS) simulate.cmx

parse.cmx: parse.cmi parse.ml
main.cmx: parse.cmi
