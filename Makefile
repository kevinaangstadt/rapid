# AP Programming Language
# Kevin Angstadt
# University of Virginia
# Department of Computer Science

OCAMLOPT  = ocamloptp
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all: language simulator

test: simulator
	cd test && ./test.sh && cd ..

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
	$(RM) -f *.cmi *.cmx *.o *.cmo lex.ml parse.ml parse.mli language language.exe rapidsim rapidsim.exe

%.cmi: %.mli
	$(OCAMLOPT) -c -p $<

%.cmx: %.ml 
	$(OCAMLOPT) -c -p $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $<
	
language: $(LANG_OBJS) main.cmx
	$(OCAMLOPT) -o language str.cmxa $(LANG_OBJS) main.cmx
	
simulator: $(LANG_OBJS) simulate.cmx
	$(OCAMLOPT) -p -o rapidsim str.cmxa $(LANG_OBJS) simulate.cmx

parse.cmx: parse.cmi parse.ml
main.cmx: parse.cmi
