OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep

MPI=/usr/local/lib/mpich

CCFLAGS=-I$(MPI)/include -O -g

COBJS=mpistubs.o
OBJS=mpi.cmo


all: libmpi.a mpi.cma mpi.cmxa

libmpi.a: $(COBJS)
	rm -f libmpi.a
	ar rc libmpi.a $(COBJS)

mpi.cma: $(OBJS)
	$(OCAMLC) -a -o mpi.cma $(OBJS)

mpi.cmxa: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o mpi.cmxa $(OBJS:.cmo=.cmx)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<
.c.o:
	$(OCAMLC) -c -ccopt "$(CCFLAGS)" $<

depend:
	$(OCAMLDEP) *.ml > .depend

include .depend

