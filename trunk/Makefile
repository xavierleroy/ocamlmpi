OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep

MPI=/usr/local/lib/mpich
CAMLLIB=/usr/local/lib/ocaml

CC=gcc
CFLAGS=-I$(CAMLLIB) -I$(MPI)/include -O -g -Wall

COBJS=init.o comm.o msgs.o collcomm.o
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

clean:
	rm -f *.cm* *.o libmpi.a
depend:
	$(OCAMLDEP) *.ml > .depend
	gcc -MM $(CFLAGS) *.c >> .depend

include .depend

