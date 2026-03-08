OCAMLC=ocamlc
OCAMLFLAGS=-g -bin-annot
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib -g -failsafe

MPIINCDIR=$(shell pkg-config --variable=includedir ompi)
MPILIBDIR=$(shell pkg-config --variable=libdir ompi)
MPICC=mpicc
MPIRUN=mpirun

CFLAGS=-I$(MPIINCDIR) -O2 -g -Wall -DCAML_NAME_SPACE

COBJS=init.o comm.o msgs.o collcomm.o groups.o utils.o
BYTEOBJS=mpi.cmo
NATOBJS=$(BYTEOBJS:.cmo=.cmx)

all: byte

byte: libcamlmpi.a mpi.cma

opt: libcamlmpi.a mpi.cmxa

install:
	ocamlfind install mpi META mpi.mli mpi.cmi mpi.cmti \
          $(wildcard mpi*.cmx) $(wildcard mpi.cm*a) \
          $(wildcard *mpi.a) $(wildcard *mpi.so)

uninstall:
	ocamlfind remove mpi

libcamlmpi.a: $(COBJS)
	$(OCAMLMKLIB) -oc camlmpi $(COBJS) -L$(MPILIBDIR) -lmpi

mpi.cma: $(BYTEOBJS)
	$(OCAMLMKLIB) -o mpi -oc camlmpi $(BYTEOBJS) -L$(MPILIBDIR) -lmpi

mpi.cmxa: $(NATOBJS)
	$(OCAMLMKLIB) -o mpi -oc camlmpi $(NATOBJS) -L$(MPILIBDIR) -lmpi

.SUFFIXES: .ml .mli .cmo .cmi .cmx .c .o

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<
.c.o:
	$(OCAMLC) -ccopt "$(CFLAGS)" -c $<

ifeq (old,$(patsubst 4.%,old,$(shell $(OCAMLC) -version)))
OCAMLC_LIBS=unix.cmxa bigarray.cmxa
else
OCAMLC_LIBS=-I +unix unix.cmxa
endif

testmpi: test.ml mpi.cmxa libcamlmpi.a
	$(OCAMLOPT) -o testmpi $(OCAMLC_LIBS) mpi.cmxa test.ml -ccopt -L.

testmpinb: testnb.ml mpi.cmxa libcamlmpi.a
	$(OCAMLOPT) -o testmpinb $(OCAMLC_LIBS) mpi.cmxa testnb.ml -ccopt -L.

clean::
	rm -f testmpi testmpinb

test: testmpi testmpinb
	$(MPIRUN) -np 5 ./testmpi
	$(MPIRUN) -np 5 ./testmpinb

test_mandel: test_mandel.ml mpi.cmxa libcamlmpi.a
	ocamlfind ocamlopt -package graphics -linkpkg -o test_mandel mpi.cmxa test_mandel.ml -ccopt -L.

clean::
	rm -f test_mandel

clean::
	rm -f *.cm* *.o *.a *.so

depend:
	$(OCAMLDEP) *.ml > .depend
	gcc -MM $(CFLAGS) *.c >> .depend

include .depend

