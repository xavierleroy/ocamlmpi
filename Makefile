OCAMLC=ocamlc
OCAMLFLAGS=-g -bin-annot
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep

MPIINCDIR=$(shell pkg-config --variable=includedir mpich)
MPILIBDIR=$(shell pkg-config --variable=libdir mpich)
MPICC=mpicc
MPIRUN=mpirun

CFLAGS=-I`$(OCAMLC) -where` -I$(MPIINCDIR) -O2 -g -Wall -DCAML_NAME_SPACE

COBJS=init.o comm.o msgs.o collcomm.o groups.o utils.o
OBJS=mpi.cmo

all: libcamlmpi.a byte

install:
	ocamlfind install mpi META mpi.mli mpi.cmi mpi.cmti \
	    $(wildcard mpi*.cmx) $(wildcard mpi.cm*a) $(wildcard *mpi.a)

uninstall:
	ocamlfind remove mpi

libcamlmpi.a: $(COBJS)
	rm -f $@
	ar rc $@ $(COBJS)

byte: $(OBJS)
	$(OCAMLC) -a -o mpi.cma -custom $(OBJS) -cclib -lcamlmpi -ccopt -L$(MPILIBDIR) -cclib -lmpi

opt: $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -a -o mpi.cmxa $(OBJS:.cmo=.cmx) -cclib -lcamlmpi -ccopt -L$(MPILIBDIR) -cclib -lmpi

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

ifeq (old,$(patsubst 4.%,old,$(shell $(OCAMLC) -version)))
OCAMLC_LIBS=unix.cma bigarray.cma
else
OCAMLC_LIBS=-I +unix unix.cma
endif

testmpi: test.ml mpi.cma libcamlmpi.a
	$(OCAMLC) -g -o testmpi $(OCAMLC_LIBS) mpi.cma test.ml -ccopt -L$(MPILIBDIR) -ccopt -L.

testmpinb: testnb.ml mpi.cma libcamlmpi.a
	$(OCAMLC) -cc $(CC) -g -o testmpinb $(OCAMLC_LIBS) mpi.cma testnb.ml -ccopt -L$(MPILIBDIR) -ccopt -L.

clean::
	rm -f testmpi

test: testmpi testmpinb
	$(MPIRUN) -np 5 ./testmpi
	$(MPIRUN) -np 5 ./testmpinb

test_mandel: test_mandel.ml mpi.cmxa libcamlmpi.a
	ocamlfind ocamlopt -package graphics -linkpkg -o test_mandel mpi.cmxa test_mandel.ml -ccopt -L.

clean::
	rm -f test_mandel

clean::
	rm -f *.cm* *.o *.a
depend:
	$(OCAMLDEP) *.ml > .depend
	gcc -MM $(CFLAGS) *.c >> .depend

include .depend

