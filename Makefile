OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep

MPIINCDIR=/usr/local/lib/mpich/include
MPILIBDIR=/usr/local/lib/mpich/lib/LINUX/ch_p4
CAMLLIB=/usr/local/lib/ocaml

CC=gcc
CFLAGS=-I$(CAMLLIB) -I$(MPIINCDIR) -O -g -Wall

COBJS=init.o comm.o msgs.o collcomm.o
OBJS=mpi.cmo

all: libcamlmpi.a mpi.cma mpi.cmxa

libcamlmpi.a: $(COBJS)
	rm -f $@
	ar rc $@ $(COBJS)

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

testmpi: test.ml mpi.cma libcamlmpi.a
	ocamlc -custom -o testmpi unix.cma mpi.cma test.ml libcamlmpi.a -ccopt -L$(MPILIBDIR) -cclib -lmpi -cclib -lunix

clean::
	rm -f testmpi

test: testmpi
	mpirun -np 5 ./testmpi

test_mandel: test_mandel.ml mpi.cmxa libcamlmpi.a
	ocamlopt -ccopt -static -o test_mandel graphics.cmxa mpi.cmxa test_mandel.ml libcamlmpi.a -ccopt -L$(MPILIBDIR) -cclib -lmpi -cclib -lgraphics -ccopt -L/usr/X11R6/lib -cclib -lX11

clean::
	rm -f test_mandel

clean::
	rm -f *.cm* *.o libmpi.a
depend:
	$(OCAMLDEP) *.ml > .depend
	gcc -MM $(CFLAGS) *.c >> .depend

include .depend

