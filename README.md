This is OCamlMPI, an OCaml binding to the Message Passing Interface (MPI).

# What is MPI?

MPI is a popular library for distributed-memory parallel programming
in SPMD (single program, multiple data) style.

MPI offers both point-to-point message passing and group communication
operations (broadcast, scatter/gather, etc).

Several implementations of MPI are available, both for networks of
Unix workstations and for supercomputers with specialized communication
networks.

More info on MPI is available [here](https://www.mcs.anl.gov/mpi/).

Two popular, free implementations of MPI are
  [MPICH](https://www.mpich.org/)
and
  [OpenMPI](https://www.open-mpi.org/) .

# The OCamlMPI interface

OCamlMPI provides Caml bindings for a large subset of MPI functions.
I omitted a number of MPI functions for which I had no use, though.
The file mpi.mli in this directory lists the MPI functions provided,
along with short documentation.  See the MPI docs at the URLs above
for more detailed info.

Most communication functions come in 10 flavors:
- one generic function operating on any data type  (e.g. `Mpi.send`)
- nine specialized functions for the following types:

| Type                  | "Send" function          |
|-----------------------|--------------------------|
| `int`                 | `Mpi.send_int`           |
| `float`               | `Mpi.send_float`         |
| `int array`           | `Mpi.send_int_array`     |
| `float array`         | `Mpi.send_float_array`   |
| `Bigarray.Genarray.t` | `Mpi.send_bigarray`      |
| `Bigarray.Array0.t`   | `Mpi.send_bigarray0`     |
| `Bigarray.Array1.t`   | `Mpi.send_bigarray1`     |
| `Bigarray.Array2.t`   | `Mpi.send_bigarray2`     |
| `Bigarray.Array3.t`   | `Mpi.send_bigarray3`     |

The generic function is simpler to use, and more general, but involves
more overhead than the specialized functions.

The data types that can be transmitted using the generic
communication functions are those that can be marshaled by the
`Marshal.to_channel` function (q.v.) with the `Marshal.Closures` option.
That is:
  - all concrete data structures (base types, arrays, records, variant types),
  - function closures,
  - but not objects,
  - nor certain abstract types (`in_channel`, `out_channel`, `Graphics.image`).

# Building OCamlMPI

If MPI is installed in the standard locations, just do

       make all opt

If MPI headers and libraries cannot be found, you may need to edit the
Makefile and set the following variables according to your MPI
installation:

- `MPIINCDIR`    directory containing the MPI include file `<mpi.h>`
- `MPILIBDIR`    directory containing the MPI library `-lmpi`
- `MPICC`        path to the `mpicc` executable
- `MPIRUN`       path to the `mpirun` executable.

You may also need to adjust `CFLAGS`.

For final installation: become super-user and do `make install`.

# Testing OCamlMPI

`make test` builds and runs a couple of test programs.

# Using OCamlMPI

In native-code:

        ocamlfind ocamlopt -package mpi -linkpkg <your files>

In bytecode:

        ocamlfind ocamlc -package mpi -linkpkg <your files>

# Licensing

The OCamlMPI library is copyright 1998 INRIA and distributed under the
terms of the GNU Library General Public License version 2, with a
special exception on clause 6 described in the file LICENSE.


# Support

For issues specific to this OCaml-MPI binding, please use
[the Github bug tracker](https://github.com/xavierleroy/ocamlmpi/issues).

Questions about the following issues should be directed to MPI
newsgroups, mailing-lists or implementation vendors, but not to the
issue tracker above: semantics of MPI functions, how to program with
MPI, finding and installing implementations of MPI, performance tuning
of MPI applications, etc.


