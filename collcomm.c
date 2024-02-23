/***********************************************************************/
/*                                                                     */
/*                         The Caml/MPI interface                      */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file LICENSE.        */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Group communication */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include "camlmpi.h"

/* Barrier synchronization */

value caml_mpi_barrier(value comm)
{
  MPI_Barrier(Comm_val(comm));
  return Val_unit;
}

/* Broadcast */

value caml_mpi_broadcast(value buffer, value root, value comm)
{
  MPI_Bcast(Bp_val(buffer), caml_string_length(buffer), MPI_BYTE,
            Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_broadcast_int(value data, value root, value comm)
{
  long n = Long_val(data);
  MPI_Bcast(&n, 1, MPI_LONG, Int_val(root), Comm_val(comm));
  return Val_long(n);
}

value caml_mpi_broadcast_float(value data, value root, value comm)
{
  double d = Double_val(data);
  MPI_Bcast(&d, 1, MPI_DOUBLE, Int_val(root), Comm_val(comm));
  return caml_copy_double(d);
}

value caml_mpi_broadcast_intarray(value data, value root, value comm)
{
  MPI_Bcast(Longptr_val(data), Wosize_val(data), MPI_LONG,
            Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_broadcast_floatarray(value data, value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  MPI_Bcast(d, len, MPI_DOUBLE, Int_val(root), Comm_val(comm));
  caml_mpi_commit_floatarray(d, data, len);
  return Val_unit;
}

value caml_mpi_broadcast_bigarray(value data, value root, value comm)
{
  struct caml_ba_array* d = Caml_ba_array_val(data);
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[d->flags & CAML_BA_KIND_MASK];

  MPI_Bcast(d->data, dlen, dt, Int_val(root), Comm_val(comm));
  return Val_unit;
}

/* Scatter */

static void caml_mpi_counts_displs(value lengths,
                                   /* out */ int ** counts,
                                   /* out */ int ** displs)
{
  int size, disp, i;

  size = Wosize_val(lengths);
  if (size > 0) {
    *counts = caml_stat_alloc(size * sizeof(int));
    *displs = caml_stat_alloc(size * sizeof(int));
    for (i = 0, disp = 0; i < size; i++) {
      (*counts)[i] = Int_val(Field(lengths, i));
      (*displs)[i] = disp;
      disp += (*counts)[i];
    }
  } else {
    *counts = NULL;
    *displs = NULL;
  }
}

value caml_mpi_scatter(value sendbuf, value sendlengths, 
                       value recvbuf,
                       value root, value comm)
{
  int * sendcounts, * displs;

  caml_mpi_counts_displs(sendlengths, &sendcounts, &displs);
  MPI_Scatterv(String_val(sendbuf), sendcounts, displs, MPI_BYTE,
               Bp_val(recvbuf), caml_string_length(recvbuf), MPI_BYTE,
               Int_val(root), Comm_val(comm));
  if (sendcounts != NULL) {
    caml_stat_free(sendcounts);
    caml_stat_free(displs);
  }
  return Val_unit;
}

value caml_mpi_scatter_int(value data, value root, value comm)
{
  value n;

  MPI_Scatter(Longptr_val(data), 1, MPI_LONG,
              &n, 1, MPI_LONG,
              Int_val(root), Comm_val(comm));
  return n;
}

value caml_mpi_scatter_float(value data, value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * src = caml_mpi_input_floatarray(data, len);
  double dst;
  MPI_Scatter(src, 1, MPI_DOUBLE, &dst, 1, MPI_DOUBLE,
              Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(src);
  return caml_copy_double(dst);
}

CAMLprim value caml_mpi_scatter_from_bigarray(value data, value root,
					      value comm)
{
  CAMLparam3(data, root, comm);
  struct caml_ba_array* d = Caml_ba_array_val(data);
  intnat kind = d->flags & CAML_BA_KIND_MASK;
  MPI_Comm c = Comm_val(comm);
  int rank, csize;
  MPI_Datatype dt = caml_mpi_ba_mpi_type[kind];
  any_ba_value(dst);

  MPI_Comm_rank(c, &rank);
  if (rank == Int_val(root)) {
    MPI_Comm_size(c, &csize);
    if (caml_ba_num_elts(d) != csize)
      caml_mpi_raise_error("Mpi.scatter_from_bigarray: array size mismatch");
  }
  MPI_Scatter(d->data, 1, dt, &dst, 1, dt, Int_val(root), c);

  CAMLreturn(caml_mpi_ba_value(dst, kind));
}

value caml_mpi_scatter_intarray(value source, value dest,
                                value root, value comm)
{
  mlsize_t len = Wosize_val(dest);
  MPI_Scatter(Longptr_val(source), len, MPI_LONG,
              Longptr_val(dest), len, MPI_LONG,
              Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_scatter_floatarray(value source, value dest,
                                  value root, value comm)
{
  mlsize_t srclen = Wosize_val(source) / Double_wosize;
  mlsize_t len = Wosize_val(dest) / Double_wosize;
  double * src = caml_mpi_input_floatarray_at_node(source, srclen, root, comm);
  double * dst = caml_mpi_output_floatarray(dest, len);

  MPI_Scatter(src, len, MPI_DOUBLE, dst, len, MPI_DOUBLE,
              Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(src);
  caml_mpi_commit_floatarray(dst, dest, len);
  return Val_unit;
}

value caml_mpi_scatter_bigarray(value source, value dest,
                                value root, value comm)
{
  struct caml_ba_array* s = Caml_ba_array_val(source);
  struct caml_ba_array* d = Caml_ba_array_val(dest);
  MPI_Comm c = Comm_val(comm);
  int rank, csize;
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[d->flags & CAML_BA_KIND_MASK];

  MPI_Comm_rank(c, &rank);
  if (rank == Int_val(root)) {
    MPI_Comm_size(c, &csize);
    if (caml_ba_num_elts(s) != dlen * csize)
      caml_mpi_raise_error("Mpi.scatter_bigarray: array size mismatch");
  }

  MPI_Scatter(s->data, dlen, dt, d->data, dlen, dt, Int_val(root), c);
  return Val_unit;
}

/* Gather */

value caml_mpi_gather(value sendbuf,
                      value recvbuf, value recvlengths,
                      value root, value comm)
{
  int * recvcounts, * displs;

  caml_mpi_counts_displs(recvlengths, &recvcounts, &displs);
  MPI_Gatherv(String_val(sendbuf), caml_string_length(sendbuf), MPI_BYTE,
              Bp_val(recvbuf), recvcounts, displs, MPI_BYTE,
              Int_val(root), Comm_val(comm));
  if (recvcounts != NULL) {
    caml_stat_free(recvcounts);
    caml_stat_free(displs);
  }
  return Val_unit;
}

value caml_mpi_gather_int(value data, value result, value root, value comm)
{
  MPI_Gather(&data, 1, MPI_LONG,
             Longptr_val(result), 1, MPI_LONG,
             Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_gather_intarray(value data, value result,
                               value root, value comm)
{
  mlsize_t len = Wosize_val(data);
  MPI_Gather(Longptr_val(data), len, MPI_LONG,
             Longptr_val(result), len, MPI_LONG,
             Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_gather_float(value data, value result, value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  mlsize_t reslen = Wosize_val(result) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res =
    caml_mpi_output_floatarray_at_node(result, reslen, root, comm);
  MPI_Gather(d, len, MPI_DOUBLE, res, len, MPI_DOUBLE,
             Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, reslen);
  return Val_unit;
}

CAMLprim value caml_mpi_gather_to_bigarray(value data, value result,
				           value root, value comm)
{
  CAMLparam4(data, result, root, comm);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  intnat kind = r->flags & CAML_BA_KIND_MASK;
  MPI_Comm c = Comm_val(comm);
  int rank, csize;
  MPI_Datatype dt = caml_mpi_ba_mpi_type[kind];
  any_ba_value(d);

  MPI_Comm_rank(c, &rank);
  if (rank == Int_val(root)) {
    MPI_Comm_size(c, &csize);
    if (caml_ba_num_elts(r) != csize)
      caml_mpi_raise_error("Mpi.gather_to_bigarray: array size mismatch");
  }

  caml_mpi_ba_element(data, kind, d);
  MPI_Gather(d, 1, dt, r->data, 1, dt, Int_val(root), c);
  CAMLreturn(Val_unit);
}

value caml_mpi_gather_bigarray(value data, value result,
			       value root, value comm)
{
  struct caml_ba_array* d = Caml_ba_array_val(data);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  MPI_Comm c = Comm_val(comm);
  int rank, csize;
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[r->flags & CAML_BA_KIND_MASK];

  MPI_Comm_rank(c, &rank);
  if (rank == Int_val(root)) {
    MPI_Comm_size(c, &csize);
    if (caml_ba_num_elts(r) != dlen * csize)
      caml_mpi_raise_error("Mpi.gather_bigarray: array size mismatch");
  }

  MPI_Gather(d->data, dlen, dt, r->data, dlen, dt, Int_val(root), c);
  return Val_unit;
}

/* Gather to all */

value caml_mpi_allgather(value sendbuf,
                         value recvbuf, value recvlengths,
                         value comm)
{
  int * recvcounts, * displs;

  caml_mpi_counts_displs(recvlengths, &recvcounts, &displs);
  MPI_Allgatherv(String_val(sendbuf), caml_string_length(sendbuf), MPI_BYTE,
                 Bp_val(recvbuf), recvcounts, displs, MPI_BYTE,
                 Comm_val(comm));
  caml_stat_free(recvcounts);
  caml_stat_free(displs);
  return Val_unit;
}

value caml_mpi_allgather_int(value data, value result, value comm)
{
  MPI_Allgather(&data, 1, MPI_LONG,
                Longptr_val(result), 1, MPI_LONG,
                Comm_val(comm));
  return Val_unit;
}

value caml_mpi_allgather_intarray(value data, value result, value comm)
{
  mlsize_t len = Wosize_val(data);
  MPI_Allgather(Longptr_val(data), len, MPI_LONG,
                Longptr_val(result), len, MPI_LONG,
                Comm_val(comm));
  return Val_unit;
}

value caml_mpi_allgather_float(value data, value result, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  mlsize_t reslen = Wosize_val(result) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, reslen);

  MPI_Allgather(d, len, MPI_DOUBLE, res, len, MPI_DOUBLE,
                Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, reslen);
  return Val_unit;
}

CAMLprim value caml_mpi_allgather_to_bigarray(value data, value result,
					      value comm)
{
  CAMLparam3(data, result, comm);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  intnat kind = r->flags & CAML_BA_KIND_MASK;
  MPI_Comm c = Comm_val(comm);
  int csize;
  MPI_Datatype dt = caml_mpi_ba_mpi_type[kind];
  any_ba_value(d);

  MPI_Comm_size(c, &csize);
  if (caml_ba_num_elts(r) != csize)
    caml_mpi_raise_error("Mpi.allgather_to_bigarray: array size mismatch");

  caml_mpi_ba_element(data, kind, d);
  MPI_Allgather(d, 1, dt, r->data, 1, dt, c);
  CAMLreturn(Val_unit);
}

value caml_mpi_allgather_bigarray(value data, value result, value comm)
{
  struct caml_ba_array* d = Caml_ba_array_val(data);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  MPI_Comm c = Comm_val(comm);
  int csize;
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[r->flags & CAML_BA_KIND_MASK];

  MPI_Comm_size(c, &csize);
  if (caml_ba_num_elts(r) != dlen * csize)
    caml_mpi_raise_error("Mpi.allgather_bigarray: array size mismatch");

  MPI_Allgather(d->data, dlen, dt, r->data, dlen, dt, c);
  return Val_unit;
}

/* All to all */

value caml_mpi_alltoall(value sendbuf, value sendlengths,
                        value recvbuf, value recvlengths,
                        value comm)
{
  int * recvcounts, * recvdispls;
  int * sendcounts, * senddispls;

  caml_mpi_counts_displs(sendlengths, &sendcounts, &senddispls);
  caml_mpi_counts_displs(recvlengths, &recvcounts, &recvdispls);
  MPI_Alltoallv(String_val(sendbuf), sendcounts, senddispls, MPI_BYTE,
                Bp_val(recvbuf), recvcounts, recvdispls, MPI_BYTE,
                Comm_val(comm));
  caml_stat_free(recvcounts);
  caml_stat_free(recvdispls);
  caml_stat_free(sendcounts);
  caml_stat_free(senddispls);
  return Val_unit;
}

value caml_mpi_alltoall_intarray(value data, value result, value comm)
{
  mlsize_t len = Wosize_val(data);
  MPI_Comm c = Comm_val(comm);
  int csize, count;
  void* sendbuf = Longptr_val(data);
  void* recvbuf = Longptr_val(result);

  MPI_Comm_size(c, &csize);
  count = len / csize;
  if (len % csize != 0)
    caml_mpi_raise_error("Mpi.alltoall_intarray: incorrect array size");

  if (sendbuf == recvbuf) sendbuf = MPI_IN_PLACE;

  MPI_Alltoall(sendbuf, count, MPI_LONG, recvbuf, count, MPI_LONG, c);
  return Val_unit;
}

value caml_mpi_alltoall_floatarray(value data, value result, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  mlsize_t reslen = Wosize_val(result) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, reslen);
  MPI_Comm c = Comm_val(comm);
  int csize, count;

  MPI_Comm_size(c, &csize);
  count = len / csize;
  if (len % csize != 0)
    caml_mpi_raise_error("Mpi.alltoall_floatarray: incorrect array size");

  MPI_Alltoall(d, count, MPI_DOUBLE, res, count, MPI_DOUBLE, c);
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, reslen);
  return Val_unit;
}

value caml_mpi_alltoall_bigarray(value data, value result, value comm)
{
  struct caml_ba_array* d = Caml_ba_array_val(data);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  MPI_Comm c = Comm_val(comm);
  int csize, count;
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[r->flags & CAML_BA_KIND_MASK];

  MPI_Comm_size(c, &csize);
  if (caml_ba_num_elts(r) != dlen)
    caml_mpi_raise_error("Mpi.alltoall_bigarray: array size mismatch");
  count = dlen / csize;
  if (dlen % csize != 0)
    caml_mpi_raise_error("Mpi.alltoall_bigarray: incorrect array size");

  MPI_Alltoall(d->data, count, dt, r->data, count, dt, c);
  return Val_unit;
}

/* Reduce */

static MPI_Op reduce_op[] =
  { MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_BAND, MPI_BOR, MPI_BXOR,
    // deprecated Int_* ops:
    MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, MPI_BAND, MPI_BOR, MPI_BXOR,
    // deprecated Float_* ops:
    MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD };

value caml_mpi_reduce_int(value data, value op, value root, value comm)
{
  long d = Long_val(data);
  long r = 0;
  MPI_Reduce(&d, &r, 1, MPI_LONG,
             reduce_op[Int_val(op)], Int_val(root), Comm_val(comm));
  return Val_long(r);
}

value caml_mpi_reduce_intarray(value data, value result, value op,
                               value root, value comm)
{
  mlsize_t len = Wosize_val(data);
  int myrank;
  /* Decode data at all nodes in place */
  caml_mpi_decode_intarray(data, len);
  /* Do the reduce */
  MPI_Reduce(Longptr_val(data), Longptr_val(result), len, MPI_LONG,
             reduce_op[Int_val(op)], Int_val(root), Comm_val(comm));
  /* Re-encode data at all nodes in place */
  caml_mpi_encode_intarray(data, len);
  /* At root node, also encode result */
  MPI_Comm_rank(Comm_val(comm), &myrank);
  if (myrank == Int_val(root)) caml_mpi_encode_intarray(result, len);
  return Val_unit;
}

value caml_mpi_reduce_float(value data, value op, value root, value comm)
{
  double d = Double_val(data);
  double r = 0.0;
  MPI_Reduce(&d, &r, 1, MPI_DOUBLE,
             reduce_op[Int_val(op)], Int_val(root), Comm_val(comm));
  return caml_copy_double(r);
}

value caml_mpi_reduce_floatarray(value data, value result, value op,
                            value root, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, len);

  MPI_Reduce(d, res, len, MPI_DOUBLE,
             reduce_op[Int_val(op)], Int_val(root), Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, len);
  return Val_unit;
}

value caml_mpi_reduce_bigarray(value data, value result, value op,
                               value root, value comm)
{
  struct caml_ba_array* d = Caml_ba_array_val(data);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  MPI_Comm c = Comm_val(comm);
  int rank;
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[d->flags & CAML_BA_KIND_MASK];
  void* sendbuf = d->data;

  MPI_Comm_rank(c, &rank);
  if (rank == root) {
    if (dlen != caml_ba_num_elts(r))
      caml_mpi_raise_error("Mpi.reduce_bigarray: array size mismatch");

    if (d->data == r->data) sendbuf = MPI_IN_PLACE;
  }

  MPI_Reduce(sendbuf, r->data, dlen, dt,
             reduce_op[Int_val(op)], Int_val(root), c);
  return Val_unit;
}

/* Allreduce */

value caml_mpi_allreduce_int(value data, value op, value comm)
{
  long d = Long_val(data);
  long r;
  MPI_Allreduce(&d, &r, 1, MPI_LONG,
                reduce_op[Int_val(op)], Comm_val(comm));
  return Val_long(r);
}

value caml_mpi_allreduce_intarray(value data, value result, value op,
                                  value comm)
{
  mlsize_t len = Wosize_val(data);
  /* Decode data at all nodes in place */
  caml_mpi_decode_intarray(data, len);
  /* Do the reduce */
  MPI_Allreduce(Longptr_val(data), Longptr_val(result), len, MPI_LONG,
                reduce_op[Int_val(op)], Comm_val(comm));
  /* Re-encode data at all nodes in place */
  caml_mpi_encode_intarray(data, len);
  /* Re-encode result at all nodes in place */
  caml_mpi_encode_intarray(result, len);
  return Val_unit;
}

value caml_mpi_allreduce_float(value data, value op, value comm)
{
  double d = Double_val(data);
  double r;
  MPI_Allreduce(&d, &r, 1, MPI_DOUBLE,
                reduce_op[Int_val(op)], Comm_val(comm));
  return caml_copy_double(r);
}

value caml_mpi_allreduce_floatarray(value data, value result, value op,
                                    value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, len);

  MPI_Allreduce(d, res, len, MPI_DOUBLE,
                reduce_op[Int_val(op)], Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, len);
  return Val_unit;
}

value caml_mpi_allreduce_bigarray(value data, value result, value op,
                                  value comm)
{
  struct caml_ba_array* d = Caml_ba_array_val(data);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[d->flags & CAML_BA_KIND_MASK];
  void* sendbuf = (d->data == r->data) ? MPI_IN_PLACE : d->data;

  if (caml_ba_num_elts(r) != dlen)
    caml_mpi_raise_error("Mpi.allreduce_bigarray: array size mismatch");

  MPI_Allreduce(sendbuf, r->data, dlen, dt,
		reduce_op[Int_val(op)], Comm_val(comm));
  return Val_unit;
}

/* Scan */

value caml_mpi_scan_int(value data, value op, value comm)
{
  long d = Long_val(data);
  long r;

  MPI_Scan(&d, &r, 1, MPI_LONG, reduce_op[Int_val(op)], Comm_val(comm));
  return Val_long(r);
}

value caml_mpi_scan_intarray(value data, value result, value op, value comm)
{
  mlsize_t len = Wosize_val(data);

  /* Decode data at all nodes in place */
  caml_mpi_decode_intarray(data, len);
  /* Do the scan */
  MPI_Scan(Longptr_val(data), Longptr_val(result), len, MPI_LONG,
           reduce_op[Int_val(op)], Comm_val(comm));
  /* Re-encode data at all nodes in place */
  caml_mpi_encode_intarray(data, len);
  /* Encode result */
  caml_mpi_encode_intarray(result, len);
  return Val_unit;
}

value caml_mpi_scan_float(value data, value op, value comm)
{
  double d = Double_val(data), r;

  MPI_Scan(&d, &r, 1, MPI_DOUBLE,
           reduce_op[Int_val(op)], Comm_val(comm));
  return caml_copy_double(r);
}

value caml_mpi_scan_floatarray(value data, value result, value op, value comm)
{
  mlsize_t len = Wosize_val(data) / Double_wosize;
  double * d = caml_mpi_input_floatarray(data, len);
  double * res = caml_mpi_output_floatarray(result, len);

  MPI_Scan(d, res, len, MPI_DOUBLE,
           reduce_op[Int_val(op)], Comm_val(comm));
  caml_mpi_free_floatarray(d);
  caml_mpi_commit_floatarray(res, result, len);
  return Val_unit;
}

value caml_mpi_scan_bigarray(value data, value result, value op, value comm)
{
  struct caml_ba_array* d = Caml_ba_array_val(data);
  struct caml_ba_array* r = Caml_ba_array_val(result);
  mlsize_t dlen = caml_ba_num_elts(d);
  MPI_Datatype dt = caml_mpi_ba_mpi_type[d->flags & CAML_BA_KIND_MASK];
  void* sendbuf = (d->data == r->data) ? MPI_IN_PLACE : d->data;

  if (caml_ba_num_elts(r) != dlen)
    caml_mpi_raise_error("Mpi.scan_bigarray: array size mismatch");

  MPI_Scan(sendbuf, r->data, dlen, dt,
           reduce_op[Int_val(op)], Comm_val(comm));
  return Val_unit;
}

