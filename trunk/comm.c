/***********************************************************************/
/*                                                                     */
/*                         The Caml/MPI interface                      */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Handling of communicators */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "camlmpi.h"

static void caml_mpi_finalize_comm(value v)
{
  MPI_Comm_free(&Comm_val(v));
}

value caml_mpi_alloc_comm(MPI_Comm c)
{
  value res =
    alloc_final(1 + (sizeof(MPI_Comm) + sizeof(value) - 1) / sizeof(value),
                caml_mpi_finalize_comm, 1, 100);
  Comm_val(res) = c;
  return res;
}

value caml_mpi_get_comm_world(value unit)
{
  return caml_mpi_alloc_comm(MPI_COMM_WORLD);
}

value caml_mpi_comm_size(value comm)
{
  int size;
  MPI_Comm_size(Comm_val(comm), &size);
  return Val_int(size);
}

value caml_mpi_comm_rank(value comm)
{
  int rank;
  MPI_Comm_rank(Comm_val(comm), &rank);
  return Val_int(rank);
}

value caml_mpi_comm_compare(value comm1, value comm2)
{
  int res;
  MPI_Comm_compare(Comm_val(comm1), Comm_val(comm2), &res);
  return Val_bool(res);
}

value caml_mpi_comm_split(value comm, value color, value key)
{
  MPI_Comm newcomm;
  MPI_Comm_split(Comm_val(comm), Int_val(color), Int_val(key), &newcomm);
  return caml_mpi_alloc_comm(newcomm);
}

value caml_mpi_get_undefined(value unit)
{
  return Val_int(MPI_UNDEFINED);
}

