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

/* Point-to-point communication */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "camlmpi.h"

/* Sending */

value caml_mpi_send(value data, value dest, value tag, value comm)
{
  MPI_Send(String_val(data), string_length(data), MPI_BYTE,
           Int_val(dest), Int_val(tag), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_send_int(value data, value dest, value tag, value comm)
{
  long n = Long_val(data);
  MPI_Send(&n, 1, MPI_LONG, Int_val(dest), Int_val(tag), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_send_intarray(value data, value dest, value tag, value comm)
{
  MPI_Send(&Field(data, 0), Wosize_val(data), MPI_LONG,
           Int_val(dest), Int_val(tag), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_send_float(value data, value dest, value tag, value comm)
{
  /* FIXME: potential alignment problem if ARCH_ALIGN_DOUBLE */
  MPI_Send(&Double_val(data), Wosize_val(data) / Double_wosize, MPI_DOUBLE,
           Int_val(dest), Int_val(tag), Comm_val(comm));
  return Val_unit;
}

/* Probe for pending messages and determine length */

value caml_mpi_probe(value source, value tag, value comm)
{
  MPI_Status status;
  int count;
  value res;

  MPI_Probe(Int_val(source), Int_val(tag), Comm_val(comm), &status);
  MPI_Get_count(&status, MPI_BYTE, &count);
  res = alloc_tuple(3);
  Field(res, 0) = Val_int(count);
  Field(res, 1) = Val_int(status.MPI_SOURCE);
  Field(res, 2) = Val_int(status.MPI_TAG);
  return res;
}

/* Receive */

value caml_mpi_receive(value buffer, value source, value tag, value comm)
{
  MPI_Status status;

  MPI_Recv(String_val(buffer), string_length(buffer), MPI_BYTE,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return Val_unit;
}

value caml_mpi_receive_int(value source, value tag, value comm)
{
  MPI_Status status;
  long n;

  MPI_Recv(&n, 1, MPI_LONG,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return Val_long(n);
}

value caml_mpi_receive_intarray(value data, value source, value tag, value comm)
{
  MPI_Status status;

  MPI_Recv(&Field(data, 0), Wosize_val(data), MPI_LONG,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return Val_unit;
}

value caml_mpi_receive_float(value source, value tag, value comm)
{
  MPI_Status status;
  double d;

  MPI_Recv(&d, 1 , MPI_DOUBLE,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return copy_double(d);
}

value caml_mpi_receive_floatarray(value data, value source, value tag, value comm)
{
  MPI_Status status;

  /* FIXME: potential alignment problem if ARCH_ALIGN_DOUBLE */
  MPI_Recv(&Double_val(data), Wosize_val(data) / Double_wosize, MPI_DOUBLE,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return Val_unit;
}

/* Auxiliaries */

value caml_mpi_get_any_tag(value unit)
{
  return Val_int(MPI_ANY_TAG);
}

value caml_mpi_get_any_source(value unit)
{
  return Val_int(MPI_ANY_SOURCE);
}


