/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* An interface with the MPI message-passing library */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

/* Error handling */

static value * caml_mpi_exn = NULL;

static void caml_mpi_error_handler(MPI_Comm * comm, int * errcode, ...)
{
  char errmsg[MPI_MAX_ERROR_STRING + 1];
  int resultlen;
  value msg;

  MPI_Error_string(*errcode, errmsg, &resultlen);
  msg = copy_string(errmsg);
  if (caml_mpi_exn == NULL) {
    caml_mpi_exn = caml_named_value("Mpi.Error");
    if (caml_mpi_exn == NULL)
      invalid_argument("Exception MPI.Error not initialized");
  }
  raise_with_arg(*caml_mpi_exn, msg);
}

/* Initialization and finalization */

value caml_mpi_init(value arguments)
{
  int argc, i;
  char ** argv;
  MPI_Errhandler hdlr;

  argc = Wosize_val(arguments);
  argv = stat_alloc((argc + 1) * sizeof(char *));
  for (i = 0; i < argc; i++) argv[i] = String_val(Field(arguments, i));
  argv[i] = NULL;
  MPI_Init(&argc, &argv);
  /* Register an error handler */
#if 0
  MPI_Errhandler_create((MPI_Handler_function *)caml_mpi_error_handler, &hdlr);
  MPI_Errhandler_set(MPI_COMM_WORLD, hdlr);
#endif
  return Val_unit;
}

value caml_mpi_finalize(value unit)
{
  MPI_Finalize();
}

/* Communicators */

#define Comm_val(comm) (*((MPI_Comm *) &Field(comm, 0)))

static value caml_mpi_alloc_comm(MPI_Comm c)
{
  value res = alloc((sizeof(MPI_Comm) + sizeof(value) - 1) / sizeof(value),
                    Abstract_tag);
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

/* Point-to-point communication */

value caml_mpi_send(value data, value dest, value tag, value comm)
{
  MPI_Send(String_val(data), string_length(data), MPI_BYTE,
           Int_val(dest), Int_val(tag), Comm_val(comm));
  return Val_unit;
}

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

value caml_mpi_receive(value buffer, value source, value tag, value comm)
{
  MPI_Status status;

  MPI_Recv(String_val(buffer), string_length(buffer), MPI_BYTE,
           Int_val(source), Int_val(tag), Comm_val(comm), &status);
  return Val_unit;
}

value caml_mpi_get_any_tag(value unit)
{
  return Val_int(MPI_ANY_TAG);
}

value caml_mpi_get_any_source(value unit)
{
  return Val_int(MPI_ANY_SOURCE);
}

/* Barrier synchronization */

value caml_mpi_barrier(value comm)
{
  MPI_Barrier(Comm_val(comm));
  return Val_unit;
}

/* Broadcast */

value caml_mpi_broadcast(value buffer, value root, value comm)
{
  MPI_Bcast(String_val(buffer), string_length(buffer), MPI_BYTE,
            Int_val(root), Comm_val(comm));
  return Val_unit;
}

value caml_mpi_broadcast_long(value data, value root, value comm)
{
  long n = Long_val(data);
  MPI_Bcast(&n, 1, MPI_LONG, Int_val(root), Comm_val(comm));
  return Val_long(n);
}

/* Scatter */

static void caml_mpi_counts_displs(value lengths,
                                   /* out */ int ** counts,
                                   /* out */ int ** displs)
{
  int size, disp, i;

  size = Wosize_val(lengths);
  if (size > 0) {
    *counts = stat_alloc(size * sizeof(int));
    *displs = stat_alloc(size * sizeof(int));
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
               String_val(recvbuf), string_length(recvbuf), MPI_BYTE,
               Int_val(root), Comm_val(comm));
  if (sendcounts != NULL) {
    stat_free(sendcounts);
    stat_free(displs);
  }
  return Val_unit;
}

value caml_mpi_scatter_long(value data, value root, value comm)
{
  value n;

  MPI_Scatter(&Field(data, 0), Wosize_val(data), MPI_LONG,
              &n, 1, MPI_LONG,
              Int_val(root), Comm_val(comm));
  return n;
}

/* Gather*/

value caml_mpi_gather(value sendbuf,
                      value recvbuf, value recvlengths,
                      value root, value comm)
{
  int * recvcounts, * displs;

  caml_mpi_counts_displs(recvlengths, &recvcounts, &displs);
  MPI_Gatherv(String_val(sendbuf), string_length(sendbuf), MPI_BYTE,
              String_val(recvbuf), recvcounts, displs, MPI_BYTE,
              Int_val(root), Comm_val(comm));
  if (recvcounts != NULL) {
    stat_free(recvcounts);
    stat_free(displs);
  }
  return Val_unit;
}

value caml_mpi_gather_long(value data, value result, value root, value comm)
{
  MPI_Gather(&data, 1, MPI_LONG,
             &Field(result, 0), Wosize_val(result), MPI_LONG,
             Int_val(root), Comm_val(comm));
  return Val_unit;
}

/* Gather to all */

value caml_mpi_allgather(value sendbuf,
                         value recvbuf, value recvlengths,
                         value comm)
{
  int * recvcounts, * displs;

  caml_mpi_counts_displs(recvlengths, &recvcounts, &displs);
  MPI_Allgatherv(String_val(sendbuf), string_length(sendbuf), MPI_BYTE,
                 String_val(recvbuf), recvcounts, displs, MPI_BYTE,
                 Comm_val(comm));
  stat_free(recvcounts);
  stat_free(displs);
  return Val_unit;
}

value caml_mpi_allgather_long(value data, value result, value comm)
{
  MPI_Allgather(&data, 1, MPI_LONG,
                &Field(result, 0), Wosize_val(result), MPI_LONG,
                Comm_val(comm));
  return Val_unit;
}

