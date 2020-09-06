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

/* Initialization and error handling */

#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include "camlmpi.h"

/* Error handling */

static const value * caml_mpi_exn = NULL;

static void caml_mpi_error_handler(MPI_Comm * comm, int * errcode, ...)
{
  char errmsg[MPI_MAX_ERROR_STRING + 1];
  int resultlen;
  value msg;

  MPI_Error_string(*errcode, errmsg, &resultlen);
  msg = caml_copy_string(errmsg);
  if (caml_mpi_exn == NULL) {
    caml_mpi_exn = caml_named_value("Mpi.Error");
    if (caml_mpi_exn == NULL)
      caml_invalid_argument("Exception MPI.Error not initialized");
  }
  caml_raise_with_arg(*caml_mpi_exn, msg);
}

void caml_mpi_raise_error(const char *msg)
{
  if (caml_mpi_exn == NULL) {
    caml_mpi_exn = caml_named_value("Mpi.Error");
    if (caml_mpi_exn == NULL)
      caml_invalid_argument("Exception MPI.Error not initialized");
  }
  caml_raise_with_string(*caml_mpi_exn, msg);
}

/* Bigarrays */

MPI_Datatype caml_mpi_ba_mpi_type[] =
{ MPI_FLOAT /*FLOAT32*/, MPI_DOUBLE /*FLOAT64*/,
  MPI_INT8_T /*SINT8*/, MPI_UINT8_T /*UINT8*/,
  MPI_INT16_T /*SINT16*/, MPI_UINT16_T /*UINT16*/,
  MPI_INT32_T /*INT32*/, MPI_INT64_T /*INT64*/,
  MPI_LONG /*CAML_INT*/, MPI_LONG /*NATIVE_INT*/,
  MPI_C_FLOAT_COMPLEX /*COMPLEX32*/, MPI_C_DOUBLE_COMPLEX /*COMPLEX64*/,
  MPI_CHAR /*CHAR*/
};

/* Initialization and finalization */

value caml_mpi_init(value arguments)
{
  int argc, i;
  char ** argv;
  MPI_Errhandler hdlr;

  argc = Wosize_val(arguments);
  argv = caml_stat_alloc((argc + 1) * sizeof(char *));
  for (i = 0; i < argc; i++) argv[i] = Bp_val(Field(arguments, i));
  argv[i] = NULL;
  MPI_Init(&argc, &argv);
  /* Register an error handler */
#if MPI_VERSION >= 2
  MPI_Comm_create_errhandler(
	  (MPI_Handler_function *)caml_mpi_error_handler, &hdlr);
  MPI_Comm_set_errhandler(MPI_COMM_WORLD, hdlr);
#else
  MPI_Errhandler_create((MPI_Handler_function *)caml_mpi_error_handler, &hdlr);
  MPI_Errhandler_set(MPI_COMM_WORLD, hdlr);
#endif

  return Val_unit;
}

value caml_mpi_finalize(value unit)
{
  MPI_Finalize();
  return Val_unit;
}

value caml_mpi_wtime(value unit)
{
  return caml_copy_double(MPI_Wtime());
}

