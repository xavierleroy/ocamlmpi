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

/* Utility functions on arrays */

#include <string.h>
#include <mpi.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include "camlmpi.h"

void caml_mpi_decode_intarray(value data, mlsize_t len)
{
  mlsize_t i;
  for (i = 0; i < len; i++) Field(data, i) = Long_val(Field(data, i));
}

void caml_mpi_encode_intarray(value data, mlsize_t len)
{
  mlsize_t i;
  for (i = 0; i < len; i++) Field(data, i) = Val_long(Field(data, i));
}

static value copy_two_doubles(double d0, double d1)
{
  value res = caml_alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(res, 0, d0);
  Store_double_field(res, 1, d1);
  return res;
}

value caml_mpi_ba_value(any_ba_value(dv), intnat kind)
{
  void *d = dv;

  switch (kind) {
  default:
    CAMLassert(0);
  case CAML_BA_FLOAT32:
    return caml_copy_double((double) *((float*) d));
  case CAML_BA_FLOAT64:
    return caml_copy_double(*((double *) d));
  case CAML_BA_SINT8:
    return Val_int(*((caml_ba_int8 *) d));
  case CAML_BA_UINT8:
    return Val_int(*((caml_ba_uint8 *) d));
  case CAML_BA_SINT16:
    return Val_int(*((caml_ba_int16 *) d));
  case CAML_BA_UINT16:
    return Val_int(*((caml_ba_uint16 *) d));
  case CAML_BA_INT32:
    return caml_copy_int32(*((int32_t *) d));
  case CAML_BA_INT64:
    return caml_copy_int64(*((int64_t *) d));
  case CAML_BA_NATIVE_INT:
    return caml_copy_nativeint(*((intnat *) d));
  case CAML_BA_CAML_INT:
    return Val_long(*((intnat *) d));
  case CAML_BA_COMPLEX32:
    { float * p = (float *) d;
      return copy_two_doubles((double) p[0], (double) p[1]); }
  case CAML_BA_COMPLEX64:
    { double * p = (double *) d;
      return copy_two_doubles(p[0], p[1]); }
  case CAML_BA_CHAR:
    return Val_int(*((unsigned char *) d));
  }
}

void caml_mpi_ba_element(value dv, intnat kind, any_ba_value(rv))
{
  void *r = rv;

  switch (kind) {
  default:
    CAMLassert(0);
  case CAML_BA_FLOAT32:
    *((float *) r) = Double_val(dv); break;
  case CAML_BA_FLOAT64:
    *((double *) r) = Double_val(dv); break;
  case CAML_BA_CHAR:
  case CAML_BA_SINT8:
  case CAML_BA_UINT8:
    *((caml_ba_int8 *) r) = Int_val(dv); break;
  case CAML_BA_SINT16:
  case CAML_BA_UINT16:
    *((caml_ba_int16 *) r) = Int_val(dv); break;
  case CAML_BA_INT32:
    *((int32_t *) r) = Int32_val(dv); break;
  case CAML_BA_INT64:
    *((int64_t *) r) = Int64_val(dv); break;
  case CAML_BA_NATIVE_INT:
    *((intnat *) r) = Nativeint_val(dv); break;
  case CAML_BA_CAML_INT:
    *((intnat *) r) = Long_val(dv); break;
  case CAML_BA_COMPLEX32:
    { float * p = ((float *) r);
      p[0] = Double_field(dv, 0);
      p[1] = Double_field(dv, 1);
      break; }
  case CAML_BA_COMPLEX64:
    { double * p = ((double *) r);
      p[0] = Double_field(dv, 0);
      p[1] = Double_field(dv, 1);
      break; }
  }
}

#ifdef ARCH_ALIGN_DOUBLE

double * caml_mpi_input_floatarray(value data, mlsize_t len)
{
  double * d = caml_stat_alloc(len * sizeof(double));
  memcpy(d, (double *) data, len * sizeof(double));
  return d;
}

double * caml_mpi_output_floatarray(value data, mlsize_t len)
{
  return caml_stat_alloc(len * sizeof(double));
}

void caml_mpi_free_floatarray(double * d)
{
  if (d != NULL) caml_stat_free(d);
}

void caml_mpi_commit_floatarray(double * d, value data, mlsize_t len)
{
  if (d != NULL) {
    memcpy((double *) data, d, len * sizeof(double));
    caml_stat_free(d);
  }
}

double * caml_mpi_input_floatarray_at_node(value data, mlsize_t len,
                                           value root, value comm)
{
  int myrank;
  MPI_Comm_rank(Comm_val(comm), &myrank);
  if (myrank == Int_val(root))
    return caml_mpi_input_floatarray(data, len);
  else
    return NULL;
}

double * caml_mpi_output_floatarray_at_node(value data, mlsize_t len,
                                           value root, value comm)
{
  int myrank;
  MPI_Comm_rank(Comm_val(comm), &myrank);
  if (myrank == Int_val(root))
    return caml_mpi_output_floatarray(data, len);
  else
    return NULL;
}

#endif

