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

/* Common definitions */

#define Comm_val(comm) (*((MPI_Comm *) Data_custom_val(comm)))
#define Group_val(grp) (*((MPI_Group *) Data_custom_val(grp)))

struct async_request {
  MPI_Request req;
  char * buf;
  size_t len;
};

#define Request_req_val(v) \
 (((struct async_request *) Data_custom_val(v))->req)
#define Buffer_req_val(v) \
 (((struct async_request *) Data_custom_val(v))->buf)
#define Buffer_req_len(v) \
 (((struct async_request *) Data_custom_val(v))->len)

extern void caml_mpi_raise_error(const char *msg);
extern value caml_mpi_alloc_comm(MPI_Comm c);

extern void caml_mpi_decode_intarray(value array, mlsize_t len);
extern void caml_mpi_encode_intarray(value array, mlsize_t len);

extern MPI_Datatype caml_mpi_ba_mpi_type[];

// declare big enough for int64, float64, complex64 (two doubles)
#define any_ba_value(x) double (x)[2]

// transform a bigarray element into an OCaml value
value caml_mpi_ba_value(any_ba_value(dv), intnat kind);
// transform an OCaml value into a bigarray element
void caml_mpi_ba_element(value dv, intnat kind, any_ba_value(rv));
// integer kind: returns 0; floating-point kind: returns 1

// Pointer to an array of OCaml integers

#define Longptr_val(v) ((value *) &Field(v, 0))

// Handling of float arrays

#ifdef ARCH_ALIGN_DOUBLE

extern double * caml_mpi_input_floatarray(value data, mlsize_t len);
extern double * caml_mpi_output_floatarray(value data, mlsize_t len);
extern void caml_mpi_free_floatarray(double * d);
extern void caml_mpi_commit_floatarray(double * d, value data, mlsize_t len);
extern double * caml_mpi_input_floatarray_at_node(value data, mlsize_t len,
                                                  value root, value comm);
extern double * caml_mpi_output_floatarray_at_node(value data, mlsize_t len,
                                                   value root, value comm);

#else

#define caml_mpi_input_floatarray(data,len) ((void)(len), (double *)(data))
#define caml_mpi_output_floatarray(data,len) ((void)(len), (double *)(data))
#define caml_mpi_free_floatarray(d)
#define caml_mpi_commit_floatarray(d,data,len)
#define caml_mpi_input_floatarray_at_node(data,len,root,comm) ((void)(len), (double *)(data))
#define caml_mpi_output_floatarray_at_node(data,len,root,comm) ((void)(len), (double *)(data))

#endif
