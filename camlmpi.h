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

#define Comm_val(comm) (*((MPI_Comm *) &Field(comm, 1)))
#define Group_val(grp) (*((MPI_Group *) &Field(grp, 1)))
#define Request_req_val(req) (*((MPI_Request *) &Field(req, 1)))
#define Buffer_req_val(req)   (*((char **) &Field(req, 2)))

extern value caml_mpi_alloc_comm(MPI_Comm c);

extern void caml_mpi_decode_intarray(value array, mlsize_t len);
extern void caml_mpi_encode_intarray(value array, mlsize_t len);

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
