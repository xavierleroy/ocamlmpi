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

/* Common definitions */

#define Comm_val(comm) (*((MPI_Comm *) &Field(comm, 1)))

extern value caml_mpi_alloc_comm(MPI_Comm c);
extern void caml_mpi_decode_intarray(value array, int len);
extern void caml_mpi_encode_intarray(value array, int len);
