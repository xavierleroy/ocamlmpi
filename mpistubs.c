#include <mpi.h>
#include <mlvalues.h>

value caml_mpi_send(data, dest, tag, comm)
     value data, dest, tag, comm;
{
  caml_mpi_check(MPI_Send(String_val(data), string_length(data), MPI_BYTE,
			  Int_val(dest), Int_val(tag), Comm_val(comm)));
  return Val_unit;
}

int MPI_Probe(int source, int tag, MPI_Comm comm,
MPI_Status *status) 

value caml_mpi_probe(source, tag, comm)
     value source, tag, comm;
{
  MPI_Status status;
  int count;

  caml_mpi_check(MPI_Probe(Int_val(source), Int_val(tag), Comm_val(comm),
			   &status));
  caml_mpi_check(MPI_Get_Count(&status, MPI_BYTE, &count));
  res = alloc_tuple(3);
  Field(res, 0) = Val_int(count);
  Field(res, 1) = Val_int(status.MPI_SOURCE);
  Field(res, 2) = Val_int(status.MPI_TAG);
  return res;
}

value caml_mpi_receive(buffer, source, tag, comm)
     value buffer, source, tag, comm;
{
  MPI_Status status;

  caml_mpi_check(MPI_Receive(String_val(buffer), string_length(buffer),
			     MPI_BYTE, Val_int(source), Val_int(tag),
			     Comm_val(comm), &status));
  return Val_unit;
}

value caml_mpi_get_any_tag(unit)
     value unit;
{
  return Val_int(ANY_TAG);
}

value caml_mpi_get_any_source(unit)
     value unit;
{
  return Val_int(ANY_SOURCE);
}

value caml_mpi_broadcast(buffer, root, comm)
     value buffer, root, comm;
{
  caml_mpi_check(MPI_Bcast(String_val(buffer), string_length(buffer),
			   MPI_BYTE, Int_val(root), Comm_val(comm)));
  return Val_unit;
}

value caml_mpi_broadcast_uint(data, root, comm)
     value data, root, comm;
{
  unsigned long n = Long_val(data);
  caml_mpi_check(MPI_Bcast(&n, 1, MPI_UNSIGNED_LONG, 
			   Int_val(root), Comm_val(comm)));
  return Val_long(n);
}

value caml_mpi_scatter(sendbuf, recvbuf, root, comm)
     value sendbuf, recvbuf, root, comm;
{
  int len = string_length(recvbuf);
  caml_mpi_check(MPI_Scatter(String_val(sendbuf), len, MPI_BYTE,
			     String_val(recvbuf), len, MPI_BYTE,
			     Int_val(root), Comm_val(comm)));
  return Val_unit;
}

value caml_mpi_gather(sendbuf, recvbuf, root, comm)
     value sendbuf, recvbuf, root, comm;
{
  int len = string_length(sendbuf);
  caml_mpi_check(MPI_Gather(String_val(sendbuf), len, MPI_BYTE,
			    String_val(recvbuf), len, MPI_BYTE,
			    Int_val(root), Comm_val(comm)));
  return Val_unit;
}

value caml_mpi_allgather(sendbuf, recvbuf, comm)
     value sendbuf, recvbuf, comm;
{
  int len = string_length(sendbuf);
  caml_mpi_check(MPI_Allgather(String_val(sendbuf), len, MPI_BYTE,
			       String_val(recvbuf), len, MPI_BYTE,
			       Comm_val(comm)));
  return Val_unit;
}

static int op_table[] = {
  MPI_MAX, MPI_MIN, MPI_SUM, MPI_PROD, 
  MPI_LAND, MPI_LOR, MPI_LXOR,
  MPI_MAXLOC, MPI_MINLOC
};

enum { TYP_INT, TYP_FLOAT, TYP_INTINT, TYP_FLOATINT };

value caml_mpi_reduce(mlarg, typ, op, root, comm)
     value mlarg, typ, op, root, comm;
{
  /* Extract array elements to their proper C format */
  switch(Int_val(typ)) {
  case TYP_INT:
    count = Wosize_val(mlarg);
    size = sizeof(long);
    arg = stat_alloc(count * size);
    for (i = 0; i < count; i++)
      ((long *) arg)[i] = Long_val(Field(mlarg, i));
    mpi_type = MPI_LONG;
    break;
  case TYP_FLOAT:
    size = sizeof(double);
#ifdef NATIVE_CODE
    count = Wosize_val(mlarg) / Double_wosize;
    arg = stat_alloc(count * size);
    for (i = 0; i < count; i++)
      ((double *) arg)[i] = Double_field(mlarg, i));
#else
    count = Wosize_val(mlarg);
    arg = stat_alloc(count * size);
    for (i = 0; i < count; i++)
      ((double *) arg)[i] = Double_val(Field(mlarg, i));
#endif
    mpi_type = MPI_FLOAT;
    break;
  case TYP_INTINT:
    count = Wosize_val(mlarg);
    size = sizeof(struct mpi_long_int);
    arg = stat_alloc(count * size);
    for (i = 0; i < count; i++) {
      value v = Field(mlarg, i);
      ((struct mpi_long_int *) arg)[i].val = Long_val(Field(v, 0));
      ((struct mpi_long_int *) arg)[i].rank = Int_val(Field(v, 1));
    }
    mpi_type = MPI_LONG_INT;
    break;
  case TYP_FLOATINT:
    count = Wosize_val(mlarg);
    size = sizeof(struct mpi_double_int);
    arg = stat_alloc(count * size);
    for (i = 0; i < count; i++) {
      value v = Field(mlarg, i);
      ((struct mpi_double_int *) arg)[i].val = Double_val(Field(v, 0));
      ((struct mpi_double_int *) arg)[i].rank = Int_val(Field(v, 1));
    }
    mpi_type = MPI_DOUBLE_INT;
    break;
  }
  /* Allocate room for result if needed */
  MPI_Comm_rank(Comm_val(comm), &myself);
  if (Int_val(root) == myself || Bool_val(allreduce)) {
    res = stat_alloc(count * size);
  } else {
    res = NULL;
  }
  /* Do the reduce */
  if (Bool_val(allreduce)) {
    caml_mpi_check(MPI_Allreduce(arg, res, count, mpi_type,
				 op_table[Int_val(op)],
				 Comm_val(comm)));
  } else {
    caml_mpi_check(MPI_Reduce(arg, res, count, mpi_type,
			      op_table[Int_val(op)],
			      Int_val(root), Comm_val(comm)));
  }
  /* Return empty array if result not available here */
  if (res == NULL) {
    stat_free(arg);
    return Val_unit;
  }
  /* Allocate result as ML array */
  switch(Int_val(typ)) {
  case TYP_INT:
    mlres = count < Max_young_wosize ? alloc(count, 0) : alloc_shr(count, 0);
    for (i = 0; i < count; i++)
      Field(mlres, i) = Val_long(((long *) res)[i]);
    break;
  case TYP_FLOAT:
#ifdef NATIVE_CODE
    mlres = count * Double_wosize < Max_young_wosize ?
            alloc(count * Double_wosize, Double_array_tag) :
	    alloc_shr(count * Double_wosize, Double_array_tag);
    for (i = 0; i < count; i++)
      Store_double_field(mlres, i, ((double *)res)[i]);
#else
    mlres = caml_mpi_alloc_array(count, size, copy_double);
#endif
    break;
  case TYP_INTINT:
    mlres = caml_mpi_alloc_array(count, size, caml_mpi_copy_intint);
    break;
  case TYP_FLOATINT:
    mlres = caml_mpi_alloc_array(count, size, caml_mpi_copy_floatint);
    break;
  }
  stat_free(arg);
  stat_free(res);
  return mlres;
  }


  

