(***********************************************************************)
(*                                                                     *)
(*                         The Caml/MPI interface                      *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Caml bindings for the Message Passing Interface (MPI) library *)

type communicator

type rank = int

val comm_world: communicator
external comm_size: communicator -> int = "caml_mpi_comm_size"
external comm_rank: communicator -> rank = "caml_mpi_comm_rank"

type tag = int

val any_tag: tag
val any_source: rank

val send: 'a -> rank -> tag -> communicator -> unit
val receive: rank -> tag -> communicator -> 'a
val receive_status: rank -> tag -> communicator -> 'a * rank * tag
val probe: rank -> tag -> communicator -> rank * tag

val send_int: int -> rank -> tag -> communicator -> unit
val receive_int: rank -> tag -> communicator -> int

val send_float: float -> rank -> tag -> communicator -> unit
val receive_float: rank -> tag -> communicator -> float

val send_int_array: int array -> rank -> tag -> communicator -> unit
val receive_int_array: int array -> rank -> tag -> communicator -> unit

val send_float_array: float array -> rank -> tag -> communicator -> unit
val receive_float_array: float array -> rank -> tag -> communicator -> unit

val barrier: communicator -> unit

val broadcast: 'a -> rank -> communicator -> 'a
val broadcast_opt: 'a option -> rank -> communicator -> 'a
val broadcast_int: int -> rank -> communicator -> int
val broadcast_float: float -> rank -> communicator -> float
val broadcast_int_array: int array -> rank -> communicator -> unit
val broadcast_float_array: float array -> rank -> communicator -> unit

val scatter: 'a array -> rank -> communicator -> 'a
val scatter_int: int array -> rank -> communicator -> int
val scatter_float: float array -> rank -> communicator -> float
val scatter_int_array: int array -> int array -> rank -> communicator -> unit
val scatter_float_array:
  float array -> float array -> rank -> communicator -> unit

val gather: 'a -> rank -> communicator -> 'a array
val gather_int: int -> int array -> rank -> communicator -> unit
val gather_float: float -> float array -> rank -> communicator -> unit
val gather_int_array: int array -> int array -> rank -> communicator -> unit
val gather_float_array:
  float array -> float array -> rank -> communicator -> unit

val allgather: 'a -> communicator -> 'a array
val allgather_int: int -> int array -> communicator -> unit
val allgather_float: float -> float array -> communicator -> unit
val allgather_int_array: int array -> int array -> communicator -> unit
val allgather_float_array:
  float array -> float array -> communicator -> unit

type intop =
  Int_max | Int_min | Int_sum | Int_prod | Int_land | Int_lor | Int_xor
type floatop =
  Float_max | Float_min | Float_sum | Float_prod

val reduce_int: int -> intop -> rank -> communicator -> int
val reduce_float: float -> floatop -> rank -> communicator -> float
val reduce_int_array:
  int array -> int array -> intop -> rank -> communicator -> unit
val reduce_float_array:
  float array -> float array -> floatop -> rank -> communicator -> unit

val allreduce_int: int -> intop -> communicator -> int
val allreduce_float: float -> floatop -> communicator -> float
val allreduce_int_array:
  int array -> int array -> intop -> communicator -> unit
val allreduce_float_array:
  float array -> float array -> floatop -> communicator -> unit

val scan_int: int -> int array -> intop -> communicator -> unit
val scan_float: float -> float array -> floatop -> communicator -> unit
val scan_int_array: int array -> int array -> intop -> communicator -> unit
val scan_float_array:
  float array -> float array -> floatop -> communicator -> unit

val comm_compare: communicator -> communicator -> bool
type color = int
val comm_split: communicator -> color -> int -> communicator
val color_none: color

external wtime: unit -> float = "caml_mpi_wtime"
