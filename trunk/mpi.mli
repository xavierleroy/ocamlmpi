(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
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
external comm_size : communicator -> int = "caml_mpi_comm_size"
external comm_rank : communicator -> rank = "caml_mpi_comm_rank"

type tag = int

val send: 'a -> rank -> tag -> communicator -> unit
val receive: rank -> tag -> communicator -> 'a
val receive_status: rank -> tag -> communicator -> 'a * rank * tag
val probe: rank -> tag -> communicator -> int * rank * tag

val any_tag: tag
val any_source: rank

val barrier: communicator -> unit
val broadcast: 'a option -> rank -> communicator -> 'a
val scatter: 'a array -> rank -> communicator -> 'a
val gather: 'a -> rank -> communicator -> 'a array
val allgather: 'a -> communicator -> 'a array

