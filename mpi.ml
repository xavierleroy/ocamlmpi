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

(* Initialization *)

exception Error of string

external init : string array -> unit = "caml_mpi_init"
external finalize : unit -> unit = "caml_mpi_finalize"

let _ =
  Callback.register_exception "Mpi.Error" (Error "");
  init Sys.argv;
  at_exit finalize

(* Communicators *)

type communicator
type rank = int

external get_comm_world : unit -> communicator = "caml_mpi_get_comm_world"

let comm_world = get_comm_world()

external comm_size : communicator -> int = "caml_mpi_comm_size"
external comm_rank : communicator -> int = "caml_mpi_comm_rank"

(* Point-to-point communication *)

type tag = int

external send_string: string -> int -> int -> communicator -> unit
       = "caml_mpi_send"

let send data dest tag comm =
  let s = Marshal.to_string data [Marshal.Closures] in
  send_string s dest tag comm

external probe: int -> int -> communicator -> int * int * int
	= "caml_mpi_probe"

external receive_string: string -> int -> int -> communicator -> unit
	= "caml_mpi_receive"

let receive source tag comm =
  let (len, actual_source, actual_tag) = probe source tag comm in
  let buffer = String.create len in
  receive_string buffer source tag comm;
  let v = Marshal.from_string buffer 0 in
  v

let receive_status source tag comm =
  let (len, actual_source, actual_tag) = probe source tag comm in
  let buffer = String.create len in
  receive_string buffer source tag comm;
  let (v, _) = Marshal.from_string buffer 0 in
  (v, actual_source, actual_tag)

external get_any_tag : unit -> int = "caml_mpi_get_any_tag"
external get_any_source : unit -> int = "caml_mpi_get_any_source"

let any_tag = get_any_tag()
let any_source = get_any_source()

(* Barrier *)

external barrier : communicator -> unit = "caml_mpi_barrier"

(* Broadcast *)

external broadcast_string: string -> int -> communicator -> unit
	 = "caml_mpi_broadcast"
external broadcast_int: int -> int -> communicator -> int
	 = "caml_mpi_broadcast_long"

let broadcast data root comm =
  let myself = comm_rank comm in
  if myself = root then begin
    match data with
      None -> invalid_arg "Mpi.broadcast"
    | Some v ->
        (* Originating process marshals message, broadcast length, then
           broadcast data *)
        let data = Marshal.to_string v [Marshal.Closures] in
        broadcast_int (String.length data) root comm;
        broadcast_string data root comm;
        v
  end else begin
    (* Other processes receive length, allocate buffer, receive data,
       and unmarshal it. *)
    let len = broadcast_int 0 root comm in
    let data = String.create len in
    broadcast_string data root comm;
    Marshal.from_string data 0
  end

(* Scatter *)

external scatter_string:
  string -> int array -> string -> int -> communicator -> unit
  = "caml_mpi_scatter"

external scatter_long: int array -> int -> communicator -> int
  = "caml_mpi_scatter_long"

let scatter data root comm =
  let myself = comm_rank comm in
  let nprocs = comm_size comm in
  if myself = root then begin
    (* Check correct length for array *)
    if Array.length data <> nprocs then invalid_arg "Mpi.scatter";
    (* Marshal data to strings *)
    let buffers =
      Array.map (fun d -> Marshal.to_string d [Marshal.Closures]) data in
    (* Determine lengths of strings *)
    let lengths = Array.map String.length buffers in
    (* Scatter those lengths *)
    scatter_long lengths root comm;
    (* Build single buffer with all data *)
    let total_len = Array.fold_left (+) 0 lengths in
    let send_buffer = String.create total_len in
    let pos = ref 0 in
    for i = 0 to nprocs - 1 do
      String.blit buffers.(i) 0 send_buffer !pos lengths.(i);
      pos := !pos + lengths.(i)
    done;
    (* Allocate receive buffer *)
    let recv_buffer = String.create lengths.(myself) in
    (* Do the scatter *)
    scatter_string send_buffer lengths recv_buffer root comm;
    (* Return value for root *)
    data.(myself)
  end else begin
    (* Get our length *)
    let len = scatter_long [||] root comm in
    (* Allocate receive buffer *)
    let recv_buffer = String.create len in
    (* Do the scatter *)
    scatter_string "" [||] recv_buffer root comm;
    (* Return value received *)
    Marshal.from_string recv_buffer 0
  end

(* Gather *)

external gather_string:
  string -> string -> int array -> int -> communicator -> unit
  = "caml_mpi_gather"

external gather_long: int -> int array -> int -> communicator -> unit
  = "caml_mpi_gather_long"

let gather data root comm =
  let myself = comm_rank comm in
  let nprocs = comm_size comm in
  let send_buffer = Marshal.to_string data [Marshal.Closures] in
  if myself = root then begin
    (* Gather lengths for all data *)
    let lengths = Array.make nprocs 0 in
    gather_long (String.length send_buffer) lengths root comm;
    (* Allocate receive buffer big enough to hold all data *)
    let total_len = Array.fold_left (+) 0 lengths in
    let recv_buffer = String.create total_len in
    (* Gather the data *)
    gather_string send_buffer recv_buffer lengths root comm;
    (* Build array of results *)
    let res0 = Marshal.from_string recv_buffer 0 in
    let res = Array.make nprocs res0 in
    let pos = ref 0 in
    for i = 1 to nprocs - 1 do
      pos := !pos + lengths.(i - 1);
      res.(i) <- Marshal.from_string recv_buffer !pos
    done;
    res
  end else begin
    (* Send our length *)
    gather_long (String.length send_buffer) [||] root comm;
    (* Send our data *)
    gather_string send_buffer "" [||] root comm;
    (* Return dummy results *)
    [||]
  end

(* Gather to all *)

external allgather_string:
  string -> string -> int array -> communicator -> unit
  = "caml_mpi_allgather"

external allgather_long: int -> int array -> communicator -> unit
  = "caml_mpi_allgather_long"

let allgather data comm =
  let myself = comm_rank comm in
  let nprocs = comm_size comm in
  let send_buffer = Marshal.to_string data [Marshal.Closures] in
  (* Gather lengths for all data *)
  let lengths = Array.make nprocs 0 in
  allgather_long (String.length send_buffer) lengths comm;
  (* Allocate receive buffer big enough to hold all data *)
  let total_len = Array.fold_left (+) 0 lengths in
  let recv_buffer = String.create total_len in
  (* Gather the data *)
  allgather_string send_buffer recv_buffer lengths comm;
  (* Build array of results *)
  let res0 = Marshal.from_string recv_buffer 0 in
  let res = Array.make nprocs res0 in
  let pos = ref 0 in
  for i = 1 to nprocs - 1 do
    pos := !pos + lengths.(i - 1);
    res.(i) <- Marshal.from_string recv_buffer !pos
  done;
  res
