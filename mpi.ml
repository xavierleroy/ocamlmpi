(***********************************************************************)
(*                                                                     *)
(*                         The Caml/MPI interface                      *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*            Non-blocking calls added by Eray Ozkural                 *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Initialization *)

exception Error of string

let mpi_error s = raise(Error s)

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

external comm_compare:
    communicator -> communicator -> bool
    = "caml_mpi_comm_compare"

type color = int
external comm_split:
    communicator -> color -> int -> communicator
    = "caml_mpi_comm_split"

external get_undefined : unit -> int = "caml_mpi_get_undefined"

let color_none = get_undefined()

external cart_create:
    communicator -> int array -> bool array -> bool -> communicator
    = "caml_mpi_cart_create"
external dims_create: int -> int array -> int array = "caml_mpi_dims_create"
external cart_rank: communicator -> int array -> rank = "caml_mpi_cart_rank"
external cart_coords:
    communicator -> rank -> int array
    = "caml_mpi_cart_coords"

(* Point-to-point communication *)

type tag = int

external get_any_tag : unit -> int = "caml_mpi_get_any_tag"
external get_any_source : unit -> int = "caml_mpi_get_any_source"

let any_tag = get_any_tag()
let any_source = get_any_source()

external send_basic:
    'a -> Marshal.extern_flags list -> rank -> tag -> communicator -> unit
    = "caml_mpi_send"

let send data dest tag comm =
  send_basic data [Marshal.Closures] dest tag comm

external probe:
    int -> int -> communicator -> int * int * int
    = "caml_mpi_probe"

external iprobe:
    int -> int -> communicator -> (int * int * int) option
    = "caml_mpi_iprobe"

external receive_basic:
    int -> rank -> tag -> communicator -> 'a
    = "caml_mpi_receive"

let receive source tag comm =
  let (len, actual_source, actual_tag) = probe source tag comm in
  receive_basic len source tag comm

let receive_status source tag comm =
  let (len, actual_source, actual_tag) = probe source tag comm in
  let v = receive_basic len source tag comm in
  (v, actual_source, actual_tag)

let probe source tag comm =
  let (len, actual_source, actual_tag) = probe source tag comm in
  (actual_source, actual_tag)

let iprobe source tag comm =
  match iprobe source tag comm with
  | None -> None
  | Some (len, actual_source, actual_tag) -> Some (actual_source, actual_tag)

external send_int:
    int -> rank -> tag -> communicator -> unit
    = "caml_mpi_send_int"
external receive_int:
    rank -> tag -> communicator -> int
    = "caml_mpi_receive_int"

external send_float:
    float -> rank -> tag -> communicator -> unit
    = "caml_mpi_send_float"
external receive_float:
    rank -> tag -> communicator -> float
    = "caml_mpi_receive_float"

external send_int_array:
    int array -> rank -> tag -> communicator -> unit
    = "caml_mpi_send_intarray"
external receive_int_array:
    int array -> rank -> tag -> communicator -> unit
    = "caml_mpi_receive_intarray"

external send_float_array:
    float array -> rank -> tag -> communicator -> unit
    = "caml_mpi_send_float"
external receive_float_array:
    float array -> rank -> tag -> communicator -> unit
    = "caml_mpi_receive_floatarray"

external send_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t -> rank -> tag -> communicator -> unit
    = "caml_mpi_send_bigarray"
external receive_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t -> rank -> tag -> communicator -> unit
    = "caml_mpi_receive_bigarray"

let send_bigarray0 x = send_bigarray (Bigarray.(genarray_of_array0 x))
let send_bigarray1 x = send_bigarray (Bigarray.(genarray_of_array1 x))
let send_bigarray2 x = send_bigarray (Bigarray.(genarray_of_array2 x))
let send_bigarray3 x = send_bigarray (Bigarray.(genarray_of_array3 x))

let receive_bigarray0 x = receive_bigarray (Bigarray.(genarray_of_array0 x))
let receive_bigarray1 x = receive_bigarray (Bigarray.(genarray_of_array1 x))
let receive_bigarray2 x = receive_bigarray (Bigarray.(genarray_of_array2 x))
let receive_bigarray3 x = receive_bigarray (Bigarray.(genarray_of_array3 x))

(* Non-blocking communication *)

type request

external alloc_request : unit -> request = "caml_mpi_alloc_request"



let null_request = alloc_request ()

external isend_basic:
  'a -> Marshal.extern_flags list -> rank -> tag -> communicator -> request
  = "caml_mpi_isend"

let isend data dest tag comm =
  isend_basic data [Marshal.Closures] dest tag comm

external isend_basic_varlength:
  'a -> Marshal.extern_flags list -> rank -> tag -> communicator -> 
  request * request
  = "caml_mpi_isend_varlength"

let isend_varlength data dest tag comm =
  isend_basic_varlength data [Marshal.Closures] dest tag comm

external ireceive:
  int -> rank -> tag -> communicator -> request = "caml_mpi_ireceive"

external ireceive_varlength:
  rank -> tag -> communicator -> request 
  = "caml_mpi_ireceive_varlength"

external wait: request -> unit = "caml_mpi_wait"

let wait_pair (req1,req2) = wait req1; wait req2

external wait_receive: request -> 'a = "caml_mpi_wait_receive"

(* Barrier *)

external barrier : communicator -> unit = "caml_mpi_barrier"

(* Broadcast *)

external broadcast_bytes: bytes -> int -> communicator -> unit
	 = "caml_mpi_broadcast"
external broadcast_int: int -> int -> communicator -> int
	 = "caml_mpi_broadcast_int"

let broadcast v root comm =
  let myself = comm_rank comm in
  if myself = root then begin
    let data = Marshal.to_bytes v [Marshal.Closures] in
    ignore(broadcast_int (Bytes.length data) root comm);
    broadcast_bytes data root comm;
    v
  end else begin
    (* Other processes receive length, allocate buffer, receive data,
       and unmarshal it. *)
    let len = broadcast_int 0 root comm in
    let data = Bytes.create len in
    broadcast_bytes data root comm;
    Marshal.from_bytes data 0
  end

let broadcast_opt data root comm =
  match data with
    Some d ->
      broadcast d root comm
  | None ->
      if root = comm_rank comm
      then mpi_error "Mpi.broadcast_opt: no data at root"
      else broadcast (Obj.magic ()) root comm

external broadcast_float:
    float -> rank -> communicator -> float
    = "caml_mpi_broadcast_float"
external broadcast_int_array:
    int array -> rank -> communicator -> unit
    = "caml_mpi_broadcast_intarray"
external broadcast_float_array:
    float array -> rank -> communicator -> unit
    = "caml_mpi_broadcast_floatarray"
external broadcast_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t -> rank -> communicator -> unit
    = "caml_mpi_broadcast_bigarray"

let broadcast_bigarray0 x = broadcast_bigarray (Bigarray.(genarray_of_array0 x))
let broadcast_bigarray1 x = broadcast_bigarray (Bigarray.(genarray_of_array1 x))
let broadcast_bigarray2 x = broadcast_bigarray (Bigarray.(genarray_of_array2 x))
let broadcast_bigarray3 x = broadcast_bigarray (Bigarray.(genarray_of_array3 x))

(* Scatter *)

external scatter_bytes:
  bytes -> int array -> bytes -> int -> communicator -> unit
  = "caml_mpi_scatter"

external scatter_int: int array -> int -> communicator -> int
  = "caml_mpi_scatter_int"

let scatter data root comm =
  let myself = comm_rank comm in
  let nprocs = comm_size comm in
  if myself = root then begin
    (* Check correct length for array *)
    if Array.length data <> nprocs
    then mpi_error "Mpi.scatter: wrong array size";
    (* Marshal data to bytes *)
    let buffers =
      Array.map (fun d -> Marshal.to_bytes d [Marshal.Closures]) data in
    (* Determine lengths of bytes *)
    let lengths = Array.map Bytes.length buffers in
    (* Scatter those lengths *)
    ignore(scatter_int lengths root comm);
    (* Build single buffer with all data *)
    let total_len = Array.fold_left (+) 0 lengths in
    let send_buffer = Bytes.create total_len in
    let pos = ref 0 in
    for i = 0 to nprocs - 1 do
      Bytes.blit buffers.(i) 0 send_buffer !pos lengths.(i);
      pos := !pos + lengths.(i)
    done;
    (* Allocate receive buffer *)
    let recv_buffer = Bytes.create lengths.(myself) in
    (* Do the scatter *)
    scatter_bytes send_buffer lengths recv_buffer root comm;
    (* Return value for root *)
    data.(myself)
  end else begin
    (* Get our length *)
    let len = scatter_int [||] root comm in
    (* Allocate receive buffer *)
    let recv_buffer = Bytes.create len in
    (* Do the scatter *)
    scatter_bytes Bytes.empty [||] recv_buffer root comm;
    (* Return value received *)
    Marshal.from_bytes recv_buffer 0
  end

external scatter_float:
    float array -> rank -> communicator -> float
    = "caml_mpi_scatter_float"

external scatter_from_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t -> rank -> communicator -> 'a
    = "caml_mpi_scatter_from_bigarray"

let scatter_from_bigarray1 x =
  scatter_from_bigarray (Bigarray.(genarray_of_array1 x))
let scatter_from_bigarray2 x =
  scatter_from_bigarray (Bigarray.(genarray_of_array2 x))
let scatter_from_bigarray3 x =
  scatter_from_bigarray (Bigarray.(genarray_of_array3 x))

external scatter_int_array:
    int array -> int array -> rank -> communicator -> unit
    = "caml_mpi_scatter_intarray"
let scatter_int_array src dst rank comm =
  if rank = comm_rank comm
  && Array.length src <> Array.length dst * comm_size comm
  then mpi_error "Mpi.scatter_int_array: array size mismatch"
  else scatter_int_array src dst rank comm

external scatter_float_array:
    float array -> float array -> rank -> communicator -> unit
    = "caml_mpi_scatter_floatarray"
let scatter_float_array src dst rank comm =
  if rank = comm_rank comm
  && Array.length src <> Array.length dst * comm_size comm
  then mpi_error "Mpi.scatter_float_array: array size mismatch"
  else scatter_float_array src dst rank comm

external scatter_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t  -> ('a, 'b, 'c) Bigarray.Genarray.t
    -> rank -> communicator -> unit
    = "caml_mpi_scatter_bigarray"

let scatter_bigarray1 s d = scatter_bigarray (Bigarray.(genarray_of_array1 s))
                                             (Bigarray.(genarray_of_array1 d))

(* Gather *)

external gather_bytes:
  bytes -> bytes -> int array -> int -> communicator -> unit
  = "caml_mpi_gather"

external gather_int: int -> int array -> int -> communicator -> unit
  = "caml_mpi_gather_int"

let gather data root comm =
  let myself = comm_rank comm in
  let nprocs = comm_size comm in
  let send_buffer = Marshal.to_bytes data [Marshal.Closures] in
  if myself = root then begin
    (* Gather lengths for all data *)
    let lengths = Array.make nprocs 0 in
    gather_int (Bytes.length send_buffer) lengths root comm;
    (* Allocate receive buffer big enough to hold all data *)
    let total_len = Array.fold_left (+) 0 lengths in
    let recv_buffer = Bytes.create total_len in
    (* Gather the data *)
    gather_bytes send_buffer recv_buffer lengths root comm;
    (* Build array of results *)
    let res0 = Marshal.from_bytes recv_buffer 0 in
    let res = Array.make nprocs res0 in
    let pos = ref 0 in
    for i = 1 to nprocs - 1 do
      pos := !pos + lengths.(i - 1);
      res.(i) <- Marshal.from_bytes recv_buffer !pos
    done;
    res
  end else begin
    (* Send our length *)
    gather_int (Bytes.length send_buffer) [||] root comm;
    (* Send our data *)
    gather_bytes send_buffer Bytes.empty [||] root comm;
    (* Return dummy results *)
    [||]
  end

let gather_int src dst rank comm =
  if rank = comm_rank comm
  && Array.length dst <> comm_size comm
  then mpi_error "Mpi.gather_int: array size mismatch"
  else gather_int src dst rank comm

external gather_float:
    float -> float array -> rank -> communicator -> unit
    = "caml_mpi_gather_float"
let gather_float src dst rank comm =
  if rank = comm_rank comm
  && Array.length dst <> comm_size comm
  then mpi_error "Mpi.gather_float: array size mismatch"
  else gather_float src dst rank comm

external gather_to_bigarray:
    'a -> ('a, 'b, 'c) Bigarray.Genarray.t -> rank -> communicator -> unit
    = "caml_mpi_gather_to_bigarray"

let gather_to_bigarray1 s d =
  gather_to_bigarray s (Bigarray.(genarray_of_array1 d))
let gather_to_bigarray2 s d =
  gather_to_bigarray s (Bigarray.(genarray_of_array2 d))
let gather_to_bigarray3 s d =
  gather_to_bigarray s (Bigarray.(genarray_of_array3 d))

external gather_int_array:
    int array -> int array -> rank -> communicator -> unit
    = "caml_mpi_gather_intarray"
let gather_int_array src dst rank comm =
  if rank = comm_rank comm
  && Array.length dst <> Array.length src * comm_size comm
  then mpi_error "Mpi.gather_int_array: array size mismatch"
  else gather_int_array src dst rank comm

external gather_float_array:
    float array -> float array -> rank -> communicator -> unit
    = "caml_mpi_gather_float"
let gather_float_array src dst rank comm =
  if rank = comm_rank comm
  && Array.length dst <> Array.length src * comm_size comm
  then mpi_error "Mpi.gather_float_array: array size mismatch"
  else gather_float_array src dst rank comm

external gather_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t  -> ('a, 'b, 'c) Bigarray.Genarray.t
    -> rank -> communicator -> unit
    = "caml_mpi_gather_bigarray"

let gather_bigarray1 s d = gather_bigarray (Bigarray.(genarray_of_array1 s))
                                           (Bigarray.(genarray_of_array1 d))

(* Gather to all *)

external allgather_bytes:
  bytes -> bytes -> int array -> communicator -> unit
  = "caml_mpi_allgather"

external allgather_int: int -> int array -> communicator -> unit
  = "caml_mpi_allgather_int"

let allgather data comm =
  let nprocs = comm_size comm in
  let send_buffer = Marshal.to_bytes data [Marshal.Closures] in
  (* Gather lengths for all data *)
  let lengths = Array.make nprocs 0 in
  allgather_int (Bytes.length send_buffer) lengths comm;
  (* Allocate receive buffer big enough to hold all data *)
  let total_len = Array.fold_left (+) 0 lengths in
  let recv_buffer = Bytes.create total_len in
  (* Gather the data *)
  allgather_bytes send_buffer recv_buffer lengths comm;
  (* Build array of results *)
  let res0 = Marshal.from_bytes recv_buffer 0 in
  let res = Array.make nprocs res0 in
  let pos = ref 0 in
  for i = 1 to nprocs - 1 do
    pos := !pos + lengths.(i - 1);
    res.(i) <- Marshal.from_bytes recv_buffer !pos
  done;
  res

let allgather_int src dst comm =
  if Array.length dst <> comm_size comm
  then mpi_error "MPI.allgather_int: array size mismatch"
  else allgather_int src dst comm

external allgather_float:
    float -> float array -> communicator -> unit
    = "caml_mpi_allgather_float"
let allgather_float src dst comm =
  if Array.length dst <> comm_size comm
  then mpi_error "MPI.allgather_float: array size mismatch"
  else allgather_float src dst comm

external allgather_to_bigarray:
    'a -> ('a, 'b, 'c) Bigarray.Genarray.t -> communicator -> unit
    = "caml_mpi_allgather_to_bigarray"

let allgather_to_bigarray1 s d =
  allgather_to_bigarray s (Bigarray.(genarray_of_array1 d))
let allgather_to_bigarray2 s d =
  allgather_to_bigarray s (Bigarray.(genarray_of_array2 d))
let allgather_to_bigarray3 s d =
  allgather_to_bigarray s (Bigarray.(genarray_of_array3 d))

external allgather_int_array:
    int array -> int array -> communicator -> unit
    = "caml_mpi_allgather_intarray"
let allgather_int_array src dst comm =
  if Array.length dst <> Array.length src * comm_size comm
  then mpi_error "MPI.allgather_int_array: array size mismatch"
  else allgather_int_array src dst comm

external allgather_float_array:
    float array -> float array -> communicator -> unit
    = "caml_mpi_allgather_float"
let allgather_float_array src dst comm =
  if Array.length dst <> Array.length src * comm_size comm
  then mpi_error "MPI.allgather_float_array: array size mismatch"
  else allgather_float_array src dst comm

external allgather_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t  -> ('a, 'b, 'c) Bigarray.Genarray.t
    -> communicator -> unit
    = "caml_mpi_allgather_bigarray"

let allgather_bigarray1 s d =
  allgather_bigarray (Bigarray.(genarray_of_array1 s))
                     (Bigarray.(genarray_of_array1 d))

(* Alltoall *)

external alltoall_int_array:
  int array -> int array -> communicator -> unit
  = "caml_mpi_alltoall_intarray"

external alltoall_bytes:
  bytes -> int array -> bytes -> int array -> communicator -> unit
  = "caml_mpi_alltoall"

let alltoall data comm =
  let nprocs = comm_size comm in
  (* Check correct length for array *)
  if Array.length data <> nprocs
  then mpi_error "Mpi.alltoall: wrong array size";
  (* Marshal data to bytes *)
  let send_buffers =
    Array.map (fun d -> Marshal.to_bytes d [Marshal.Closures]) data in
  (* Determine lengths of bytes *)
  let send_lengths = Array.map Bytes.length send_buffers in
  let recv_lengths = Array.make nprocs 0 in
  (* Swap lengths between processes *)
  alltoall_int_array send_lengths recv_lengths comm;
  (* Build single buffer with all data *)
  let total_sendlen = Array.fold_left (+) 0 send_lengths in
  let send_buffer = Bytes.create total_sendlen in
  let pos = ref 0 in
  for i = 0 to nprocs - 1 do
    Bytes.blit send_buffers.(i) 0 send_buffer !pos send_lengths.(i);
    pos := !pos + send_lengths.(i)
  done;
  (* Allocate receive buffer big enough to hold all data *)
  let total_recvlen = Array.fold_left (+) 0 recv_lengths in
  let recv_buffer = Bytes.create total_recvlen in
  (* Send and receive *)
  alltoall_bytes send_buffer send_lengths recv_buffer recv_lengths comm;
  (* Build array of results *)
  let res0 = Marshal.from_bytes recv_buffer 0 in
  let res = Array.make nprocs res0 in
  let pos = ref 0 in
  for i = 1 to nprocs - 1 do
    pos := !pos + recv_lengths.(i - 1);
    res.(i) <- Marshal.from_bytes recv_buffer !pos
  done;
  res

let alltoall_int_array src dst comm =
  if Array.length src <> Array.length dst
  then mpi_error "Mpi.alltoall_int_array: array size mismatch"
  else alltoall_int_array src dst comm
external alltoall_float_array:
  float array -> float array -> communicator -> unit
  = "caml_mpi_alltoall_floatarray"
let alltoall_float_array src dst comm =
  if Array.length src <> Array.length dst
  then mpi_error "Mpi.alltoall_float_array: array size mismatch"
  else alltoall_float_array src dst comm
external alltoall_bigarray:
  ('a, 'b, 'c) Bigarray.Genarray.t  -> ('a, 'b, 'c) Bigarray.Genarray.t
  -> communicator -> unit
  = "caml_mpi_alltoall_bigarray"

let alltoall_bigarray1 s d =
  alltoall_bigarray (Bigarray.(genarray_of_array1 s))
                    (Bigarray.(genarray_of_array1 d))
let alltoall_bigarray2 s d =
  alltoall_bigarray (Bigarray.(genarray_of_array2 s))
                    (Bigarray.(genarray_of_array2 d))
let alltoall_bigarray3 s d =
  alltoall_bigarray (Bigarray.(genarray_of_array3 s))
                    (Bigarray.(genarray_of_array3 d))

(* Reduce *)

type _ op =
    Max  : [< `Int | `Float ] op
  | Min  : [< `Int | `Float ] op
  | Sum  : [< `Int | `Float ] op
  | Prod : [< `Int | `Float ] op
  | Land : [< `Int ] op
  | Lor  : [< `Int ] op
  | Xor  : [< `Int ] op
  | Int_max  : [< `Int ] op
  | Int_min  : [< `Int ] op
  | Int_sum  : [< `Int ] op
  | Int_prod : [< `Int ] op
  | Int_land : [< `Int ] op
  | Int_lor  : [< `Int ] op
  | Int_xor  : [< `Int ] op
  | Float_max  : [< `Float ] op
  | Float_min  : [< `Float ] op
  | Float_sum  : [< `Float ] op
  | Float_prod : [< `Float ] op

external reduce_int:
  int -> [`Int] op -> rank -> communicator -> int
    = "caml_mpi_reduce_int"
external reduce_float:
    float -> [`Float] op -> rank -> communicator -> float
    = "caml_mpi_reduce_float"
external reduce_int_array:
    int array -> int array -> [`Int] op -> rank -> communicator -> unit
    = "caml_mpi_reduce_intarray"
let reduce_int_array src dst op rank comm =
  if rank = comm_rank comm && Array.length src <> Array.length dst
  then mpi_error "Mpi.reduce_int_array: array size mismatch"
  else reduce_int_array src dst op rank comm

external reduce_float_array:
    float array -> float array -> [`Float] op
    -> rank -> communicator -> unit
    = "caml_mpi_reduce_floatarray"
let reduce_float_array src dst op rank comm =
  if rank = comm_rank comm && Array.length src <> Array.length dst
  then mpi_error "Mpi.reduce_float_array: array size mismatch"
  else reduce_float_array src dst op rank comm

external reduce_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t  -> ('a, 'b, 'c) Bigarray.Genarray.t
    -> 'any op -> rank -> communicator -> unit
    = "caml_mpi_reduce_bigarray"

let reduce_bigarray0 s d = reduce_bigarray (Bigarray.(genarray_of_array0 s))
                                           (Bigarray.(genarray_of_array0 d))
let reduce_bigarray1 s d = reduce_bigarray (Bigarray.(genarray_of_array1 s))
                                           (Bigarray.(genarray_of_array1 d))
let reduce_bigarray2 s d = reduce_bigarray (Bigarray.(genarray_of_array2 s))
                                           (Bigarray.(genarray_of_array2 d))
let reduce_bigarray3 s d = reduce_bigarray (Bigarray.(genarray_of_array3 s))
                                           (Bigarray.(genarray_of_array3 d))

(* Reduce at all nodes *)

external allreduce_int:
    int -> [`Int] op -> communicator -> int
    = "caml_mpi_allreduce_int"
external allreduce_float:
    float -> [`Float] op -> communicator -> float
    = "caml_mpi_allreduce_float"
external allreduce_int_array:
    int array -> int array -> [`Int] op -> communicator -> unit
    = "caml_mpi_allreduce_intarray"
let allreduce_int_array src dst op comm =
  if Array.length src <> Array.length dst
  then mpi_error "Mpi.allreduce_int_array: array size mismatch"
  else allreduce_int_array src dst op comm

external allreduce_float_array:
    float array -> float array -> [`Float] op -> communicator -> unit
    = "caml_mpi_allreduce_floatarray"
let allreduce_float_array src dst op comm =
  if Array.length src <> Array.length dst
  then mpi_error "Mpi.allreduce_float_array: array size mismatch"
  else allreduce_float_array src dst op comm

external allreduce_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t  -> ('a, 'b, 'c) Bigarray.Genarray.t
    -> 'any op -> communicator -> unit
    = "caml_mpi_allreduce_bigarray"

let allreduce_bigarray0 s d =
  allreduce_bigarray (Bigarray.(genarray_of_array0 s))
                     (Bigarray.(genarray_of_array0 d))
let allreduce_bigarray1 s d =
  allreduce_bigarray (Bigarray.(genarray_of_array1 s))
                     (Bigarray.(genarray_of_array1 d))
let allreduce_bigarray2 s d =
  allreduce_bigarray (Bigarray.(genarray_of_array2 s))
                     (Bigarray.(genarray_of_array2 d))
let allreduce_bigarray3 s d =
  allreduce_bigarray (Bigarray.(genarray_of_array3 s))
                     (Bigarray.(genarray_of_array3 d))

(* Scan *)

external scan_int: int -> [`Int] op -> communicator -> int
    = "caml_mpi_scan_int"

external scan_float:
    float -> [`Float] op -> communicator -> float
    = "caml_mpi_scan_float"

external scan_int_array:
    int array -> int array -> [`Int] op -> communicator -> unit
    = "caml_mpi_scan_intarray"
let scan_int_array src dst op comm =
  if Array.length dst <> Array.length src
  then mpi_error "Mpi.scan_int_array: array size mismatch"
  else scan_int_array src dst op comm

external scan_float_array:
    float array -> float array -> [`Float] op -> communicator -> unit
    = "caml_mpi_scan_floatarray"
let scan_float_array src dst op comm =
  if Array.length dst <> Array.length src
  then mpi_error "Mpi.scan_float_array: array size mismatch"
  else scan_float_array src dst op comm

external scan_bigarray:
    ('a, 'b, 'c) Bigarray.Genarray.t  -> ('a, 'b, 'c) Bigarray.Genarray.t
    -> 'any op -> communicator -> unit
    = "caml_mpi_scan_bigarray"

let scan_bigarray0 s d = scan_bigarray (Bigarray.(genarray_of_array0 s))
                                       (Bigarray.(genarray_of_array0 d))
let scan_bigarray1 s d = scan_bigarray (Bigarray.(genarray_of_array1 s))
                                       (Bigarray.(genarray_of_array1 d))
let scan_bigarray2 s d = scan_bigarray (Bigarray.(genarray_of_array2 s))
                                       (Bigarray.(genarray_of_array2 d))
let scan_bigarray3 s d = scan_bigarray (Bigarray.(genarray_of_array3 s))
                                       (Bigarray.(genarray_of_array3 d))

(*** Process group management *)

type group

external comm_create: communicator -> group -> communicator = "caml_mpi_comm_create"

external group_size: group -> int = "caml_mpi_group_size"
external group_rank: group -> int = "caml_mpi_group_rank"
external group_translate_ranks: group -> int array -> group -> int array = "caml_mpi_group_translate_ranks"

external comm_group: communicator -> group = "caml_mpi_comm_group"
external group_union: group -> group -> group = "caml_mpi_group_union"
external group_intersection: group -> group -> group = "caml_mpi_group_intersection"
external group_difference: group -> group -> group = "caml_mpi_group_difference"

external group_incl: group -> int array -> group = "caml_mpi_group_incl"
external group_excl: group -> int array -> group = "caml_mpi_group_excl"

type group_range = { range_first: int; range_last: int; range_stride: int }

external group_range_incl: group -> group_range array -> group = "caml_mpi_group_range_incl"
external group_range_excl: group -> group_range array -> group = "caml_mpi_group_range_excl"

(* Miscellaneous *)

external wtime: unit -> float = "caml_mpi_wtime"
