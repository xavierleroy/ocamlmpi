external send_string: string -> int -> int -> communicator -> unit
       = "caml_mpi_send"

let send data dest tag comm =
  let s = Marshal.to_string data in
  send_string s dest tag comm

external probe: int -> int -> communicator -> int * int * int
	= "caml_mpi_probe"

external receive_string: string -> int -> int -> communicator -> unit
	= "caml_mpi_receive"

let receive source tag comm =
  let (len, actual_source, actual_tag) = probe source tag comm in
  let buffer = String.create len in
  receive_string buffer source tag comm;
  let (v, _) = Marshal.from_string buffer 0 in
  v

let receive_status source tag comm =
  let (len, actual_source, actual_tag) = probe source tag comm in
  let buffer = String.create len in
  receive_string buffer source tag comm;
  let (v, _) = Marshal.from_string buffer 0 in
  (v, actual_source, actual_tag)

external get_any_tag : unit -> int = "caml_mpi_any_tag"
external get_any_source : unit -> int = "caml_mpi_any_source"

let any_tag = get_any_tag()
let any_source = get_any_source()

external broadcast_string: string -> int -> communicator -> unit
	 = "caml_mpi_broadcast"
external broadcast_uint: int -> int -> communicator -> int
	 = "caml_mpi_broadcast_uint"

let broadcast data root comm =
  let myself = comm_rank comm in
  match data with
    Some v when myself = root ->
      (* Originating process marshals message, broadcast length, then
         broadcast data *)
      let data = Marshal.to_string v in
      broadcast_uint (String.length data) root comm;
      broadcast_string data root comm;
      v
  | None when myself <> root ->
      (* Other processes receive length, allocate buffer, receive data,
         and unmarshal it. *)
      let len = broadcast_uint 0 root comm in
      let data = String.create len in
      broadcast_string data root comm;
      let (v, _) = Marshal.from_string data 0 in
      v
  | _ -> invalid_arg "Mpi.broadcast"

external scatter_string: string -> string -> int -> communicator
	= "caml_mpi_scatter"

let scatter data root comm =
  let myself = comm_rank comm in
  let nprocs = comm_size comm in
  let len = ref 0 in
  let send_buffer =
    if myself = root then begin
      (* Check correct length for array *)
      if Array.length data <> nprocs then invalid_arg "Mpi.scatter";
      (* Marshal data to strings *)
      let buffers = Array.map Marshal.to_string data in
      (* Determine max length of string *)
      let len = ref 0 in
      for i = 0 to nprocs - 1 do
        let l = String.length buffers.(i) in
        if l > !len then len := l
      done;
      (* Broadcast that length *)
      broadcast_uint !len root comm;
      (* Build single buffer *)
      let buffer = String.create (!len * nprocs) in
      for i = 0 to nprocs - 1 do
        String.blit buffers.[i] 0
                    buffer (i * !len)
                    (String.length buffers.[i])
      done;
      buffer
    end else
      (* Receive buffer length *)
      len := broadcast_uint 0 root comm;
      ""
    end in
  (* Allocate receive buffer *)
  let recv_buffer = String.create !len in
  (* Do the scatter *)
  scatter_string send_buffer recv_buffer root comm;
  (* Unmarshal the result *)
  let (v, _) = Marshal.from_string recv_buffer 0 in
  v

external gather_string: string -> string -> int -> communicator -> unit
	= "caml_mpi_gather"

let gather data root comm =
  let myself = comm_rank comm in
  let nprocs = comm_size comm in
  let buff = Marshal.to_string data in
  let red_res = allreduce_int [| String.length buff |] MPI_MAX comm in
  let len = red_res.(0) in
  let send_buffer = String.create len in
  String.blit buff 0 send_buffer 0 (String.length buff);
  if myself = root then begin
    let recv_buffer = String.create (len * nprocs) in
    gather_string send_buffer recv_buffer root comm;
    let (v0, _) = Marshal.from_string recv_buffer 0 in
    let res = Array.create nprocs v0 in
    for i = 1 to nprocs - 1 do
      let (vi, _) = Marshal.from_string recv_buffer (i * len) in
      res.(i) <- vi
    done;
    res
  end else begin
    gather_string send_buffer "" root comm;
    [||]
  end

external allgather_string: string -> string -> communicator -> unit
	= "caml_mpi_allgather"

let allgather data comm =
  let nprocs = comm_size comm in
  let buff = Marshal.to_string data in
  let red_res = allreduce_int [| String.length buff |] MPI_MAX comm in
  let len = red_res.(0) in
  let send_buffer = String.create len in
  String.blit buff 0 send_buffer 0 (String.length buff);
  let recv_buffer = String.create (len * nprocs) in
  allgather_string send_buffer recv_buffer comm;
  let (v0, _) = Marshal.from_string recv_buffer 0 in
  let res = Array.create nprocs v0 in
  for i = 1 to nprocs - 1 do
    let (vi, _) = Marshal.from_string recv_buffer (i * len) in
    res.(i) <- vi
  done;
  res

