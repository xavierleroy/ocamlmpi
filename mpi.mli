(* Sample Caml bindings for MPI *)

type communicator

val comm_world: communicator

(* Rank of calling process in communicator *)
val comm_rank: communicator -> int

(* Number of processes in communicator *)
val comm_size: communicator -> int

(* Send message *)
val send: 'a -> int (*destination*) -> int (*tag*) -> communicator -> unit

(* Receive message *)
val receive: int (*source*) -> int (*tag*) -> communicator -> 'a

val any_tag: int
val any_source: int

(* Same, but also return status info *)
val receive_status: int (*source*) -> int (*tag*) -> communicator ->
                    'a * int (*source*) * int (*tag*)

(* Broadcast *)
(* val broadcast: 'a -> int (*root*) -> communicator -> 'a *)
val broadcast: 'a option -> communicator -> 'a

(* Reduce *)
val reduce: 'a array -> ('a -> 'a -> 'a) ->
            int (*root proc*) -> communicator -> 'a array option
val allreduce: 'a array -> ('a -> 'a -> 'a) ->
               int (*root proc*) -> communicator -> 'a array

(* Barrier *)
val barrier: communicator -> unit

(* Scatter, gather --- PROBLEM: how to represent the array in memory
   in such a way that it can be scattered? *)

val scatter: 'a array option -> communicator -> 'a
val gather: 'a -> int (*root proc*) -> communicator -> 'a array option
val allgather: 'a -> communicator -> 'a array

(* Constructing communicators *)

type group

val comm_group: communicator -> group
val group_incl: group -> int array (* ranks of members *) -> group

val comm_create: communicator -> group -> communicator

val comm_split: communicator -> int (*split key*) -> int (*rank key*) -> 
		commununicator

