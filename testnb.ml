
(* $Id: test.ml 18 2003-03-31 14:22:57Z xleroy $ *)

(* Regression test *)

open Printf
open Mpi

let print_list printelt l =
  printf "[ ";
  List.iter (fun x-> printelt x; printf "; ") l;
  printf " ]"

let print_int x = printf "%d" x

let print_float x = printf "%f" x

let print_intlist l = print_list print_int l

(* comm_size, comm_rank *)

let size = comm_size comm_world
let myrank = comm_rank comm_world

(* Non-blocking comms *)

(* test between 0 and 1 *)


let _ =
  if myrank = 0 then (
    printf "rank 0: testing variable length non-blocking comms, sending a list [16;32] to proc 1\n";
    let req_pair = Mpi.isend_varlength [16;32] 1 8 comm_world in
        wait_pair req_pair
  )
  else 
    if myrank = 1 then (
      let req = Mpi.ireceive_varlength 0 8 comm_world in
      let x = wait_receive req in
        printf "rank 1: received "; print_intlist x; printf "\n"
    )

let _ =
  if myrank = 0 then (
    printf "rank 0: testing plain non-blocking comms, sending integer 5 to proc 1\n";
    let req = Mpi.isend 5 1 8 comm_world in
        wait req
  )
  else 
    if myrank = 1 then (
      let req = Mpi.ireceive 100 0 8 comm_world in
      let x = wait_receive req in
        printf "rank 1: received %d\n" x
    )



let _ = 
  if myrank = 0 then (
    printf "rank 0: testing non-blocking comms, sending a string to proc 1\n";
    let reqpair = Mpi.isend_varlength "ocaml rules" 1 8 comm_world in
      wait_pair reqpair
  )
  else 
    if myrank = 1 then (
      let req = Mpi.ireceive_varlength 0 8 comm_world in
      let x = wait_receive req in
        printf "rank 1: received string: %s\n" x
    )

