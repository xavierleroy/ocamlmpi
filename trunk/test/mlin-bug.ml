(* ocamlopt -o test -I +ocamlmpi mpi.cmxa test.ml *)
let me = Mpi.comm_rank Mpi.comm_world;;
let n = Mpi.comm_size Mpi.comm_world;;
let k = 10000

let src = Array.make 1 k;;
let dest = Array.make 1 0;;
Mpi.reduce_int_array src dest Mpi.Int_sum 0 Mpi.comm_world;;
if me = 0 then 
  Printf.printf "using reduce_int_array, expected: %d got: %d\n" 
    (n*k) dest.(0);;

let srcf = Array.make 1 (float k);;
let destf = Array.make 1 0.;;
Mpi.reduce_float_array srcf destf Mpi.Float_sum 0 Mpi.comm_world;;
if me = 0 then 
  Printf.printf "using reduce_float_array, expected: %.1f got: %.1f\n" 
    (float (n*k)) destf.(0);;
