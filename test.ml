(***********************************************************************)
(*                                                                     *)
(*                         The Caml/MPI interface                      *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file LICENSE.        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Regression test *)

open Printf
open Mpi

(* comm_size, comm_rank *)

let size = comm_size comm_world
let myrank = comm_rank comm_world

let _ =
  printf "%d: comm_size = %d" myrank size; print_newline()

(* Barrier *)

let _ = barrier comm_world

(* Simple send/receive *)

let _ =
  if myrank = 0 then begin
    printf "%d: sending %s" myrank "aa"; print_newline();
    send "aa" 1 0 comm_world;
    let n = receive any_source any_tag comm_world in
    printf "%d: received %s" myrank n; print_newline()
  end else begin
    let n = receive any_source any_tag comm_world in
    let n' = n ^ "a" in
    printf "%d: received %s, resending %s" myrank n n'; print_newline();
    send n' ((myrank + 1) mod size) 0 comm_world
  end

let _ = barrier comm_world

(* Send and receive with tags *)

let _ =
  if myrank = 0 then begin
    printf "%d: sending %s (tag 0)" myrank "aa"; print_newline();
    send "aa" 1 0 comm_world;
    printf "%d: sending %s (tag 1)" myrank "bb"; print_newline();
    send "bb" 1 1 comm_world;
    let (n, src, tag) = receive_status any_source any_tag comm_world in
    printf "%d: received %s (tag %d) from %d" myrank n tag src;
    print_newline();
    let (n, src, tag) = receive_status any_source any_tag comm_world in
    printf "%d: received %s (tag %d) from %d" myrank n tag src;
    print_newline()
  end else begin
    let (n1, src, tag1) = receive_status any_source 0 comm_world in
    let n1' = n1 ^ "a" in
    printf "%d: received %s (tag %d) from %d, resending %s"
           myrank n1 tag1 src n1'; print_newline();
    let (n2, src, tag2) = receive_status any_source 1 comm_world in
    let n2' = n2 ^ "b" in
    printf "%d: received %s (tag %d) from %d, resending %s"
           myrank n2 tag2 src n2'; print_newline();
    send n2'  ((myrank + 1) mod size) 1 comm_world;
    send n1'  ((myrank + 1) mod size) 0 comm_world
  end

let _ = barrier comm_world

(* Send and receive base types *)

let test_send_recv msg sendfun recvfun transf printfun data =
  if myrank = 0 then begin
    for i = 1 to size - 1 do
      printf "0: %s sending %a to %d" msg printfun data.(i-1) i; print_newline();
      sendfun data.(i-1) i 0 comm_world
    done;
    for i = 1 to size - 1 do
      let x = recvfun i 0 comm_world in
      printf "0: %s received %a" msg printfun x; print_newline()
    done
  end else begin
    let x = recvfun 0 0 comm_world in
    let y = transf x in
    printf "%d: %s received %a,\n   %s  sending %a"
      myrank msg printfun x (String.map (fun _ -> ' ') msg) printfun y;
    print_newline();
    sendfun y 0 0 comm_world
  end

let output_int o i = output_string o (string_of_int i)
let output_float o f = output_string o (string_of_float f)
let output_complex o c =
  let open Complex in
  if c.re <> 0. then output_string o (string_of_float c.re);
  if c.re <> 0. && c.im > 0. then output_string o "+";
  if c.im <> 0. then (output_string o (string_of_float c.im);
                      output_string o "i")
let output_int32 o i = output_string o (Int32.to_string i)
let output_int64 o i = output_string o (Int64.to_string i)
let output_nativeint o i = output_string o (Nativeint.to_string i)
let output_array fn o a =
  output_string o "[ ";
  for i = 0 to Array.length a - 1 do
    fn o a.(i); output_char o ' '
  done;
  output_string o "]"
let output_int_array = output_array output_int
let output_float_array = output_array output_float
let loop_bounds (type t) (l : t Bigarray.layout) n =
  match l with
    Bigarray.C_layout -> 0, n - 1
  | Bigarray.Fortran_layout -> 1, n
let bigarray1_bounds (type t) (a : ('a, 'b, t) Bigarray.Array1.t) =
  loop_bounds (Bigarray.Array1.layout a) (Bigarray.Array1.dim a)
let output_bigarray0 fn o a =
  output_string o "[ ";
  fn o (Bigarray.Array0.get a);
  output_string o " ]"
let output_bigarray1 fn o a =
  let b, e = bigarray1_bounds a in
  output_string o "[ ";
  for i = b to e do
    fn o a.{i}; output_char o ' '
  done;
  output_string o "]"
let output_bigarray2 fn o a =
  let n1, n2 = Bigarray.Array2.(dim1 a, dim2 a) in
  let bi, ei = loop_bounds (Bigarray.Array2.layout a) n1 in
  let bj, ej = loop_bounds (Bigarray.Array2.layout a) n2 in
  output_string o "[ ";
  for i = bi to ei do
    if i > 0 then output_string o "; ";
    for j = bj to ej do
      fn o a.{i,j}; output_char o ' '
    done
  done;
  output_string o "]"
let output_bigarray2 (type t) fn o (a : ('a, 'b, t) Bigarray.Array2.t) =
  let n1, n2 = Bigarray.Array2.(dim1 a, dim2 a) in
  output_string o "[ ";
  (match Bigarray.Array2.layout a with
   | Bigarray.C_layout ->
       for i = 0 to n1 - 1 do
         if i > 0 then output_string o "; ";
         for j = 0 to n2 - 1 do
           fn o a.{i,j}; output_char o ' '
         done
       done;
   | Bigarray.Fortran_layout ->
       for j = 1 to n2 do
         if j > 1 then output_string o "; ";
         for i = 1 to n1 do
           fn o a.{i,j}; output_char o ' '
         done
       done;
  );
  output_string o "]"
let bigarray0_map f a = Bigarray.Array0.(set a (f (get a))); a
let bigarray1_map f a =
  let r = Bigarray.Array1.(create (kind a) (layout a) (dim a)) in
  let b, e = bigarray1_bounds a in
  for i = b to e do
    r.{i} <- f a.{i}
  done;
  r
let bigarray2_map f a =
  let n1, n2 = Bigarray.Array2.(dim1 a, dim2 a) in
  let r = Bigarray.Array2.(create (kind a) (layout a) n1 n2) in
  let bi, ei = loop_bounds (Bigarray.Array2.layout a) n1 in
  let bj, ej = loop_bounds (Bigarray.Array2.layout a) n2 in
  for i = bi to ei do
    for j = bj to ej do
      r.{i, j} <- f a.{i, j}
    done
  done;
  r
let makebigarray0 k l v =
  let ba = Bigarray.(Array0.create k l) in
  Bigarray.Array0.fill ba v;
  ba
let makebigarray1 k l n v =
  let ba = Bigarray.(Array1.create k l n) in
  Bigarray.Array1.fill ba v;
  ba
let makebigarray2 k l n1 n2 v =
  let ba = Bigarray.(Array2.create k l n1 n2) in
  Bigarray.Array2.fill ba v;
  ba
let tobigarray0 = Bigarray.Array0.of_value
let tobigarray1 = Bigarray.Array1.of_array
let tobigarray2 = Bigarray.Array2.of_array
let tobigarrays0 k l = Array.map (tobigarray0 k l)
let tobigarrays1 k l = Array.map (tobigarray1 k l)
let tobigarrays2 k l = Array.map (tobigarray2 k l)
let cx re im = Complex.({re; im})
let cxr re = Complex.({re; im = 0.0})
let cxi im = Complex.({re = 0.0; im})

let _ =
  test_send_recv "int" send_int receive_int (fun n -> n+1) output_int
    [| 10; 20; 30; 40; 50; 60; 70; 80; 90 |];
  test_send_recv "float" send_float receive_float (fun n -> n *. 2.0) output_float
    [| 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9 |];
  let ia = Array.make 3 0 in
  test_send_recv "int array" send_int_array
               (fun src tag comm -> receive_int_array ia src tag comm; ia)
               (Array.map (fun n -> n+1))
               output_int_array
               [| [|10;11;12|]; [|20;21;22|]; [|30;31;32|]; [|40;41;42|] |];
  let fa = Array.make 2 0.0 in
  test_send_recv "float array" send_float_array
               (fun src tag comm -> receive_float_array fa src tag comm; fa)
               (Array.map (fun n -> n +. 0.01))
               output_float_array
               [| [|1.1; 1.2|]; [|2.1; 2.2|]; [|3.1; 3.2|]; [|4.1; 4.2|] |];
  let ba = makebigarray0 Float64 C_layout 0.0 in
  test_send_recv "bigarray0(Float64)" send_bigarray0
               (fun src tag comm -> receive_bigarray0 ba src tag comm; ba)
               (bigarray0_map (fun n -> n +. 0.01))
               (output_bigarray0 output_float)
               (tobigarrays0 Float64 C_layout
                 [| 1.1; 2.1; 3.1; 4.1 |]);
  let ba = makebigarray1 Float64 C_layout 2 0.0 in
  test_send_recv "bigarray1(Float64)" send_bigarray1
               (fun src tag comm -> receive_bigarray1 ba src tag comm; ba)
               (bigarray1_map (fun n -> n +. 0.01))
               (output_bigarray1 output_float)
               (tobigarrays1 Float64 C_layout
                 [| [|1.1; 1.2|]; [|2.1; 2.2|]; [|3.1; 3.2|]; [|4.1; 4.2|] |]);
  let ba = makebigarray2 Int16_signed C_layout 2 3 0 in
  test_send_recv "bigarray2(Int16)" send_bigarray2
               (fun src tag comm -> receive_bigarray2 ba src tag comm; ba)
               (bigarray2_map (fun n -> n+1))
               (output_bigarray2 output_int)
               (tobigarrays2 Int16_signed C_layout
                 [| [| [|10;11;12|]; [| 13;14;15 |] |];
                    [| [|20;21;22|]; [| 23;24;25 |] |];
                    [| [|30;31;32|]; [| 33;34;35 |] |];
                    [| [|40;41;42|]; [| 43;44;45 |] |]
                 |])

let _ = barrier comm_world

(* Barrier, 2 *)

let _ =
  if myrank > 0 then Unix.sleep myrank;
  printf "%d: hitting barrier" myrank; print_newline();
  barrier comm_world;
  if myrank = 0 then begin printf "Jumped barrier"; print_newline() end

(* Broadcast *)

let test_broadcast msg broadcastfun printfun data =
  if myrank = 0 then begin
    printf "0: %s broadcasting %a" msg printfun data; print_newline()
  end;
  ignore (broadcastfun data 0 comm_world);
  printf "%d: %s received %a" myrank msg printfun data; print_newline()

let _ =
  test_broadcast "generic" broadcast output_string "Hello!";
  test_broadcast "int" broadcast_int output_int 123456;
  test_broadcast "float" broadcast_float output_float 3.141592654;
  let ia = if myrank = 0 then [| 123; 456; 789 |] else Array.make 3 0 in
  test_broadcast "int array"
                 (fun x r c -> broadcast_int_array x r c; x)
                 output_int_array ia;
  let fa = if myrank = 0 then [| 3.14; 2.718 |] else Array.make 2 0.0 in
  test_broadcast "float array"
                 (fun x r c -> broadcast_float_array x r c; x)
                 output_float_array fa;
  let ba = if myrank = 0
           then tobigarray1 Float32 C_layout [| 3.14; 2.718 |]
           else makebigarray1 Float32 C_layout 2 0.0 in
  test_broadcast "bigarray1(Float32)"
                 (fun x r c -> broadcast_bigarray1 x r c; x)
                 (output_bigarray1 output_float) ba

let _ = barrier comm_world

(* Scatter *)

let test_scatter msg scatterfun printfun1 printfun2 data =
  if myrank = 0 then begin
    printf "0: %s scattering %a" msg printfun1 data;
    print_newline()
  end;
  let res = scatterfun data 0 comm_world in
  printf "%d: %s received %a" myrank msg printfun2 res; print_newline();
  barrier comm_world
  
let _ =
  test_scatter "generic" scatter (output_array output_string) output_string
    [| "Six"; "scies"; "scient"; "six"; "cigares" |];
  test_scatter "int" scatter_int output_int_array output_int
    [| 12; 34; 56; 78; 90 |];
  test_scatter "float" scatter_float output_float_array output_float
    [| 1.2; 3.4; 5.6; 7.8; 9.1 |];
  test_scatter "from bigarray1(Complex64)"
    scatter_from_bigarray1 (output_bigarray1 output_complex) output_complex
    (tobigarray1 Complex64 C_layout
                 [| cxr 1.; cxi 1.; cxr (-1.); cxi (-1.); cx 0.5 (-0.5) |]);
  let ia = Array.make 3 0 in
  test_scatter "int array"
               (fun d r c -> scatter_int_array d ia r c; ia)
               output_int_array output_int_array
               [| 10;11;12; 20;21;22; 30;31;32; 40;41;42; 50;51;52 |];
  let fa = Array.make 3 0.0 in
  test_scatter "float array"
               (fun d r c -> scatter_float_array d fa r c; fa)
               output_float_array output_float_array
               [| 1.0;1.1;1.2; 2.0;2.1;2.2; 3.0;3.1;3.2;
                  4.0;4.1;4.2; 5.0;5.1;5.2 |];
  let ba = makebigarray1 Char Fortran_layout 3 '@' in
  test_scatter "bigarray1(Char)"
               (fun d r c -> scatter_bigarray1 d ba r c; ba)
               (output_bigarray1 output_char)
               (output_bigarray1 output_char)
               (tobigarray1 Char Fortran_layout
                [| 'a';'b';'c'; 'd';'e';'f'; 'g';'h';'I';
                   'J';'K';'L'; 'M'; 'N'; 'O' |]);
  let ba = Bigarray.genarray_of_array1
            (makebigarray1 Char C_layout 3 '@') in
  test_scatter "bigarray(2->1)(C:Char)"
               (fun d r c -> scatter_bigarray d ba r c; ba)
               (fun o a -> output_bigarray2 output_char o
                             (Bigarray.array2_of_genarray a))
               (fun o a -> output_bigarray1 output_char o
                             (Bigarray.array1_of_genarray a))
               (Bigarray.genarray_of_array2 (tobigarray2 Char C_layout
                [| [| 'a';'b';'c' |];
                   [| 'd';'e';'f' |];
                   [| 'g';'h';'I' |];
                   [| 'J';'K';'L' |];
                   [| 'M';'N';'O' |] |]));
  let ba = Bigarray.genarray_of_array1
            (makebigarray1 Char Fortran_layout 3 '@') in
  test_scatter "bigarray(2->1)(F:Char)"
               (fun d r c -> scatter_bigarray d ba r c; ba)
               (fun o a -> output_bigarray2 output_char o
                             (Bigarray.array2_of_genarray a))
               (fun o a -> output_bigarray1 output_char o
                             (Bigarray.array1_of_genarray a))
               (Bigarray.genarray_of_array2 (tobigarray2 Char Fortran_layout
                [| [| 'a';'d';'g';'J';'M' |];
                   [| 'b';'e';'h';'K';'N' |];
                   [| 'c';'f';'I';'L';'O' |] |]))

(* Gather *)

let test_gather msg gatherfun printfun1 printfun2 data =
  printf "%d: %s sending %a" myrank msg printfun2 data; print_newline();
  let res = gatherfun data 0 comm_world in
  if myrank = 0 then begin
    printf "0: %s gathered %a" msg printfun1 res;
    print_newline()
  end;
  barrier comm_world
  
let _ =
  test_gather "generic" gather (output_array output_string) output_string
    [| "The"; "quick"; "fox"; "jumps"; "over" |].(myrank);
  let ia = Array.make size 0 in
  test_gather "int"
              (fun d r c -> gather_int d ia r c; ia) 
              output_int_array output_int
              [| 12; 34; 56; 78; 90 |].(myrank);
  let fa = Array.make size 0.0 in
  test_gather "float"
              (fun d r c -> gather_float d fa r c; fa) 
              output_float_array output_float
              [| 1.2; 3.4; 5.6; 7.8; 9.1 |].(myrank);
  let ba = makebigarray1 Int64 C_layout size 0L in
  test_gather "to_bigarray1(Int64)"
              (fun d r c -> gather_to_bigarray1 d ba r c; ba)
              (output_bigarray1 output_int64) output_int64
              [| 12L; 34L; 56L; 78L; 90L |].(myrank);
  let ia = Array.make (3 * size) 0 in
  test_gather "int array"
              (fun d r c -> gather_int_array d ia r c; ia) 
              output_int_array output_int_array
              [| myrank*10; myrank*10 + 1; myrank*10 + 2 |];               
  let fa = Array.make (3 * size) 0.0 in
  test_gather "float array"
              (fun d r c -> gather_float_array d fa r c; fa) 
              output_float_array output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |];
  let ba = makebigarray1 Complex32 Fortran_layout (3 * size) Complex.zero in
  test_gather "bigarray1(Complex32)"
              (fun d r c -> gather_bigarray1 d ba r c; ba)
              (output_bigarray1 output_complex)
              (output_bigarray1 output_complex)
              (tobigarray1 Complex32 Fortran_layout
                [| cx (float myrank) 0.25;
                   cx (float myrank) 0.50;
                   cx (float myrank) 0.75 |])

(* Gather to all *)

let test_allgather msg gatherfun printfun1 printfun2 data =
  printf "%d: %s sending %a" myrank msg printfun2 data; print_newline();
  let res = gatherfun data comm_world in
  printf "%d: %s gathered %a" myrank msg printfun1 res;
  print_newline();
  barrier comm_world
  
let _ =
  test_allgather "generic" allgather (output_array output_string) output_string
    [| "The"; "quick"; "fox"; "jumps"; "over" |].(myrank);
  let ia = Array.make size 0 in
  test_allgather "int"
              (fun d c -> allgather_int d ia c; ia) 
              output_int_array output_int
              [| 12; 34; 56; 78; 90 |].(myrank);
  let fa = Array.make size 0.0 in
  test_allgather "float"
              (fun d c -> allgather_float d fa c; fa) 
              output_float_array output_float
              [| 1.2; 3.4; 5.6; 7.8; 9.1 |].(myrank);
  let ba = makebigarray1 Int C_layout size 0 in
  test_allgather "to bigarray1(Int)"
              (fun d c -> allgather_to_bigarray1 d ba c; ba)
              (output_bigarray1 output_int) output_int
              [| 12; 34; 56; 78; 90 |].(myrank);
  let ia = Array.make (3 * size) 0 in
  test_allgather "int array"
              (fun d c -> allgather_int_array d ia c; ia) 
              output_int_array output_int_array
              [| myrank*10; myrank*10 + 1; myrank*10 + 2 |];               
  let fa = Array.make (3 * size) 0.0 in
  test_allgather "float array"
              (fun d c -> allgather_float_array d fa c; fa) 
              output_float_array output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |];
  let ba = makebigarray1 Nativeint C_layout (3 * size) 0n in
  test_allgather "bigarray1(Nativeint)"
              (fun d c -> allgather_bigarray1 d ba c; ba)
              (output_bigarray1 output_nativeint)
              (output_bigarray1 output_nativeint)
              (tobigarray1 Nativeint C_layout
                Nativeint.([| of_int (myrank*10);
                              of_int (myrank*10 + 1);
                              of_int (myrank*10 + 2) |]))

(* Alltoall *)

let test_alltoall msg alltoallfun printfun data =
  printf "%d: %s alltoall -  sending %a" myrank msg printfun data;
  print_newline();
  let res = alltoallfun data comm_world in
  printf "%d: %s alltoall - received %a" myrank msg printfun res;
  print_newline();
  barrier comm_world

let _ =
  test_alltoall "generic" alltoall (output_array output_string)
    ([|
      [| "Un";      "chèque";  "kitch";  "est";      "chique" |];
      [| "Six";     "scies";   "scient"; "six";      "cigares" |];
      [| "Elle";    "chausse"; "ses";    "souliers"; "secs" |];
      [| "Je";      "bois";    "aux";    "trois";    "oies" |];
      [| "L'œuvre"; "pieuse";  "d'une";  "pieuvre";  "heureuse" |];
    |]).(myrank);
  test_alltoall "int"
    (fun d c -> alltoall_int_array d d c; d)
    output_int_array
    ([|
      [|  0;  1;  2;  3;  4;  5;  6;  7;  8;  9 |];
      [| 10; 11; 12; 13; 14; 15; 16; 17; 18; 19 |];
      [| 20; 21; 22; 23; 24; 25; 26; 27; 28; 29 |];
      [| 30; 31; 32; 33; 34; 35; 36; 37; 38; 39 |];
      [| 40; 41; 42; 43; 44; 45; 46; 47; 48; 49 |];
    |]).(myrank);
  let a = Array.make 5 0.0 in
  test_alltoall "float"
    (fun d c -> alltoall_float_array d a c; a)
    output_float_array
    ([|
      [|  1.2;  3.4;  5.6;  7.8;  9.1 |];
      [| 11.2; 13.4; 15.6; 17.8; 19.1 |];
      [| 21.2; 23.4; 25.6; 27.8; 29.1 |];
      [| 31.2; 33.4; 35.6; 37.8; 39.1 |];
      [| 41.2; 43.4; 45.6; 47.8; 49.1 |];
    |]).(myrank);
  let ba = makebigarray1 Char Fortran_layout 5 '@' in
  test_alltoall "bigarray1(Char)"
               (fun d c -> alltoall_bigarray1 d ba c; ba)
               (output_bigarray1 output_char)
               (tobigarray1 Char Fortran_layout
                 ([|
                   [| 'S'; 'A'; 'T'; 'O'; 'R' |];
                   [| 'a'; 'r'; 'e'; 'p'; 'o' |];
                   [| 'T'; 'E'; 'N'; 'E'; 'T' |];
                   [| 'o'; 'p'; 'e'; 'r'; 'a' |];
                   [| 'R'; 'O'; 'T'; 'A'; 'S' |];
                 |]).(myrank));
  let ba = makebigarray2 Int16_unsigned C_layout 5 2 0 in
  test_alltoall "bigarray2(Int16_unsigned)"
               (fun d c -> alltoall_bigarray2 d ba c; ba)
               (output_bigarray2 output_int)
               (tobigarray2 Int16_unsigned C_layout ([|
     [| [| 10; 11 |]; [| 12; 13 |]; [| 14; 15 |]; [| 16; 17 |]; [| 18; 19 |] |];
     [| [| 20; 21 |]; [| 22; 23 |]; [| 24; 25 |]; [| 26; 27 |]; [| 28; 29 |] |];
     [| [| 30; 31 |]; [| 32; 33 |]; [| 34; 35 |]; [| 36; 37 |]; [| 38; 39 |] |];
     [| [| 40; 41 |]; [| 42; 43 |]; [| 44; 45 |]; [| 46; 47 |]; [| 48; 49 |] |];
     [| [| 50; 51 |]; [| 52; 53 |]; [| 54; 55 |]; [| 56; 57 |]; [| 58; 59 |] |];
                 |]).(myrank))

(* Reduce *)

let name_of_reduce_op (type t) (x : t op) =
  match x with
    Max -> "Max"
  | Min -> "Min"
  | Sum -> "Sum"
  | Prod -> "Prod"
  | Land -> "Land"
  | Lor -> "Int_lor"
  | Xor -> "Int_xor"
  | Int_max -> "Int_max"
  | Int_min -> "Int_min"
  | Int_sum -> "Int_sum"
  | Int_prod -> "Int_prod"
  | Int_land -> "Int_land"
  | Int_lor -> "Int_lor"
  | Int_xor -> "Int_xor"
  | Float_max -> "Float_max"
  | Float_min -> "Float_min"
  | Float_sum -> "Float_sum"
  | Float_prod -> "Float_prod"

let test_reduce msg reducefun reduceops printfun printop data =
  printf "%d: %s my data is %a" myrank msg printfun data; print_newline();
  List.iter
    (fun op ->
      let res = reducefun data op 0 comm_world in
      if myrank = 0 then begin
        printf "0: %s result of reduction %s is %a"
               msg (printop op) printfun res;
        print_newline()
      end)
    reduceops;
  barrier comm_world

let _ =
  test_reduce "int"
              reduce_int
              [Max; Min; Sum; Prod; Land; Lor; Xor]
              output_int name_of_reduce_op
              (myrank + 1);
  test_reduce "float"
              reduce_float
              [Max; Min; Sum; Prod]
              output_float name_of_reduce_op
              (float myrank +. 1.0);
  let ia = Array.make 3 0 in
  test_reduce "int array"
              (fun d op r c -> reduce_int_array d ia op r c; ia)
              [Max; Min; Sum; Prod; Land; Lor; Xor]
              output_int_array name_of_reduce_op
              [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |];
  let fa = Array.make 3 0.0 in
  test_reduce "float array"
              (fun d op r c -> reduce_float_array d fa op r c; fa)
              [Max; Min; Sum; Prod]
              output_float_array name_of_reduce_op
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |];
  let ba = makebigarray1 Int8_unsigned C_layout 3 0 in
  (* note: result of Prod is [0 225 0] due to 8-bit precision *)
  test_reduce "bigarray1(Int8_unsigned)"
              (fun d op r c -> reduce_bigarray1 d ba op r c; ba)
              [Max; Min; Sum; Prod; Land; Lor; Xor]
              (output_bigarray1 output_int) name_of_reduce_op
              (tobigarray1 Int8_unsigned C_layout
                [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |]);
  let ba = makebigarray2 Int16_unsigned C_layout 2 3 0 in
  test_reduce "bigarray2(Int16_unsigned)"
              (fun d op r c -> reduce_bigarray2 d ba op r c; ba)
              [Max; Min; Sum; Prod; Land; Lor; Xor]
              (output_bigarray2 output_int) name_of_reduce_op
              (tobigarray2 Int16_unsigned C_layout
                [| [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |];
                   [| myrank * 20; myrank * 20 + 1; myrank * 20 + 2 |] |])

(* Reduce all *)

let test_reduceall msg reducefun reduceop printfun data =
  printf "%d: %s my data is %a" myrank msg printfun data; print_newline();
  let res = reducefun data reduceop comm_world in
  barrier comm_world;
  printf "%d: %s result of reduction is %a" myrank msg printfun res;
  print_newline();
  barrier comm_world

let _ =
  test_reduceall "int"
              allreduce_int Sum
              output_int
              (myrank + 1);
  test_reduceall "float"
              allreduce_float Prod
              output_float
              (float myrank +. 1.0);
  let ia = Array.make 3 0 in
  test_reduceall "int array"
              (fun d op c -> allreduce_int_array d ia op c; ia)
              Sum
              output_int_array
              [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |];
  let fa = Array.make 3 0.0 in
  test_reduceall "float array"
              (fun d op c -> allreduce_float_array d fa op c; fa)
              Sum
              output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |];
  let ba = makebigarray1 Complex32 C_layout 3 Complex.zero in
  test_reduceall "bigarray1(Complex32)"
              (fun d op c -> allreduce_bigarray1 d ba op c; ba)
              Sum
              (output_bigarray1 output_complex)
              (tobigarray1 Complex32 C_layout
                [| cx (float myrank +. 0.25) (float myrank +. 0.25);
                   cx (float myrank +. 0.50) (float myrank +. 0.50);
                   cx (float myrank +. 0.75) (float myrank +. 0.75) |])


(* Scan *)

let test_scan msg scanfun reduceop printfun data =
  printf "%d: %s my data is %a" myrank msg printfun data; print_newline();
  let res = scanfun data reduceop comm_world in
  barrier comm_world;
  printf "%d: %s result of scanning is %a" myrank msg printfun res;
  print_newline();
  barrier comm_world

let _ =
  test_scan "int"
              scan_int
              Sum
              output_int
              (myrank + 1);
  test_scan "float"
              scan_float
              Sum
              output_float
              (float myrank +. 1.0);
  let ia = Array.make 3 0 in
  test_scan "int array"
              (fun d op c -> scan_int_array d ia op c; ia)
              Sum
              output_int_array
              [| myrank * 10; myrank * 10 + 1; myrank * 10 + 2 |];
  let fa = Array.make 3 0.0 in
  test_scan "float array"
              (fun d op c -> scan_float_array d fa op c; fa)
              Sum
              output_float_array
              [| float myrank; float myrank +. 0.1; float myrank +. 0.2 |];
  let ba = makebigarray1 Int32 C_layout 3 0l in
  let r = Int32.of_int myrank in
  test_scan "bigarray1(Int32)"
              (fun d op c -> scan_bigarray1 d ba op c; ba)
              Sum
              (output_bigarray1 output_int32)
              (tobigarray1 Int32 C_layout Int32.(
                [| mul r 10l; add (mul r 10l) 1l; add (mul r 10l) 2l |]))

(* Comm split *)

let send_in_comm c init incr =
  let rank_in_c = comm_rank c
  and size_of_c = comm_size c in
  if rank_in_c = 0 then begin
    printf "%d[%d]: sending %s" rank_in_c myrank init; print_newline();
    send init 1 0 c;
    let n = receive any_source any_tag c in
    printf "%d[%d]: received %s" rank_in_c myrank n; print_newline()
  end else begin
    let n = receive any_source any_tag c in
    let n' = n ^ incr in
    printf "%d[%d]: received %s, resending %s" rank_in_c myrank n n';
    print_newline();
    send n' ((rank_in_c + 1) mod size_of_c) 0 c
  end

let _ =
  let c = comm_split comm_world (myrank mod 2) 0 in
  if myrank mod 2 = 0
  then send_in_comm c "aa" "a"
  else send_in_comm c "bb" "b";
  barrier comm_world

(* Cartesian topology *)

let cart = cart_create comm_world [|2;2|] [|false;false|] true

let test_dims_create n hints =
  printf "dims_create %d %a = %a" n output_int_array hints
                                    output_int_array (dims_create n hints);
  print_newline()

let _ =
  if myrank = 0 then begin
    for x = 0 to 1 do for y = 0 to 1 do
      printf "(%d, %d) -> rank %d" x y (cart_rank cart [|x;y|]);
      print_newline()
    done done;
    for r = 0 to comm_size cart - 1 do
      let c = cart_coords cart r in
      printf "rank %d -> (%d, %d)" r c.(0) c.(1);
      print_newline()
    done;
    test_dims_create 60 [|0;0;0|];
    test_dims_create 60 [|3;0;0|];
    test_dims_create 60 [|0;4;0|];
    test_dims_create 60 [|3;0;5|]
  end;
  barrier comm_world

(* Wtime *)

let _ =
  printf "%d: my wtime is %.3f" myrank (wtime()); print_newline()
