open Core
open Scanf

let n = scanf "%Ld" Fn.id

let to_binary n =
  let open Int64 in
  let rec aux n acc =
    if n = 0L then acc
    else aux (shift_right_logical n 1) (n % 2L :: acc)
  in
  aux n []

let b = to_binary n
let n = List.length b
let () = 
  List.iteri b ~f:(fun i x ->
    if Int64.(x = 1L) then printf "A";
    if i <> n - 1 then printf "B";
  );
  printf "\n"
