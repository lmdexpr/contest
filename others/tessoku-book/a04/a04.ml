open Core
open Scanf

let n = scanf "%d" Fn.id

let to_binary n =
  let rec to_binary n acc =
    if n = 0 then acc
    else to_binary (n / 2) (n mod 2 :: acc)
  in
  to_binary n []

let ans = to_binary n

let n = List.length ans

let () =
  for _ = 1 to 10 - n do
    printf "0"
  done;
  List.iter ans ~f:(fun x -> printf "%d" x);
  printf "\n%!"
