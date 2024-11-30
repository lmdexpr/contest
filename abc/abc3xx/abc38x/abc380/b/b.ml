open Core
open Scanf

let s = scanf "%s" String.to_list

let rec solve crr acc = function
  | '|' :: xs -> solve 0 (crr :: acc) xs
  | '-' :: xs -> solve (crr + 1) acc xs
  | _         -> List.rev acc

let ans = solve 0 [] List.(tl_exn s)

let () = 
  List.iter ans ~f:(fun x -> printf "%d " x);
  printf "\n%!"
