open Core
open Scanf

let n = scanf "%d" Fn.id

let divs =
  let rec loop i acc =
    if i * i > n then acc
    else if n % i = 0 then loop (i + 1) (i :: n / i :: acc)
    else loop (i + 1) acc
  in
  loop 1 [] |> Int.Set.of_list
  |> Set.filter ~f:(fun i -> i < 10)

let solve i =
  Set.filter divs ~f:(fun j -> i % (n / j) = 0) 
  |> Set.min_elt

let () =
  for i = 0 to n do
    match solve i with
    | Some j -> printf "%d" j
    | None   -> printf "-"
  done;
  printf "\n"
