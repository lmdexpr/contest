open Core
open Scanf

let n = scanf "%Ld" Fn.id

let rec solve acc n =
  let open Int64 in
  if n <= 0L then acc
  else
    if n % 2L = 0L then solve ('B' :: acc) (n / 2L)
    else solve ('A' :: acc) (n - 1L)

let () =
  solve [] n
  |> List.iter ~f:(printf "%c");
  printf "\n%!"
