open Core
open Scanf

let n, m = scanf " %d %d" Tuple2.create

let a = Array.init m ~f:(fun _ -> scanf " %d" Fn.id) |> Int.Set.of_array

let ans =
  Set.diff
    (Array.init n ~f:succ
      |> Int.Set.of_array
    )
    a

let () = 
  printf "%d\n%!" (Set.length ans);
  Set.iter ans ~f:(printf "%d "); printf "\n%!"
