open Core
open Scanf

let n = scanf "%d" ident

let rate = Array.init n ~f:(fun i -> scanf " %Ld %Ld" @@ fun a b -> a, b, i)
let () =
  Array.sort rate ~compare:Int64.(fun (la, lb, l) (ra, rb, r) ->
    match compare (ra * (la + lb)) (la * (ra + rb)) with
    | 0 -> Int.compare l r
    | x -> x
  )

let () =
  Array.iter rate ~f:(fun (_, _, i) -> printf "%d " (i + 1));
  printf "\n"
