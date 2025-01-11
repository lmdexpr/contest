open Core
open Scanf

let n, d = scanf "%d %d" Tuple2.create

let w = Array.init n ~f:(fun _ -> scanf " %d %d" @@ fun t l k -> t * (l + k))

let () =
  for k = 1 to d do
    printf "%d\n%!" @@
    Array.fold w ~init:0 ~f:(fun acc w -> max acc @@ w k)
  done
