open Core
open Scanf

let n, k = scanf "%d %Ld" Tuple2.create
let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

let sum =
  Int64.Set.of_array a
  |> Set.filter ~f:Int64.(fun x -> x <= k)
  |> Set.fold ~init:0L ~f:Int64.(+)

let ans = Int64.(k * (k + 1L) / 2L - sum)

let () = printf "%Ld\n%!" ans
