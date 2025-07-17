open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let x = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)
let () =
  Array.sort x ~compare:Int64.ascending

let ans =
  List.init (n - 1) ~f:Int64.(fun i -> x.(Int.succ i) - x.(i))
  |> List.sort ~compare:Int64.ascending
  |> Fn.flip List.take (n - m)
  |> List.fold ~init:0L ~f:Int64.(+)

let () = printf "%Ld\n%!" ans
