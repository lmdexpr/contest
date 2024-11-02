open Core
open Scanf

let a = Array.create ~len:5 0
let () =
  for _ = 1 to 4 do
    scanf "%d " (fun x -> a.(x) <- a.(x) + 1)
  done

let ans = Array.map a ~f:(fun x -> x / 2) |> Array.fold ~init:0 ~f:(+)

let () = printf "%d\n%!" ans
