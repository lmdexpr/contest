open Core
open Scanf

let n = scanf "%d" Fn.id
let a = Array.init n ~f:(fun _ -> scanf " %d" Fn.id)

let b = Array.create ~len:200 0
let () =
  for i = 0 to n - 1 do
    b.(a.(i) % 200) <- b.(a.(i) % 200) + 1
  done

let ans = Array.fold b ~init:0 ~f:(fun acc x -> acc + x * (x - 1) / 2)

let () = printf "%d\n%!" ans
