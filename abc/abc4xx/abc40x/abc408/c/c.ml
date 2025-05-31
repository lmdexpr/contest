open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let walls = Array.create ~len:(n+1) 0
let () =
  for _ = 1 to m do
    let l = scanf " %d" Fn.id in
    let r = scanf " %d" Fn.id in
    walls.(l - 1) <- walls.(l - 1) + 1;
    walls.(r)     <- walls.(r) - 1;
  done;
  for i = 1 to n - 1 do
    walls.(i) <- walls.(i) + walls.(i - 1)
  done;
  walls.(n) <- Int.max_value

let ans = Array.min_elt walls ~compare |> Option.value_exn

let () = printf "%d\n%!" ans
