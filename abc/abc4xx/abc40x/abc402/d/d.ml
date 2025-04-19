open Core
open Scanf

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let count = Array.create ~len:n 0
let () =
  for _ = 0 to m - 1 do
    scanf " %d %d" @@ fun a b ->
    let idx = (a + b) % n in
    count.(idx) <- count.(idx) + 1
  done

let m = Int64.of_int m
let ans = Int64.(m * (m - 1L) / 2L)
let ans =
  Array.fold count ~init:ans ~f:Int64.(fun acc x ->
    let x = Int64.of_int x in
    acc - x * (x - 1L) / 2L
  )

let () = printf "%Ld\n%!" ans
