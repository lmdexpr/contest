open Core
open Scanf

let n = scanf "%d" ident
let cards = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

let dp = Array.make_matrix ~dimx:n ~dimy:2 0L

let modulo = 998244353L
let (+%) a b = Int64.((a + b) % modulo)

let () =
  dp.(0).(0) <- 1L;
  dp.(0).(1) <- 1L;
  for i = 1 to n - 1 do
    let pa, pb = cards.(i - 1) and a, b = cards.(i) in
    let solve x =
      (if x = pa then 0L else dp.(i-1).(0)) +%
      (if x = pb then 0L else dp.(i-1).(1))
    in
    dp.(i).(0) <- solve a;
    dp.(i).(1) <- solve b
  done

let ans = dp.(n - 1).(0) +% dp.(n - 1).(1)

let () = printf "%Ld\n%!" ans
