open Core
open Scanf

let (+%) a b = (a + b) mod 998244353

let n, m, k = scanf "%d %d %d" Tuple3.create

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(k + 1) 0
let () =
  dp.(0).(0) <- 1;
  for i = 1 to n do
    for j = 0 to k - 1 do
      for l = 1 to m do
        if j + l <= k then
          dp.(i).(j + l) <- dp.(i).(j + l) +% dp.(i - 1).(j)
      done
    done
  done

let ans = Array.fold dp.(n) ~init:0 ~f:(+%)

let () = printf "%d\n%!" ans
