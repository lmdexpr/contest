open Core
open Scanf

let n = scanf "%d" ident
let a = Array.init n ~f:(fun _ -> scanf " %d" ident)

let modulo = 998244353
let (+%) a b = (a + b) % modulo

let (.+%()<-) a i x = a.(i) <- a.(i) +% x

let dp i =
  let dp = Array.init (n + 1) ~f:(fun _ -> Array.make_matrix ~dimx:(i+1) ~dimy:i 0) in
  dp.(0).(0).(0) <- 1;
  for j = 0 to n - 1 do
    let next = dp.(j+1) and dp = dp.(j) in
    let a = a.(j) in
    for k = 0 to i do
      for l = 0 to i - 1 do
        next.(k).+%(l) <- dp.(k).(l);

        if k <> i then next.(k+1).+%((l + a) % i) <- dp.(k).(l)
      done
    done;
  done;
  dp.(n).(i).(0)

let ans = Iter.(1 -- n) |> Iter.map dp |> Iter.fold (+%) 0

let () = printf "%d\n%!" ans
