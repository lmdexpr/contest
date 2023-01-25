open Core

let n, p, q = Scanf.scanf "%d %d %d" Tuple3.create

let a = Array.init n ~f:(fun _ -> Scanf.scanf " %d" ident)

let iter =
  let open Iter in
  let (let+) x k = flat_map k x in
  let (let^) x k = filter k x in
  let+ i = 0 -- (n - 1) in
  let+ j = 0 -- (i - 1) in
  let+ k = 0 -- (j - 1) in
  let+ l = 0 -- (k - 1) in
  let^ m = 0 -- (l - 1) in
  a.(i) * a.(j) % p * a.(k) % p * a.(l) % p * a.(m) % p = q

let () = printf "%d\n%!" @@ Iter.length iter
