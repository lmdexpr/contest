open Core
open Scanf

let k, s = scanf "%d %d" Tuple2.create

let (let+) x k = Iter.flat_map k x
and (let^) x k = Iter.filter k x

let ans =
  let+ x = Iter.(0 -- k) in
  let^ y = Iter.(0 -- k) in
  let z = s - x - y in
  0 <= z && z <= k

let ans = Iter.length ans

let () = printf "%d\n%!" ans
