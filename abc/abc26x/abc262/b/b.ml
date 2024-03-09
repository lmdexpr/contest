open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

let e = Array.init (n+1) ~f:(fun _ -> Array.init (n+1) ~f:(fun _ -> false))
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ fun u v -> e.(u).(v) <- true; e.(v).(u) <- true
  done

let (let+) = Fn.flip Iter.flat_map
let (let^) = Fn.flip Iter.filter

let ans =
  let+ a = Iter.(1 -- n) in
  let+ b = Iter.((a+1) -- n) in
  let^ c = Iter.((b+1) -- n) in
  e.(a).(b) && e.(b).(c) && e.(c).(a)

let ans = Iter.length ans

let () = printf "%d\n%!" ans
