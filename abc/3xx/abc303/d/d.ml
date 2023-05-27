open Core
open Scanf

let x, y, z = scanf "%Ld %Ld %Ld" Tuple3.create
let s = scanf " %s" ident

let n = String.length s

let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:2 0L
let () =
  dp.(0).(1) <- Int64.(1L lsl 60);
  for i = 1 to n do
    let s = s.[i - 1] in
    let next = dp.(i) and pd = dp.(i - 1) in
    let open Int64 in
    let z = z + min x y in
    match s with
    | 'a' ->
      next.(0) <- min (pd.(0) + x) (pd.(1) + z);
      next.(1) <- min (pd.(1) + y) (pd.(0) + z)
    | _ ->
      next.(0) <- min (pd.(0) + y) (pd.(1) + z);
      next.(1) <- min (pd.(1) + x) (pd.(0) + z)
  done

let ans = Int64.min dp.(n).(0) dp.(n).(1)

let () = printf "%Ld\n%!" ans
