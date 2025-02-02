open Core
open Scanf

let n = scanf " %d" Fn.id
let q = scanf " %d" Fn.id

let where = Array.init (n + 1) ~f:Fn.id
let nest  = Array.init (n + 1) ~f:Int.Set.singleton

let () =
  let ans = ref 0 in
  for _ = 1 to q do
    match scanf " %d" Fn.id with
    | 1 ->
      let p, h = scanf " %d %d" Tuple2.create in
      let prev = where.(p) in
      where.(p)   <- h;
      nest.(prev) <- Set.remove nest.(prev) p;
      nest.(h)    <- Set.add    nest.(h)    p;
      if Set.length nest.(prev) = 1 then
        decr ans;
      if Set.length nest.(h)    = 2 then
        incr ans;
    | _ ->
      printf "%d\n%!" !ans
  done
