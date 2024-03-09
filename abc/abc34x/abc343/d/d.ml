open Core
open Scanf

let n, t = scanf "%d %d" Tuple2.create

let scores = Array.create ~len:n 0L
let count  = Hashtbl.create ~size:n (module Int64)

let ans = ref 1

let () =
  Hashtbl.set count ~key:Int64.zero ~data:n;
  for _ = 1 to t do
    let a, b  = scanf " %d %Ld" (fun a b -> a - 1, b) in
    Hashtbl.update count scores.(a) ~f:(function
      | None              -> 0
      | Some x when x = 1 -> Int.decr ans; x - 1
      | Some x            -> x - 1
    );
    scores.(a) <- Int64.(scores.(a) + b);
    Hashtbl.update count scores.(a) ~f:(function
      | None              -> Int.incr ans; 1
      | Some x when x = 0 -> Int.incr ans; x + 1
      | Some x            -> x + 1
    );
    printf "%d\n" !ans
  done
