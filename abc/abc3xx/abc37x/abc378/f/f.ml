open Core
open Scanf

let n = scanf "%d" Fn.id

let e = Array.init (n-1) ~f:(fun _ -> scanf " %d %d" Tuple2.create)
let deg = Array.create ~len:(n + 1) 0
let () =
  Array.iter e ~f:(fun (u, v) ->
    deg.(u) <- deg.(u) + 1; deg.(v) <- deg.(v) + 1;
  )

let dsu = Array.init (n + 1) ~f:Union_find.create

let c = Array.create ~len:(n + 1) 0
let () =
  Array.iter e ~f:(fun (u, v) ->
    match deg.(u), deg.(v) with
    | 3, 3 -> Union_find.union dsu.(u) dsu.(v)
    | 3, 2 -> c.(u) <- c.(u) + 1
    | 2, 3 -> c.(v) <- c.(v) + 1
    | _    -> ()
  )

let ans = Array.create ~len:(n + 1) 0
let () =
  Array.iteri dsu ~f:(fun i v ->
    let root = Union_find.get v in
    ans.(root) <- ans.(root) + c.(i)
  )

let ans = Array.fold ans ~init:0 ~f:(fun acc x -> acc + x * (x - 1) / 2)

let () = printf "%d\n%!" ans
