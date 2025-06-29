open Core
open Scanf

let bfs m ~around ~f (init_k, init_v) =
  let tbl = Hashtbl.create m in
  Hashtbl.set tbl ~key:init_k ~data:init_v;
  let rec bfs q =
    match Fqueue.dequeue q with 
    | None        -> tbl
    | Some (v, q) ->
      let data = f @@ Hashtbl.find_exn tbl v in
      around v
      |> Iter.filter (fun   u -> Hashtbl.find tbl u |> Option.is_none)
      |> Iter.fold   (fun q u ->
        Hashtbl.set tbl ~key:u ~data;
        Fqueue.enqueue q u
      ) q
      |> bfs
  in
  bfs @@ Fqueue.singleton init_k

let a = scanf " %d" Fn.id
let n = scanf " %d" Fn.id

let rotate x =
  let d = sprintf "%d" x |> String.length in
  Int.(10 ** (d - 1) * (x % 10)) + x / 10

let ans =
  bfs (module Int) (1, 0) ~f:succ
    ~around:(fun x ->
      Iter.filter (fun x -> 1 <= x && x <= 1_000_000) @@
      if x < 10 || x % 10 = 0 then Iter.singleton (a * x)
      else
        Iter.doubleton (a * x) (rotate x)
    )

let ans = Hashtbl.find ans n |> Option.value ~default:(-1)

let () = printf "%d\n%!" ans
