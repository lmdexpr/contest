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

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v
 
  let around g v = find g v |> Option.value ~default:Iter.empty
end

let n = scanf " %d" Fn.id
let m = scanf " %d" Fn.id

let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ Graph.connect g
  done

let ans = bfs (module Int) ~around:(Graph.around g) ~f:succ (1, 0)
let ans v =
  Hashtbl.find ans v |> Option.value ~default:(-1)

let () =
  for v = 1 to n do
    printf "%d\n" @@ ans v
  done
