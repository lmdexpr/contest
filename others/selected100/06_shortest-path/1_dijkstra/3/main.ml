(* https://atcoder.jp/contests/joi2016yo/tasks/joi2016yo_e *)

open Core
open Scanf

let n, m = scanf  "%d %d" Tuple2.create
let k, s = scanf " %d %d" Tuple2.create
let p, q = scanf " %Ld %Ld" Tuple2.create

let is_zombie = Array.init (n + 1) ~f:(const false)
let c = Array.init k
    ~f:(fun _ -> scanf " %d" @@ fun c -> is_zombie.(c) <- true; c)

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u = update g v ~f:(push u)
  let connect g v u = push g v u; push g u v

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ Graph.connect g
  done

module Iter = struct
  include Iter
  let fold ~init ~f iter = fold f init iter
end

let is_danger = Array.init (n+1) ~f:(const false)
let () =
  let enqueue depth q v = Fqueue.enqueue q (depth, v) in
  let rec bfs queue = 
    match Fqueue.dequeue queue with
    | None                         -> ()
    | Some ((d, _), _) when s <= d -> ()
    | Some ((d, v), queue) ->
      Graph.around g v
      |> Iter.filter (fun u -> not is_danger.(u))
      |> Iter.fold ~init:queue ~f:(fun queue u ->
          is_danger.(u) <- true;
          enqueue (d + 1) queue u
        )
      |> bfs
  in
  bfs @@ Array.fold c ~init:Fqueue.empty ~f:(enqueue 0)

module Heap = struct
  include Batteries.Heap
  let singleton v = add v empty
  let find_min_opt heap =
    if size heap = 0 then None
    else
      let result = find_min heap in
      let heap = del_min heap in
      Some (result, heap)
end

let dijkstra start goal =
  let dist = Array.init (n+1) ~f:Int64.(const @@ 1L lsl 60) in
  dist.(start) <- 0L;
  let cost u =
    if u = goal then 0L else if is_danger.(u) then q else p
  in
  let rec dijkstra heap =
    let open Int64 in
    match Heap.find_min_opt heap with
    | None                  -> dist.(goal)
    | Some ((acc, v), heap) ->
      Graph.around g v |> Iter.fold ~init:heap ~f:(fun heap u ->
          if is_zombie.(u) || dist.(u) <= acc + cost u then heap
          else begin
            dist.(u) <- acc + cost u;
            Heap.add (acc + cost u, u) heap
          end
        )
      |> dijkstra
  in
  dijkstra @@ Heap.singleton (0L, start)

let () = dijkstra 1 n |> printf "%Ld\n"
