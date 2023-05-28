(* https://atcoder.jp/contests/joi2014yo/tasks/joi2014yo_e *)
(* MLE しているが、制約が厳しいパターンなのでいつか再チャレンジする…… *)
open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

let taxi = Array.init n ~f:(fun _ -> scanf " %d %d" Tuple2.create)

module Graph = struct
  let create ~size = Array.create ~len:(size + 1) None

  let push v = function
    | None     -> Some (Iter.singleton v)
    | Some acc -> Some (Iter.cons v acc)
  let push g v u = g.(v) <- push u g.(v)
  let connect g v u = push g v u; push g u v

  let around g v = Option.value g.(v) ~default:Iter.empty
end
let g = Graph.create ~size:n
let () =
  for _ = 1 to k do
    scanf " %d %d" @@ Graph.connect g
  done

let g =
  let taxi_routes = Graph.create ~size:n in
  for start = 1 to n do
    let visited = Array.create ~len:(n+1) false in
    let c, r = taxi.(start - 1) in
    let enqueue depth q v = Fqueue.enqueue q (depth, v) in
    let rec bfs queue = 
      match Fqueue.dequeue queue with
      | None                                  -> ()
      | Some ((d, _), _)     when d > r       -> ()
      | Some ((_, v), queue) when visited.(v) -> bfs queue
      | Some ((d, v), queue) ->
        visited.(v) <- true;
        Graph.push taxi_routes start (c, v);
        Graph.around g v
        |> Iter.filter (fun u -> not visited.(u))
        |> Iter.fold (enqueue (d + 1)) queue
        |> bfs
    in
    bfs @@ Fqueue.singleton (0, start)
  done;
  taxi_routes

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
  let dist = Array.init (n+1) ~f:(const @@ 1 lsl 30) in
  dist.(start) <- 0;
  let rec dijkstra heap =
    match Heap.find_min_opt heap with
    | None                  -> dist.(goal)
    | Some ((acc, v), heap) ->
      Graph.around g v
      |> Iter.fold (fun heap (c, u) ->
          if dist.(u) <= acc + c then heap
          else begin
            dist.(u) <- acc + c;
            Heap.add (acc + c, u) heap
          end
        )
        heap
      |> dijkstra
  in
  dijkstra @@ Heap.singleton (0, start)

let () = dijkstra 1 n |> printf "%d\n"
