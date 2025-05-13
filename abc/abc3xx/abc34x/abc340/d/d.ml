open Core
open Scanf

module Heap = struct
  include Batteries.Heap
  let singleton v = add v empty
  let pop_min heap =
    if size heap = 0 then None
    else
      Some (find_min heap, del_min heap)
end

let n = scanf " %d" Fn.id

module WeightedGraph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u c = update g v ~f:(push (u, c))

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = WeightedGraph.create ~size:n (module Int)

let inf = 1_000_000_000_000_000L
let dijkstra init = 
  let dist = Array.init (n+1) ~f:(const inf) in dist.(init) <- 0L;
  let rec dijkstra h =
    match Heap.pop_min h with
    | None             -> dist
    | Some ((_, v), h) ->
      WeightedGraph.around g v
      |> Fn.flip Iter.fold h Int64.(fun h (u, cost) ->
        if dist.(u) <= dist.(v) + cost then h
        else (
          dist.(u) <- dist.(v) + cost;
          Heap.add (dist.(u), u) h
        ))
      |> dijkstra
  in
  dijkstra Heap.(singleton (dist.(init), init))

let () =
  for i = 1 to n - 1 do
    scanf " %Ld %Ld %d" @@ fun a b x ->
    WeightedGraph.push g i (i + 1) a;
    WeightedGraph.push g i x b;
  done

let ans = (dijkstra 1).(n)

let () = printf "%Ld\n%!" ans
