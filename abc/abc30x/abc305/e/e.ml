open Core
open Scanf

let n, m, k = scanf "%d %d %d" Tuple3.create
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

let guard = Array.create ~len:(n+1) (-1)

module Heap = struct
  include Batteries.Heap
  let find_min_opt heap =
    if size heap = 0 then None
    else
      let result = find_min heap in
      let heap   = del_min heap in
      Some (result, heap)
end

let add h heap u =
  if guard.(u) >= h then heap
  else begin
    guard.(u) <- h;
    Heap.add (-h, u) heap
  end

let rec dijkstra heap =
  match Heap.find_min_opt heap with
  | None                                     -> ()
  | Some ((h, p), heap) when guard.(p) <> -h -> dijkstra heap
  | Some ((h, p), heap) ->
    let h = -h in
    Graph.around g p
    |> Iter.fold (add @@ h - 1) heap
    |> dijkstra

let () =
  Iter.(1 -- k)
  |> Iter.map  (fun _ -> scanf " %d %d" Tuple2.create)
  |> Iter.fold (fun heap (p, h) -> add h heap p) Heap.empty
  |> dijkstra;
  let ans = Array.filter_mapi guard ~f:(fun i p -> Option.some_if (p >= 0) i) in
  printf "%d\n" @@ Array.length ans;
  Array.iter ans ~f:(printf "%d "); printf "\n"
