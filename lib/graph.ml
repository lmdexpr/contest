open Core

let n = 100

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

let _bfs init =
  let visited = Array.init (n+1) ~f:(const false) in visited.(init) <- true;
  let q = Queue.singleton init in
  let rec bfs () =
    match Queue.dequeue q with
    | None   -> ()
    | Some v ->
      Graph.around g v
      |> Iter.filter (fun u -> not visited.(u))
      |> Iter.iter (fun u -> visited.(u) <- true; Queue.enqueue q u);
      bfs ()
  in
  bfs ()

let _dfs init =
  let visited = Array.init (n+1) ~f:(const false) in visited.(init) <- true;
  let s = Stack.singleton init in
  let rec dfs () =
    match Stack.pop s with
    | None   -> ()
    | Some v ->
      Graph.around g v
      |> Iter.filter (fun u -> not visited.(u))
      |> Iter.iter (fun u -> visited.(u) <- true; Stack.push s u);
      dfs ()
  in
  dfs ()

module WeightedGraph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u c = update g v ~f:(push (u, c))
  let connect g v u c = push g v u c; push g u v c

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = WeightedGraph.create ~size:n (module Int)

let inf = 1000_000_000
let dijkstra init = 
  let dist = Array.init (n+1) ~f:(const inf) in dist.(init) <- 0;
  let q = Queue.singleton init in
  let rec dijkstra () =
    match Queue.dequeue q with
    | None   -> dist
    | Some v ->
      WeightedGraph.around g v
      |> Iter.iter (fun (u, cost) ->
          if dist.(u) > dist.(v) + cost then begin
            dist.(u) <- dist.(v) + cost;
            Queue.enqueue q u
          end
        );
      dijkstra ()
  in
  dijkstra ()

let _dist_from_1 = dijkstra 1
let _dist_from_n = dijkstra n
