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

module DirectedGraph = struct
  type t = {
    size : int;
    normal  : (int, int Iter.t) Hashtbl.t;
    reverse : (int, int Iter.t) Hashtbl.t;
  }
  let create ~size = {
    size;
    normal  = Hashtbl.create ~size (module Int);
    reverse = Hashtbl.create ~size (module Int);
  }

  let push v = function
    | None     -> Iter.singleton v 
    | Some acc -> Iter.snoc acc v
  let push g v u = Hashtbl.update g v ~f:(push u)
  let push g v u = push g.normal v u; push g.reverse u v

  let next g v = Hashtbl.find g.normal  v |> Option.value ~default:Iter.empty
  let pred g v = Hashtbl.find g.reverse v |> Option.value ~default:Iter.empty

  let strongly_connected_components g =
    let iterate visit f = 
      Iter.fold 
        (fun acc v -> if visit.(v) then acc else Iter.cons (f v) acc)
        Iter.empty
    in
    let step1 = 
      let visit = Array.init (g.size + 1) ~f:(const false) in
      let rec dfs acc v =
        visit.(v) <- true;
        next g v
        |> Iter.filter (fun u -> not visit.(u))
        |> Iter.fold dfs acc
        |> Iter.cons v
      in
      iterate visit @@ dfs Iter.empty
    in
    let step2 = 
      let visit = Array.init (g.size + 1) ~f:(const false) in
      let rec dfs acc v =
        visit.(v) <- true;
        pred g v
        |> Iter.filter (fun u -> not visit.(u))
        |> Iter.fold dfs (Iter.cons v acc)
      in
      iterate visit @@ dfs Iter.empty
    in
    Iter.(1 -- g.size) |> step1 |> Iter.flatten |> step2
end

let n = 3
let a = [| 1; 2; 3 |]

let g = DirectedGraph.create ~size:n
let () =
  Iter.(1 -- n) |> Iter.iter (fun i -> DirectedGraph.push g i a.(i - 1))

let self_cycle =
  Array.foldi a ~init:0 ~f:(fun i acc a -> acc + Bool.to_int (i + 1 = a))

let ans =
  DirectedGraph.strongly_connected_components g
  |> Iter.map Iter.length
  |> Iter.filter (fun len -> len > 1)
  |> Iter.sum

let ans = self_cycle + ans

let () = printf "%d\n" ans
