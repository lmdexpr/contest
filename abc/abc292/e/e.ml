open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g u v = update g u ~f:(push v)
 
  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d" @@ Graph.push g
  done

let rec bfs visited q acc =
  match Queue.dequeue q with
  | None   -> acc
  | Some v ->
    Graph.around g v
    |> Iter.filter (fun u -> not visited.(u))
    |> Iter.map (fun u ->
        visited.(u) <- true;
        Queue.enqueue q u 
      )
    |> Iter.length
    |> (+) acc
    |> bfs visited q
let bfs acc v =
  let visited = Array.init (n+1) ~f:(const false) in
  visited.(v) <- true;
  bfs visited (Queue.singleton v) acc

let ans = Iter.(1 -- n) |> Iter.fold bfs 0
let ans = ans - m

let () = printf "%d\n%!" ans
