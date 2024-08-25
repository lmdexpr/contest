open Core
open Scanf

let n, m = scanf  "%d %d" Tuple2.create

let a = Array.init n ~f:(fun _ -> scanf " %Ld" Fn.id)

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u w = update g v ~f:(push (u, w))

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    let u, v, b = scanf " %d %d %Ld" Tuple3.create in
    Graph.push g u v Int64.(b + a.(Int.pred v));
    Graph.push g v u Int64.(b + a.(Int.pred u));
  done

module FibonacciHeap = struct
  module Priority = Int64
  module Value    = Int
  module X = struct
    type t = Priority.t * Value.t
    let compare (p1, _) (p2, _) = Priority.compare p1 p2
  end

  module FibonacciTree = struct
    type t = {
      key      : X.t;
      children : X.t Doubly_linked.t;
      order    : int;
    }

    let insert_las tree child =
      { tree with 
        children = Doubly_linked.insert_last tree.children child;
        order    = tree.order + 1
      }
  end

  type t = {
    tree  : X.t Doubly_linked.t;
    least : X.t option;
    size  : int;
  }

  let empty = {
    tree = Doubly_linked.create (); 
    least = None; 
    size = 0 
  }


end

let dijkstra start =
  let inf  = Int64.(1L lsl 60) in
  let dist = Array.init (n+1) ~f:(const inf) in dist.(start) <- 0L;
  let rec dijkstra heap =
    match Heap.pop_min heap with
    | None                -> dist
    | Some ((c, v), heap) ->
      Graph.around g v |> Iter.fold (fun heap (u, w) ->
        if Int64.(dist.(u) <= c + w) then heap 
        else Int64.(
          dist.(u) <- c + w; Heap.insert (c + w, u) heap
        )
      ) heap |> dijkstra
  in
  Heap.singleton (0L, start) |> dijkstra

let dist = dijkstra 1

let () = 
  let open Int64 in
  for i = 2 to n do
    printf "%Ld " @@ dist.(i) + a.(0)
  done;
  printf "\n"
