open Core
open Scanf

let n1, n2, m = scanf "%d %d %d" Tuple3.create
let n = n1 + n2

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

let max_path init =
  let visited = Array.init (n + 1) ~f:(const false) in visited.(init) <- true;
  let rec bfs ?(max=0) q = 
    match Fqueue.dequeue q with
    | None             -> max
    | Some ((v, c), q) ->
      let max = Int.max c max in
      visited.(v) <- true;
      Graph.around g v
      |> Iter.filter (fun u -> not visited.(u))
      |> Fn.flip Iter.fold q (fun q u ->
          visited.(u) <- true;
          Fqueue.enqueue q (u, c + 1)
        )
      |> bfs ~max
  in
  bfs @@ Fqueue.singleton (init, 0)

let ans = max_path 1 + max_path n + 1

let () = printf "%d\n%!" ans
