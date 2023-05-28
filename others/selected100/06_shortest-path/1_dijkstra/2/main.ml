(* https://atcoder.jp/contests/joi2008yo/tasks/joi2008yo_f *)
open Core
open Scanf

let n, k = scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v w = function
    | None     -> Iter.singleton (v, w)
    | Some acc -> Iter.cons (v, w) acc
  let push g v u w = update g v ~f:(push u w)
  let connect g v u w = push g v u w; push g u v w

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)

let dijkstra dist init =
  dist.(init) <- 0;
  let rec dijkstra q =
    match Fqueue.dequeue q with
    | None        -> q
    | Some (v, q) ->
      let f q (u , cost) =
        if dist.(u) <= dist.(v) + cost then q
        else
          (dist.(u) <- dist.(v) + cost; Fqueue.enqueue q u)
      in
      Graph.around g v |> Iter.fold f q |> dijkstra
  in
  Fqueue.singleton init |> dijkstra |> ignore

let inf = 1000_000_000

let () =
  for _ = 1 to k do
    match scanf " %d" ident with
    | 0 ->
      let a, b = scanf " %d %d" Tuple2.create in
      let dist = Array.init (n+1) ~f:(const inf) in
      dist.(a) <- 0;
      dijkstra dist a;
      printf "%d\n%!" @@ if dist.(b) = inf then -1 else dist.(b)
    | _ ->
      scanf " %d %d %d" @@ Graph.connect g
  done
