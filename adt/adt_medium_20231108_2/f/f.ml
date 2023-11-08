open Core
open Scanf

let n, x, y = scanf "%d %d %d" Tuple3.create
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
  for _ = 1 to n - 1 do
    scanf " %d %d" @@ Graph.connect g
  done

let dfs start goal =
  let visited = Array.create ~len:(n + 1) false in
  let rec dfs = function
    | []                             -> None
    | (_,   v) :: _ when visited.(v) -> None
    | (acc, v) :: _ when v = goal    -> Some Iter.(snoc acc v)
    | (acc, v) :: vs ->
      let acc = Iter.snoc acc v in
      visited.(v) <- true;
      Graph.around g v
      |> Iter.filter (fun u -> not visited.(u))
      |> Iter.map  (fun u -> acc, u)
      |> Iter.fold (Fn.flip List.cons) vs
      |> dfs
  in
  Option.value ~default:Iter.empty @@ dfs [ Iter.empty, start ]

let () =
  dfs x y |> Iter.iter (printf "%d ");
  printf "\n"
