open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create
module Graph = struct
  include Hashtbl
  let push v c = function
    | None     -> Iter.singleton (v, c)
    | Some acc -> Iter.cons (v, c) acc
  let push g v u c = update g v ~f:(push u c)
  let connect g v u c = push g v u c; push g u v c

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d %d" @@ Graph.connect g
  done

let dfs init =
  let visited = Array.create ~len:(n + 1) false in
  let rec dfs acc v =
    if visited.(v) then acc
    else begin
      visited.(v) <- true;
      let acc =
        Graph.around g v
        |> Iter.filter (fun (u, _) -> not visited.(u))
        |> Iter.map (fun (u, c) -> dfs (acc + c) u)
        |> Iter.fold max acc
      in
      visited.(v) <- false;
      acc
    end
  in
  dfs 0 init

let ans =
  Iter.(1 -- n)
  |> Iter.map dfs
  |> Iter.fold max 0

let () = printf "%d\n%!" ans
