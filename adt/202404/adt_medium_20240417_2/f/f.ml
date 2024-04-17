open Core
open Scanf

let n, m = scanf "%d %d" Tuple2.create

module Graph = struct
  include Hashtbl
  let push v = function
    | None     -> Iter.singleton v
    | Some acc -> Iter.cons v acc
  let push g v u w = update g v ~f:(push (u, w))
  let connect g v u w = push g v u w; push g u v w

  let around g v = find g v |> Option.value ~default:Iter.empty
end
let g = Graph.create ~size:n (module Int)
let () =
  for _ = 1 to m do
    scanf " %d %d %d" @@ Graph.connect g
  done

let dfs g start =
  let visited = Array.create ~len:(n+1) false in
  let rec dfs acc v =
    if visited.(v) then acc
    else begin
      visited.(v) <- true;
      let acc =
        Graph.around g v
        |> Iter.filter (fun (u, _) -> not visited.(u))
        |> Iter.map    (fun (u, w) -> dfs (acc + w) u)
        |> Iter.max ~lt:(<)
        |> Option.value ~default:acc
      in
      visited.(v) <- false;
      acc
    end
  in
  dfs 0 start

let ans = 
  Iter.(1 -- n)
  |> Iter.map (dfs g)
  |> Iter.max_exn ~lt:(<)

let () = printf "%d\n%!" ans
